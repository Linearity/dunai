{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module FRP.BearRiver.Task (     Task (..),
                                runTask,
                                loopTask,
                                always,
                                moment,
                                momentC,
                                interval,
                                before,
                                wait,
                                sample,
                                onlyUntil,
                                lastOut,
                                mapTask,
                                Voice (..),
                                mix,
                                BusTask,
                                BusT,
                                BusVoice (..),
                                busMixM,
                                busMix,
                                bus,
                                runBus,
                                newBus,
                                noBus   ) where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.Strict
import qualified Data.Bifunctor as BF
import Data.MonadicStreamFunction
import Data.Tuple
import FRP.BearRiver
import FRP.BearRiver.Monad

-- | A temporary phase of a signal function that returns a value when it is
-- complete.
newtype Task a b m c = Task { taskRep :: SF m a (Either b c) }

instance Monad m => Functor (Task a b m) where
    fmap f (Task sf) = Task (sf >>> arr (fmap f))

instance Monad m => Applicative (Task a b m) where
    pure c  = Task (constant (Right c))
    Task sf1 <*> x  = Task (sf1 `link` (\f -> taskRep (fmap f x)))

instance Monad m => Monad (Task a b m) where
    return c        =  Task (constant (Right c))
    Task sf >>= f   =  Task (sf `link` (taskRep . f))

instance MonadTrans (Task a b) where
    lift = slipIn . const . lift

-- * From tasks to signal functions

-- | Produce a signal function from a task by combining it with a final
-- continuation. Based on the task's return value, the continuation produces a
-- signal function to switch to when the task is complete.
runTask :: Monad m => Task a b m c -> (c -> SF m a b) -> SF m a b
runTask (Task sf) = switch (sf >>> arr eitherToEvent)
    where   eitherToEvent x     = case x of
                                    Left b      -> (b, NoEvent)
                                    Right c     -> (undefined, Event c)

-- | A variant of 'runTask' that requires no final continuation. When the task
-- is complete, it begins once again.
loopTask :: Monad m => Task a b m c -> SF m a b
loopTask a = runTask (forever a) (const (loopTask a))

-- * From signal functions to tasks

-- | A task that never terminates, always behaving as the given signal function.
always :: Monad m => SF m a b -> Task a b m c
always sf = Task (sf >>> arr Left)

-- | A task that lasts a single time step, producing one output sample from one
-- input sample based on the given function.
moment :: Monad m => (a -> ClockInfo m b) -> Task a b m ()
moment f = Task (arrM f &&& (now () >>> iPre NoEvent) >>> arr eitherEvent)

-- | A variant of 'moment' that produces the given output sample regardless of
-- the input.
momentC :: Monad m => b -> Task a b m ()
momentC b = moment (const (return b))

-- | A task that behaves as the given signal function for a given duration.
interval :: Monad m => DTime -> SF m a b -> Task a b m ()
interval dt sf = Task (proc a -> do
                            b   <- sf       -< a
                            t   <- time     -< ()
                            returnA -< if t < dt then Left b else Right ())

-- | A task that behaves as the given signal function until the first occurrence
-- in the output of a given event source.
before :: Monad m => SF m a (Event c) -> SF m a b -> Task a b m c
before interrupt sf = Task (sf &&& interrupt >>> arr eitherEvent)

-- | A task that constantly produces a monoidal identity as output for a given
-- duration.
wait :: (Monad m, Monoid b) => DTime -> Task a b m ()
wait dt = interval dt (constant mempty)

eitherEvent :: (b, Event c) -> Either b c
eitherEvent (b, NoEvent) = Left b
eitherEvent (_, Event c) = Right c

-- | A task of no duration that returns the input sample.
sample :: Monad m => Task a b m a
sample = slipIn return

-- | A task of no duration that returns the value returned by the given action,
-- based on the input sample.
slipIn :: Monad m => (a -> ClockInfo m c) -> Task a b m c
slipIn g = Task (arrM (fmap Right . g))

-- * Transforming tasks

-- | Add an extra termination condition to a task. The condition is an event
-- source which takes as input both the task's input signal and its output
-- signal. The resulting task ends either when the original task ends, or upon
-- the first occurrence of the event source's output: whichever is sooner.
onlyUntil :: Monad m => SF m (a, b) (Event c) -> Task a b m c -> Task a b m c
onlyUntil condition = mapTask clip
    where clip sf = branch sf (proc (a, b) -> do
                                    stop <- condition -< (a, b)
                                    returnA -< event (Left b) Right stop)
                              (arr (Right . snd))

-- | Add an extra value to the return value a task: its last output sample
-- before it ended.
lastOut :: MonadFix m => b -> Task a b m c -> Task a b m (c, b)
lastOut b0 = mapTask (record b0)
    where record b0 sf = proc a -> do
                            rec     bR      <-  iPre b0     -< b
                                    ebc     <-  sf          -< a
                                    let b = case ebc of
                                                Left b      -> b
                                                Right _c    -> bR
                            returnA -< fmap (,b) ebc

-- | Transform any task into any other with a mapping between signal functions.
-- The mapping accounts for both normal output and the return value with the
-- sum type 'Either', where values constructed with 'Left' represent output
-- samples and a value constructed with 'Right' represent the return value.
mapTask :: (Monad m1, Monad m2) =>
                (SF m1 a (Either b c) -> SF m2 d (Either e f))
                    -> Task a b m1 c
                    -> Task d e m2 f
mapTask f (Task sf) = Task (f sf)

-- | Feed the output of a signal function representing a task to either of two
-- signal functions: the first if the output is constructed with 'Left', or the
-- second if the output is constructed with 'Right'.
branch :: Monad m => SF m a (Either b c)
                        -> SF m (a, b) d
                        -> SF m (a, c) d
                        -> SF m a d
branch sf sfL sfR = proc a -> do
                        ebc <- sf -< a
                        case ebc of
                            Left b  -> sfL -< (a, b)
                            Right c -> sfR -< (a, c)


-- * Tasks in parallel

-- | Tasks that run in parallel with each other.
newtype Voice a b m c = Voice { unVoice :: Task a b m c }
    deriving (Functor, Monad)

instance (Monad m, Monoid b) => Applicative (Voice a b m) where
    pure c  = Voice (pure c)
    liftA2 f (Voice (Task sf1)) (Voice (Task sf2))
            = Voice     (do    t  <- Task  (proc a -> do
                                               b1   <- vain sf1     -< a
                                               b2   <- vain sf2     -< a
                                               returnA -< race b1 b2)
                               case t of    Links (c, k)    -> do   d  <- Task k
                                                                    return (f c d)
                                            Mitte (d, k)    -> do   c  <- Task k
                                                                    return (f c d)
                                            Rechts (c, d)   -> return (f c d))
        where   race (Left b1, _) (Left b2, _)      = Left (b1 <> b2)
                race (Right c, _) (Left _, k2)      = Right (Links (c, k2))
                race (Left _, k1)  (Right d, _)     = Right (Mitte (d, k1))
                race (Right c, k1) (Right d, k2)    = Right (Rechts (c, d))

data Threether a b c = Links a | Mitte b | Rechts c

-- | Run two tasks in parallel, combining their output signals with a monoidal
-- operator. When the shorter task is complete its output is the monoidal
-- identity until the longer task is complete. Then the mixed task returns both
-- tasks' return values.
mix :: (MonadFix m, Monoid b) => Task a b m c -> Task a b m d -> Task a b m (c,d)
mix m1 m2 = unVoice (liftA2 (,) (Voice m1) (Voice m2))

-- * Communication between parallel tasks

-- | A task that reads from and writes to a bus via monadic actions.
type BusTask c a b m = Task a b (BusT c m)

-- | A monad transformer that adds both a writer layer and then a reader layer.
-- Its purpose is to allow tasks to communicate on a bus via monadic actions.
type BusT c m = ReaderT c (WriterT c m)

-- | Tasks that run in parallel with each other and also communicate with each
-- other via input and output signals connected to a bus.
newtype BusVoice a b c m d = BusVoice { unBusVoice :: Task (a, c) (b, c) m d }
    deriving (Functor, Monad)

instance (MonadFix m, Monoid b, Monoid c) => Applicative (BusVoice a b c m) where
    pure c  = BusVoice (pure c)
    liftA2 f (BusVoice (Task sf1)) (BusVoice (Task sf2))
            = BusVoice (do  t   <- Task sf
                            case t of   Links (c, r)    -> do   d   <- Task r
                                                                return (f c d)
                                        Mitte (d, r)    -> do   c   <- Task r
                                                                return (f c d)
                                        Rechts (c, d)   -> return (f c d))
        where   sf  = proc (a, c) -> do
                        rec c1'             <- iPre mempty  -< c1
                            c2'             <- iPre mempty  -< c2
                            (ebcd1, k1)     <- vain sf1     -< (a, c <> c2')
                            (ebcd2, k2)     <- vain sf2     -< (a, c <> c1')
                            let c1 = either snd (const mempty) ebcd1
                                c2 = either snd (const mempty) ebcd2
                        returnA -< case (ebcd1, ebcd2) of
                                    (Left (b1, c1), Left (b2, c2))  -> Left (b1 <> b2, c1 <> c2)
                                    (Right c, Left _)               -> Right (Links (c, k2))
                                    (Left _, Right d)               -> Right (Mitte (d, k1))
                                    (Right c, Right d)              -> Right (Rechts (c, d))

-- | A variant of 'busMix' that mixes two tasks that communicate on a bus
-- via monadic actions.
busMixM :: (MonadFix m, Monoid c, Monoid b) =>
                Task a b (BusT c m) d
                    -> Task a b (BusT c m) e
                    -> Task a b (BusT c m) (d, e)
busMixM m1 m2 = bus (mapTask busSwap
                        (busMix (mapTask busSwap (runBus m1))
                                (mapTask busSwap (runBus m2))))

-- | A variant of 'mix' that mixes two tasks that communicate on a bus via
-- extra input and output signals.
busMix ::  (MonadFix m, Monoid b, Monoid c) =>
                Task (a, c) (b, c) m d
                    -> Task (a, c) (b, c) m e
                    -> Task (a, c) (b, c) m (d, e)
busMix m1 m2 = let BusVoice m = liftA2 (,) (BusVoice m1) (BusVoice m2) in m

-- | Transform a task that communicates on a bus via extra input and output
-- signals into a task that does so via monadic actions.
bus :: (Monad m, Monoid c) => Task (c, a) (c, b) m d -> Task a b (BusT c m) d
bus = mapTask (readerSF . writerSF . (>>> arr swizzle))
    where   swizzle ewbd = either (second Left) (\d -> (mempty, Right d)) ewbd

-- | Transform a task that communicates on a bus via monadic actions into a
-- task that does so via extra input and output signals.
runBus :: (Monad m, Monoid c) => Task a b (BusT c m) d -> Task (c, a) (c, b) m d
runBus = mapTask ((>>> arr swizzle) . runWriterSF . runReaderSF)
    where   swizzle (w, ebd) = BF.first (w,) ebd

-- | Transform a task that communicates on a bus via monadic actions into a
-- task that does not communicate on a bus.
newBus :: (Monad m, Monoid c) => Task a b (BusT c m) d -> Task a b m d
newBus = mapTask (runWriterSF_ . runReaderSF_ mempty)

-- | Transform any task into one that connect to a bus but does not read or
-- write to it.
noBus :: (Monad m, Monoid c) => Task a b m d -> Task a b (BusT c m) d
noBus = mapTask (liftSF . liftSF)

busSwap :: Monad m =>
            SF m (a1, a2) (Either (b1, b2) c)
                -> SF m (a2, a1) (Either (b2, b1) c)
busSwap sf = arr swap >>> sf >>> arr (BF.first swap)