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
                                BusVoice (..)   ) where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import qualified Data.Bifunctor as BF
import Data.MonadicStreamFunction
import FRP.BearRiver.Basic
import FRP.BearRiver.Event
import FRP.BearRiver.EventSource
import FRP.BearRiver.Switch
import FRP.BearRiver.Time

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

-- * Communication between parallel tasks

-- | Tasks that run in parallel with each other and also communicate with each
-- other via input and output signals connected to a bus.
newtype BusVoice a b c m d = BusVoice { unBusVoice :: Task (c, a) (c, b) m d }
    deriving (Functor, Monad)

instance (MonadFix m, Monoid b, Monoid c) => Applicative (BusVoice a b c m) where
    pure d  = BusVoice (pure d)
    liftA2 f (BusVoice (Task sf1)) (BusVoice (Task sf2))
            = BusVoice (do  t   <- Task sf
                            case t of   Links (c, r)    -> do   d   <- Task r
                                                                return (f c d)
                                        Mitte (d, r)    -> do   c   <- Task r
                                                                return (f c d)
                                        Rechts (c, d)   -> return (f c d))
        where   sf  = proc (c, a) -> do
                        rec c1'             <- iPre mempty  -< c1
                            c2'             <- iPre mempty  -< c2
                            (ebcd1, k1)     <- vain sf1     -< (c <> c2', a)
                            (ebcd2, k2)     <- vain sf2     -< (c <> c1', a)
                            let c1 = either fst (const mempty) ebcd1
                                c2 = either fst (const mempty) ebcd2
                        returnA -< case (ebcd1, ebcd2) of
                                    (Left (c1, b1), Left (c2, b2))  -> Left (c1 <> c2, b1 <> b2)
                                    (Right c, Left _)               -> Right (Links (c, k2))
                                    (Left _, Right d)               -> Right (Mitte (d, k1))
                                    (Right c, Right d)              -> Right (Rechts (c, d))

data Threether a b c = Links a | Mitte b | Rechts c
