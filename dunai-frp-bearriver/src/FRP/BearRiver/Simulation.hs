module FRP.BearRiver.Simulation where

import FRP.BearRiver.Basic
import Data.MonadicStreamFunction
import Control.Monad.Trans.MSF as MSF
import Control.Monad.Identity
import Data.Maybe
import Data.MonadicStreamFunction.InternalCore

-- * Execution/simulation

-- ** Reactimation

reactimate :: Monad m => m a -> (Bool -> m (DTime, Maybe a)) -> (Bool -> b -> m Bool) -> SF m a b -> m ()
reactimate senseI sense actuate sf = do
  -- runMaybeT $ MSF.reactimate $ liftMSFTrans (senseSF >>> sfIO) >>> actuateSF
  MSF.reactimateB $ senseSF >>> sfIO >>> actuateSF
  return ()
 where sfIO        = {-morphS (return.runIdentity)-} (runReaderS sf)

       -- Sense
       senseSF     = MSF (const (do a0  <- senseI
                                    return ((0, a0), senseRest a0)))
       senseRest a = constM (sense True) >>> (arr id *** keepLast a)

       keepLast :: Monad m => a -> MSF m (Maybe a) a
       keepLast a = MSF $ \ma -> let a' = fromMaybe a ma in a' `seq` return (a', keepLast a')

       -- Consume/render
       -- actuateSF :: MSF IO b ()
       -- actuateSF    = arr (\x -> (True, x)) >>> liftMSF (lift . uncurry actuate) >>> exitIf
       actuateSF    = arr (\x -> (True, x)) >>> arrM (uncurry actuate)

-- * Debugging / Step by step simulation

-- | Evaluate an SF, and return an output and an initialized SF.
--
--   /WARN/: Do not use this function for standard simulation. This function is
--   intended only for debugging/testing. Apart from being potentially slower
--   and consuming more memory, it also breaks the FRP abstraction by making
--   samples discrete and step based.
evalAtZero :: SF Identity a b -> a -> (b, SF Identity a b)
evalAtZero sf a = runIdentity $ runReaderT (unMSF sf a) 0

-- | Evaluate an initialized SF, and return an output and a continuation.
--
--   /WARN/: Do not use this function for standard simulation. This function is
--   intended only for debugging/testing. Apart from being potentially slower
--   and consuming more memory, it also breaks the FRP abstraction by making
--   samples discrete and step based.
evalAt :: SF Identity a b -> DTime -> a -> (b, SF Identity a b)
evalAt sf dt a = runIdentity $ runReaderT (unMSF sf a) dt

-- | Given a signal function and time delta, it moves the signal function into
--   the future, returning a new uninitialized SF and the initial output.
--
--   While the input sample refers to the present, the time delta refers to the
--   future (or to the time between the current sample and the next sample).
--
--   /WARN/: Do not use this function for standard simulation. This function is
--   intended only for debugging/testing. Apart from being potentially slower
--   and consuming more memory, it also breaks the FRP abstraction by making
--   samples discrete and step based.
--
evalFuture :: SF Identity a b -> a -> DTime -> (b, SF Identity a b)
evalFuture sf = flip (evalAt sf)

embedSF :: Monad m => SF m a b -> [(DTime, a)] -> m [b]
embedSF _  []           = return []
embedSF sf ((dt,a):as)  = do    (b, sf')    <- runReaderT (unMSF sf a) dt
                                bs          <- embedSF sf' as
                                return (b:bs)