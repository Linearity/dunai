{-# LANGUAGE TupleSections #-}
module FRP.BearRiver.Stateful where

import FRP.BearRiver.Basic
import FRP.BearRiver.Event
import FRP.BearRiver.Switch
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.InternalCore

infixr 0 -->, -:>, >--, >=-

-- * Simple, stateful signal processing
sscan :: Monad m => (b -> a -> b) -> b -> SF m a b
sscan f b_init = feedback b_init u
  where u = undefined -- (arr f >>^ dup)

sscanPrim :: Monad m => (c -> a -> Maybe (c, b)) -> c -> b -> SF m a b
sscanPrim f c_init b_init = MSF $ \a -> do
  let o = f c_init a
  case o of
    Nothing       -> return (b_init, sscanPrim f c_init b_init)
    Just (c', b') -> return (b',     sscanPrim f c' b')

-- ** Initialization

-- | Initialization operator (cf. Lustre/Lucid Synchrone).
--
-- The output at time zero is the first argument, and from
-- that point on it behaves like the signal function passed as
-- second argument.
(-->) :: Monad m => b -> SF m a b -> SF m a b
b0 --> sf = sf >>> replaceOnce b0

-- | Output pre-insert operator.
--
-- Insert a sample in the output, and from that point on, behave
-- like the given sf.
(-:>) :: Monad m => b -> SF m a b -> SF m a b
b -:> sf = iPost b sf

-- | Input initialization operator.
--
-- The input at time zero is the first argument, and from
-- that point on it behaves like the signal function passed as
-- second argument.
(>--) :: Monad m => a -> SF m a b -> SF m a b
a0 >-- sf = replaceOnce a0 >>> sf

(>=-) :: Monad m => (a -> a) -> SF m a b -> SF m a b
f >=- sf = MSF $ \a -> do
  (b, sf') <- unMSF sf (f a)
  return (b, sf')

initially :: Monad m => a -> SF m a a
initially = (--> identity)

-- * State keeping combinators

-- ** Loops with guaranteed well-defined feedback
loopPre :: Monad m => c -> SF m (a, c) (b, c) -> SF m a b
loopPre = feedback

-- ** Event handling
replaceOnce :: Monad m => a -> SF m a a
replaceOnce a = dSwitch (arr $ const (a, Event ())) (const $ arr id)

replaceOnceM :: Monad m => m a -> SF m a a
replaceOnceM m = dSwitch (liftTransS (constM ((, Event ()) <$> m))) (const $ arr id)