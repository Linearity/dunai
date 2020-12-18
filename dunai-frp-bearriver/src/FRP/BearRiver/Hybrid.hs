module FRP.BearRiver.Hybrid where

import Data.MonadicStreamFunction
import FRP.BearRiver.Basic
import FRP.BearRiver.Event
import FRP.BearRiver.EventSource

-- * Discrete to continuous-time signal functions

-- ** Wave-form generation

hold :: Monad m => a -> SF m (Event a) a
hold a = feedback a $ arr $ \(e,a') ->
    dup (event a' id e)
  where
    dup x = (x,x)

trackAndHold :: Monad m => a -> SF m (Maybe a) a
trackAndHold a_init = arr (maybe NoEvent Event) >>> hold a_init

-- ** Accumulators

-- | Accumulator parameterized by the accumulation function.
accumBy :: Monad m => (b -> a -> b) -> b -> SF m (Event a) (Event b)
accumBy f b = mapEventS $ accumulateWith (flip f) b

accumHoldBy :: Monad m => (b -> a -> b) -> b -> SF m (Event a) b
accumHoldBy f b = feedback b $ arr $ \(a, b') ->
  let b'' = event b' (f b') a
  in (b'', b'')