{-# LANGUAGE Arrows #-}

module FRP.BearRiver.Integration where

import Control.Monad.Trans.Reader
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.InternalCore
import Data.VectorSpace
import FRP.BearRiver.Basic
import FRP.BearRiver.Stateful

-- * Integration and differentiation

integral :: (Monad m, VectorSpace a s) => SF m a a
integral = integralFrom zeroVector

integralFrom :: (Monad m, VectorSpace a s) => a -> SF m a a
integralFrom a0 = iPre a0 >>> proc a -> do
  dt <- constM ask         -< ()
  accumulateWith (^+^) a0 -< realToFrac dt *^ a

derivative :: (Monad m, VectorSpace a s) => SF m a a
derivative = derivativeFrom zeroVector

derivativeFrom :: (Monad m, VectorSpace a s) => a -> SF m a a
derivativeFrom a0 = proc a -> do
  dt   <- constM ask   -< ()
  aOld <- iPre a0      -< a
  d    <- initially a0 -< (a ^-^ aOld) ^/ realToFrac dt
  returnA -< d

-- NOTE: BUG in this function, it needs two a's but we
-- can only provide one
iterFrom :: Monad m => (a -> a -> DTime -> b -> b) -> b -> SF m a b
iterFrom f b = MSF $ \a -> do
  dt <- ask
  let b' = f a a dt b
  return (b, iterFrom f b')