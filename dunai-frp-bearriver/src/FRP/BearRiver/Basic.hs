module FRP.BearRiver.Basic where

import Control.Arrow
import qualified Control.Category as Category
import Control.Monad.Trans.Reader
import Data.MonadicStreamFunction.InternalCore
import Data.MonadicStreamFunction.Instances.ArrowLoop
import FRP.BearRiver.Event

-- * Basic definitions

type Time = Double

type DTime = Double

type SF m = MSF (ClockInfo m)

type ClockInfo m = ReaderT DTime m

-- ** Lifting
arrPrim :: Monad m => (a -> b) -> SF m a b
arrPrim = arr

arrEPrim :: Monad m => (Event a -> b) -> SF m (Event a) b
arrEPrim = arr

identity :: Monad m => SF m a a
identity = Category.id

constant :: Monad m => b -> SF m a b
constant = arr . const

dup  x     = (x,x)