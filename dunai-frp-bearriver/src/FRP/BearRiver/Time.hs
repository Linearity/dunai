module FRP.BearRiver.Time where

import Control.Arrow
import qualified Control.Category as Category
import Control.Monad.Trans.Reader
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.InternalCore
import FRP.BearRiver.Basic
import FRP.BearRiver.Integration

localTime :: Monad m => SF m a Time
localTime = constant 1.0 >>> integral

time :: Monad m => SF m a Time
time = localTime

arrTime :: Monad m => (DTime -> a -> b) -> SF m a b
arrTime f = arrM (\a -> reader f >>= \fdt -> return (fdt a))

delay :: Monad m => Time -> a -> SF m a a
delay q a0
        | q <= 0        = identity
        | otherwise     = MSF (\a -> return (a0, wait [] [(q, a)] 0 a0))
    where   wait rbuf buf@((bdt, ba) : buf') t0 a_prev
                  = MSF (\a -> do   dt  <- ask
                                    let t1      = t0 + dt
                                        rbuf'   = (dt, a) : rbuf
                                    if t1 < bdt
                                        then return (a_prev, wait rbuf' buf t1 a_prev)
                                        else next rbuf' buf' (t1 - bdt) ba)
            next rbuf [] t0 a
                = next [] (reverse rbuf) t0 a
            next rbuf buf@((bdt, ba) : buf') t0 a
                | t0 < bdt      = return (a, wait rbuf buf t0 a)
                | otherwise     = next rbuf buf' (t0 - bdt) ba