module FRP.BearRiver.Switch where

import Control.Monad.Trans.Reader
import Data.MonadicStreamFunction.InternalCore
import FRP.BearRiver.Basic
import FRP.BearRiver.Event

-- * Switching

-- ** Basic switchers

switch :: Monad m => SF m a (b, Event c) -> (c -> SF m a b) -> SF m a b
switch sf sfC = MSF $ \a -> do
  (o, ct) <- unMSF sf a
  case o of
    (_, Event c) -> local (const 0) (unMSF (sfC c) a)
    (b, NoEvent) -> return (b, switch ct sfC)

dSwitch ::  Monad m => SF m a (b, Event c) -> (c -> SF m a b) -> SF m a b
dSwitch sf sfC = MSF $ \a -> do
  (o, ct) <- unMSF sf a
  case o of
    (b, Event c) -> do (_,ct') <- local (const 0) (unMSF (sfC c) a)
                       return (b, ct')
    (b, NoEvent) -> return (b, dSwitch ct sfC)

link' :: Monad m => SF m a (b, Event c) -> (c -> SF m a (b, Event d)) -> SF m a (b, Event d)
link' sf sfC = MSF $ \a -> do
  (o, ct) <- unMSF sf a
  case o of
    (_, Event c) -> local (const 0) (unMSF (sfC c) a)
    (b, NoEvent) -> return ((b, NoEvent), link' ct sfC)

link :: Monad m => SF m a (Either b c) -> (c -> SF m a (Either b d)) -> SF m a (Either b d)
link sf sfC = MSF $ \a -> do
  (o, ct) <- unMSF sf a
  case o of
    Right c -> local (const 0) (unMSF (sfC c) a)
    Left b  -> return (Left b, link ct sfC)