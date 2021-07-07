{-# LANGUAGE CPP #-}

module FRP.BearRiver.Parallel where

import Control.Monad.Trans.MSF
import Control.Monad.Trans.MSF.List
import Data.MonadicStreamFunction.InternalCore
import FRP.BearRiver.Basic
import FRP.BearRiver.Event

-- * Parallel composition and switching

-- ** Parallel composition and switching over collections with broadcasting

-- #if MIN_VERSION_base(4,8,0)
-- parB :: (Monad m) => [SF m a b] -> SF m a [b]
-- #else
-- parB :: (Functor m, Monad m) => [SF m a b] -> SF m a [b]
-- #endif
-- parB = widthFirst . sequenceS

parB sfs = MSF (\a -> do    bks <- mapM (`unMSF` a) sfs
                            let bs  = fmap fst bks
                                ks  = fmap snd bks
                            return (bs, parB ks))

dpSwitchB :: (Monad m , Traversable col)
          => col (SF m a b) -> SF m (a, col b) (Event c) -> (col (SF m a b) -> c -> SF m a (col b))
          -> SF m a (col b)
dpSwitchB sfs sfF sfCs = MSF $ \a -> do
  res <- mapM (`unMSF` a) sfs
  let bs   = fmap fst res
      sfs' = fmap snd res
  (e,sfF') <- unMSF sfF (a, bs)
  case e of
    Event c -> local (const 0) (unMSF (sfCs sfs' c) a)
    NoEvent -> return (bs, dpSwitchB sfs' sfF' sfCs)

-- ** Parallel composition over collections

parC :: Monad m => SF m a b -> SF m [a] [b]
parC sf = parC0 sf
  where
    parC0 :: Monad m => SF m a b -> SF m [a] [b]
    parC0 sf0 = MSF $ \as -> do
      os <- mapM (\(a,sf) -> unMSF sf a) $ zip as (replicate (length as) sf0)
      let bs  = fmap fst os
          cts = fmap snd os
      return (bs, parC' cts)

    parC' :: Monad m => [SF m a b] -> SF m [a] [b]
    parC' sfs = MSF $ \as -> do
      os <- mapM (\(a,sf) -> unMSF sf a) $ zip as sfs
      let bs  = fmap fst os
          cts = fmap snd os
      return (bs, parC' cts)