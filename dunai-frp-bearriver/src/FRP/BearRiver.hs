{-# LANGUAGE Arrows     #-}
{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
-- The following warning is disabled so that we do not see warnings due to
-- using ListT on an MSF to implement parallelism with broadcasting.
#if __GLASGOW_HASKELL__ < 800
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
#else
{-# OPTIONS_GHC -Wno-deprecations #-}
#endif
{-# OPTIONS_HADDOCK ignore-exports #-}
-- |
-- Copyright  : (c) Ivan Perez, 2019-2022
--              (c) Ivan Perez and Manuel Baerenz, 2016-2018
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- Implementation of Yampa using Monadic Stream Processing library.
module FRP.BearRiver (module X) where
-- This is an implementation of Yampa using our Monadic Stream Processing
-- library. We focus only on core Yampa. We will use this module later to
-- reimplement an example of a Yampa system.
--
-- While we may not introduce all the complexity of Yampa today (all kinds of
-- switches, etc.) our goal is to show that the approach is promising and that
-- there do not seem to exist any obvious limitations.

import Data.MonadicStreamFunction as X hiding ( link,
                                                reactimate,
                                                repeatedly,
                                                sum,
                                                switch,
                                                trace)
import FRP.BearRiver.Basic        as X
import FRP.BearRiver.Event        as X
import FRP.BearRiver.EventSource  as X
import FRP.BearRiver.Hybrid       as X
import FRP.BearRiver.Integration  as X
import FRP.BearRiver.Monad        as X
import FRP.BearRiver.Parallel     as X
import FRP.BearRiver.Simulation   as X
import FRP.BearRiver.Stateful     as X
import FRP.BearRiver.Switch       as X
import FRP.BearRiver.Task         as X
import FRP.BearRiver.Time         as X
