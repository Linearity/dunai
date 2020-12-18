{-# LANGUAGE Arrows #-}

module FRP.BearRiver.EventSource where

import Control.Arrow
import Control.Monad.Random
import Control.Monad.Trans.MSF
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.InternalCore
import FRP.BearRiver.Basic
import FRP.BearRiver.Event
import FRP.BearRiver.Stateful
import FRP.BearRiver.Switch

-- | Event source that never occurs.
never :: Monad m => SF m a (Event b)
never = constant NoEvent

-- | Event source with a single occurrence at time 0. The value of the event
-- is given by the function argument.
now :: Monad m => b -> SF m a (Event b)
now b0 = Event b0 --> never

nowM :: Monad m => m b -> SF m a (Event b)
nowM m0 = never >>> replaceOnceM (fmap Event m0)

after :: Monad m
      => Time -- ^ The time /q/ after which the event should be produced
      -> b    -- ^ Value to produce at that time
      -> SF m a (Event b)
after q x = feedback q go
 where go = MSF $ \(_, t) -> do
              dt <- ask
              let t' = t - dt
                  e  = if t > 0 && t' < 0 then Event x else NoEvent
                  ct = if t' < 0 then constant (NoEvent, t') else go
              return ((e, t'), ct)

repeatedly :: Monad m => Time -> b -> SF m a (Event b)
repeatedly q x
    | q > 0     = afterEach qxs
    | otherwise = error "bearriver: repeatedly: Non-positive period."
  where
    qxs = (q,x):qxs

-- | Event source with consecutive occurrences at the given intervals.
-- Should more than one event be scheduled to occur in any sampling interval,
-- only the first will in fact occur to avoid an event backlog.

-- After all, after, repeatedly etc. are defined in terms of afterEach.
afterEach :: Monad m => [(Time,b)] -> SF m a (Event b)
afterEach qxs = afterEachCat qxs >>> arr (fmap head)

-- | Event source with consecutive occurrences at the given intervals.
-- Should more than one event be scheduled to occur in any sampling interval,
-- the output list will contain all events produced during that interval.
afterEachCat :: Monad m => [(Time,b)] -> SF m a (Event [b])
afterEachCat = afterEachCat' 0
  where
    afterEachCat' :: Monad m => Time -> [(Time,b)] -> SF m a (Event [b])
    afterEachCat' _ []  = never
    afterEachCat' t qxs = MSF $ \_ -> do
      dt <- ask
      let t' = t + dt
          (qxsNow, qxsLater) = span (\p -> fst p <= t') qxs
          ev = if null qxsNow then NoEvent else Event (map snd qxsNow)
      return (ev, afterEachCat' t' qxsLater)

-- * Events

-- | Apply an 'MSF' to every input. Freezes temporarily if the input is
-- 'NoEvent', and continues as soon as an 'Event' is received.
mapEventS :: Monad m => MSF m a b -> MSF m (Event a) (Event b)
mapEventS msf = proc eventA -> case eventA of
  Event a -> arr Event <<< msf -< a
  NoEvent -> returnA           -< NoEvent

-- ** Relation to other types

eventToMaybe = event Nothing Just

boolToEvent :: Bool -> Event ()
boolToEvent True  = Event ()
boolToEvent False = NoEvent

-- * Hybrid SF m combinators

edge :: Monad m => SF m Bool (Event ())
edge = edgeFrom True

iEdge :: Monad m => Bool -> SF m Bool (Event ())
iEdge = edgeFrom

-- | Like 'edge', but parameterized on the tag value.
--
-- From Yampa
edgeTag :: Monad m => a -> SF m Bool (Event a)
edgeTag a = edge >>> arr (`tag` a)

-- | Edge detector particularized for detecting transtitions
--   on a 'Maybe' signal from 'Nothing' to 'Just'.
--
-- From Yampa

-- !!! 2005-07-09: To be done or eliminated
-- !!! Maybe could be kept as is, but could be easy to implement directly
-- !!! in terms of sscan?
edgeJust :: Monad m => SF m (Maybe a) (Event a)
edgeJust = edgeBy isJustEdge (Just undefined)
    where
        isJustEdge Nothing  Nothing     = Nothing
        isJustEdge Nothing  ma@(Just _) = ma
        isJustEdge (Just _) (Just _)    = Nothing
        isJustEdge (Just _) Nothing     = Nothing

edgeBy :: Monad m => (a -> a -> Maybe b) -> a -> SF m a (Event b)
edgeBy isEdge a_prev = MSF $ \a ->
  return (maybeToEvent (isEdge a_prev a), edgeBy isEdge a)

maybeToEvent :: Maybe a -> Event a
maybeToEvent = maybe NoEvent Event

edgeFrom :: Monad m => Bool -> SF m Bool (Event())
edgeFrom prev = MSF $ \a -> do
  let res | prev      = NoEvent
          | a         = Event ()
          | otherwise = NoEvent
      ct  = edgeFrom a
  return (res, ct)

{-
A signal may be mostly constant but change from time to time. The signal
|change| detects these changes in its input signal and produces a stream of
corresponding events.
-}
change :: (Monad m, Eq a) => SF m a (Event a)
change = MSF (\a -> return (NoEvent, changeFrom a))

changeFrom prev = MSF (\a -> let    res | prev == a   = NoEvent
                                        | otherwise   = Event a
                                in return (res, changeFrom a))

-- * Stateful event suppression

-- | Suppression of initial (at local time 0) event.
notYet :: Monad m => SF m (Event a) (Event a)
notYet = feedback False $ arr (\(e,c) ->
  if c then (e, True) else (NoEvent, True))

-- | Suppress all but the first event.
once :: Monad m => SF m (Event a) (Event a)
once = takeEvents 1

-- | Suppress all but the first n events.
takeEvents :: Monad m => Int -> SF m (Event a) (Event a)
takeEvents n | n <= 0 = never
takeEvents n = dSwitch (arr dup) (const (NoEvent >-- takeEvents (n - 1)))

-- | Suppress first n events.

-- Here dSwitch or switch does not really matter.
dropEvents :: Monad m => Int -> SF m (Event a) (Event a)
dropEvents n | n <= 0  = identity
dropEvents n = dSwitch (never &&& identity)
                             (const (NoEvent >-- dropEvents (n - 1)))

-- * Noise (random signal) sources and stochastic event sources

occasionally :: MonadRandom m
             => Time -- ^ The time /q/ after which the event should be produced on average
             -> b    -- ^ Value to produce at time of event
             -> SF m a (Event b)
occasionally tAvg b
  | tAvg <= 0 = error "bearriver: Non-positive average interval in occasionally."
  | otherwise = proc _ -> do
      r   <- getRandomRS (0, 1) -< ()
      dt  <- timeDelta          -< ()
      let p = 1 - exp (-(dt / tAvg))
      returnA -< if r < p then Event b else NoEvent
 where
  timeDelta :: Monad m => SF m a DTime
  timeDelta = constM ask