module FRP.BearRiver.Event where

data Event a = Event a | NoEvent
 deriving (Eq, Show)

-- | The type 'Event' is isomorphic to 'Maybe'. The 'Functor' instance of
-- 'Event' is analogous to the 'Functo' instance of 'Maybe', where the given
-- function is applied to the value inside the 'Event', if any.
instance Functor Event where
  fmap _ NoEvent   = NoEvent
  fmap f (Event c) = Event (f c)

-- | The type 'Event' is isomorphic to 'Maybe'. The 'Applicative' instance of
-- 'Event' is analogous to the 'Applicative' instance of 'Maybe', where the
-- lack of a value (i.e., 'NoEvent') causes '(<*>)' to produce no value
-- ('NoEvent').
instance Applicative Event where
  pure = Event

  Event f <*> Event x = Event (f x)
  _       <*> _       = NoEvent

-- | The type 'Event' is isomorphic to 'Maybe'. The 'Monad' instance of 'Event'
-- is analogous to the 'Monad' instance of 'Maybe', where the lack of a value
-- (i.e., 'NoEvent') causes bind to produce no value ('NoEvent').
instance Monad Event where
  return = pure

  Event x >>= f = f x
  NoEvent >>= _ = NoEvent

-- * Pointwise functions on events

noEvent :: Event a
noEvent = NoEvent

-- | Suppress any event in the first component of a pair.
noEventFst :: (Event a, b) -> (Event c, b)
noEventFst (_, b) = (NoEvent, b)


-- | Suppress any event in the second component of a pair.
noEventSnd :: (a, Event b) -> (a, Event c)
noEventSnd (a, _) = (a, NoEvent)

event :: a -> (b -> a) -> Event b -> a
event _ f (Event x) = f x
event x _ NoEvent   = x

fromEvent (Event x) = x
fromEvent _         = error "fromEvent NoEvent"

isEvent (Event _) = True
isEvent _         = False

isNoEvent (Event _) = False
isNoEvent _         = True

tag :: Event a -> b -> Event b
tag NoEvent   _ = NoEvent
tag (Event _) b = Event b

-- | Tags an (occurring) event with a value ("replacing" the old value). Same
-- as 'tag' with the arguments swapped.
--
-- Applicative-based definition:
-- tagWith = (<$)
tagWith :: b -> Event a -> Event b
tagWith = flip tag

-- | Attaches an extra value to the value of an occurring event.
attach :: Event a -> b -> Event (a, b)
e `attach` b = fmap (\a -> (a, b)) e

-- | Left-biased event merge (always prefer left event, if present).
lMerge :: Event a -> Event a -> Event a
lMerge = mergeBy (\e1 _ -> e1)

-- | Right-biased event merge (always prefer right event, if present).
rMerge :: Event a -> Event a -> Event a
rMerge = flip lMerge

merge :: Event a -> Event a -> Event a
merge = mergeBy $ error "Bearriver: merge: Simultaneous event occurrence."

mergeBy :: (a -> a -> a) -> Event a -> Event a -> Event a
mergeBy _       NoEvent      NoEvent      = NoEvent
mergeBy _       le@(Event _) NoEvent      = le
mergeBy _       NoEvent      re@(Event _) = re
mergeBy resolve (Event l)    (Event r)    = Event (resolve l r)

-- | A generic event merge-map utility that maps event occurrences,
-- merging the results. The first three arguments are mapping functions,
-- the third of which will only be used when both events are present.
-- Therefore, 'mergeBy' = 'mapMerge' 'id' 'id'
--
-- Applicative-based definition:
-- mapMerge lf rf lrf le re = (f <$> le <*> re) <|> (lf <$> le) <|> (rf <$> re)
mapMerge :: (a -> c) -> (b -> c) -> (a -> b -> c)
            -> Event a -> Event b -> Event c
mapMerge _  _  _   NoEvent   NoEvent   = NoEvent
mapMerge lf _  _   (Event l) NoEvent   = Event (lf l)
mapMerge _  rf _   NoEvent   (Event r) = Event (rf r)
mapMerge _  _  lrf (Event l) (Event r) = Event (lrf l r)

-- | Merge a list of events; foremost event has priority.
--
-- Foldable-based definition:
-- mergeEvents :: Foldable t => t (Event a) -> Event a
-- mergeEvents =  asum
mergeEvents :: [Event a] -> Event a
mergeEvents = foldr lMerge NoEvent

-- | Collect simultaneous event occurrences; no event if none.
--
-- Traverable-based definition:
-- catEvents :: Foldable t => t (Event a) -> Event (t a)
-- carEvents e  = if (null e) then NoEvent else (sequenceA e)
catEvents :: [Event a] -> Event [a]
catEvents eas = case [ a | Event a <- eas ] of
                    [] -> NoEvent
                    as -> Event as

-- | Join (conjunction) of two events. Only produces an event
-- if both events exist.
--
-- Applicative-based definition:
-- joinE = liftA2 (,)
joinE :: Event a -> Event b -> Event (a,b)
joinE NoEvent   _         = NoEvent
joinE _         NoEvent   = NoEvent
joinE (Event l) (Event r) = Event (l,r)

-- | Split event carrying pairs into two events.
splitE :: Event (a,b) -> (Event a, Event b)
splitE NoEvent       = (NoEvent, NoEvent)
splitE (Event (a,b)) = (Event a, Event b)

------------------------------------------------------------------------------
-- Event filtering
------------------------------------------------------------------------------

-- | Filter out events that don't satisfy some predicate.
filterE :: (a -> Bool) -> Event a -> Event a
filterE p e@(Event a) = if p a then e else NoEvent
filterE _ NoEvent     = NoEvent


-- | Combined event mapping and filtering. Note: since 'Event' is a 'Functor',
-- see 'fmap' for a simpler version of this function with no filtering.
mapFilterE :: (a -> Maybe b) -> Event a -> Event b
mapFilterE _ NoEvent   = NoEvent
mapFilterE f (Event a) = case f a of
                            Nothing -> NoEvent
                            Just b  -> Event b


-- | Enable/disable event occurences based on an external condition.
gate :: Event a -> Bool -> Event a
_ `gate` False = NoEvent
e `gate` True  = e