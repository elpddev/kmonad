
-- | Constructor for linux sync events. Whenever you write an event to linux,
-- you need to emit a 'sync' to signal to linux to indicate that it should sync
-- all queued updates.
--
-- NOTE: This is in 'Types' because it is essentially just a data-constructor
sync :: UTCTime -> RawEvent
sync t = let (MkSystemTime s ns) = t ^. systemTime
         in RawEvent (fi s) (fi ns) 0 0 0
