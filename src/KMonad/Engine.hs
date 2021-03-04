
-- | The different events that can occur
data Event os where
  ExitEvent  :: CanOS os =>                         UTCTime -> Event os
  TimerEvent :: CanOS os => Tag                  -> UTCTime -> Event os
  KeyEvent   :: CanOS os => Keycode os -> Switch -> UTCTime -> Event os
makePrisms ''Event



instance CanOS os => Eq (Event os) where
  (ExitEvent t1) == (ExitEvent t2) = t1 == t2
  (TimerEvent q1 t1) == (TimerEvent q2 t2) = q1 == q2 && t1 == t2
  (KeyEvent c1 s1 t1) == (KeyEvent c2 s2 t2) = c1 == c2 && s1 == s2 && t1 == t2
  _ == _ = False

instance CanOS os => Show (Event os) where
  show (ExitEvent t)    = unwords ["ExitEvent", show t]
  show (TimerEvent q t) = unwords ["TimerEvent", show q, show t]
  show (KeyEvent c s t) = unwords ["KeyEvent", show c, show s, show t]

-- | Instance for time for 'Event'
instance HasTime (Event os) where
  time = to $ \case ExitEvent      t -> t
                    TimerEvent _   t -> t
                    KeyEvent   _ _ t -> t


-- | Tags we use do identify events with
newtype Tag = Tag { unTag :: Unique } deriving (Eq)
instance Show Tag where show = show . hashUnique . unTag

-- | Create a new tag
mkTag :: MonadIO m => m Tag
mkTag = liftIO $ Tag <$> newUnique

