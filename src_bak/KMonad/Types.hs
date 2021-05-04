module KMonad.Types
  ( Time
  , getCurrentTime

  , OnlyIO, IO, UIO, EnvIO, EnvUIO

  )
where

import RIO hiding (IO)

import qualified RIO.Time as T
import qualified RIO      as R

newtype Time = Time { unTime :: T.UTCTime }
  deriving (Eq, Show)

getCurrentTime :: MonadIO m => m Time
getCurrentTime = Time <$> T.getCurrentTime


--------------------------------------------------------------------------------
-- | Common IO-type constraints. Shorter words for common phrases.

-- | Shorthand for the actual IO type (not the constraint)
type OnlyIO a     = R.IO a
-- | Shorthand for MTL-constraints of MonadIO
type IO     m     = (MonadIO m                         )
-- | Shorthand for MTL-constraints of MonadUnliftIO
type UIO    m     = (MonadUnliftIO m                   )
-- | Shorthand for MTL-constraints of MonadIO with a Reader env
type EnvIO  m env = (MonadIO       m, MonadReader env m)
-- | Shorthand for MTL-constraints of MonadUnliftIO with a Reader env
type EnvUIO m env = (MonadUnliftIO m, MonadReader env m)

-- | Also see: LIO and LUIO in "KMonad.App.Logging"
