module KMonad.Prelude.Util
where

import RIO hiding (IO)

import Control.Lens (swapped, folded, toListOf, Prism')
import Control.Exception.Lens
-- import Control.Monad.Except
import KMonad.Prelude.Types

import Foreign.C.Types (CInt)
import qualified System.IO as SIO
import qualified RIO.HashMap as M


-- | Show an object, but no more than @n@ characters
showN :: Show a => Int -> a -> String
showN n = take n . show

-- | Only used when I quickly want to print something when I'm deving
q :: (IO m, Show a) => a -> m ()
q = liftIO . SIO.putStrLn . showN 80

-- | Eases expression of set-to statements into functors
--
-- (\a -> def & myLens .~ a) <$> mkA
-- becomes
-- def `setTo` myLens <$> mkA
setTo :: s -> ASetter s t a b -> b -> t
setTo a l = flip (set l) a


-- | Slightly different modifyMVar to make 1-liners cleaner
overMVar :: UIO m =>  m (MVar a) -> (a -> m (a, b))  -> m b
overMVar a f = a >>= \mv -> modifyMVar mv f

-- | Slightly different modifyMVar_ to make 1-liners cleaner
overMVar_ :: UIO m =>  m (MVar a) -> (a -> m a)  -> m ()
overMVar_ a f = a >>= \mv -> modifyMVar_ mv f

-- | Shorthand to run an action only on a Just
maybeDo :: Applicative m => Maybe a -> (a -> m b) -> m (Maybe b)
maybeDo (Just a) f = Just <$> f a
maybeDo Nothing _  = pure Nothing

-- | Shorthand to run an action only on a Just and ignore the result
maybeDo_ :: Applicative m => Maybe a -> (a -> m b) -> m ()
maybeDo_ (Just a) f = void $ f a
maybeDo_ Nothing _  = pure ()

-- | Run a monadic action until a Just occurs
untilJust :: Monad m => m (Maybe a) -> m a
untilJust go = go >>= \case
  Nothing -> untilJust go
  Just a  -> pure a

-- | Reordered 'either' to make certain patterns easier to write
onEither :: Either a b -> (a -> c) -> (b -> c) -> c
onEither e f g = either f g e

throwL :: (Monad m) => Either e a -> Prism' SomeException e -> m a
throwL (Left e)  l = throwing l e
throwL (Right a) _ = pure a

-- | Makes some of the continuation formulations cleaner to write
inEnv :: IO m => RIO env a -> env -> m a
inEnv = flip runRIO

-- | Simple short-hand for fromIntegral
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral



--------------------------------------------------------------------------------
-- $ffi
--
-- NOTE: This is almost exactly the same as ExitCode from @base@, but that is
-- used to signal success/failure of the actual haskell program, so we just do a
-- little bit of duplication to disambiguate.

-- | Failures are encodes as integers (except for 0)
type FailCode = Int

-- | The result of some FFI call.
data FFIResult = FFISuccess | FFIFailure FailCode

-- | We always use 0 to signify success, anything else is a failure
ffiReturn :: CInt -> FFIResult
ffiReturn 0 = FFISuccess
ffiReturn n = FFIFailure $ fi n

-- | Helper function to throw some haskell-error when the FFI fails
--
-- >>> someFFIcall `onErr` \n -> _DidNotWork # n "The snozzle did not jig the glorp"
--
onErr :: (MonadUnliftIO m)
  => m FFIResult
  -> (Int -> m ())
  -> m ()
onErr a f = a >>= \case
  FFISuccess   -> pure ()
  FFIFailure n -> f n

-- | Like `onErr`, but ignoring the error-code wrapped in FFIFailure
onErr_ :: (MonadUnliftIO m)
  => m FFIResult
  -> m ()
  -> m ()
onErr_ a f = a >>= \case
  FFISuccess   -> pure ()
  FFIFailure _ -> f

