{-# LANGUAGE LambdaCase #-}

module Util.Reloadable
  ( Reloadable(..)
  , onReload
  , access
  , invalidate
  ) where

import Control.Monad.IO.Class
import Control.Concurrent.MVar

data Reloadable a = Reloadable { resource :: MVar a
                               , reload   :: IO a
                               }

onReload :: MonadIO m => IO a -> m (Reloadable a)
onReload action = do
  mvar <- liftIO newEmptyMVar
  return Reloadable { resource = mvar, reload = action }

access :: MonadIO m => Reloadable a -> m a
access res = liftIO $ do
  let mvar = resource res
  tryReadMVar mvar >>= \case
    Just payload -> return payload
    Nothing      -> do ans <- reload res
                       tryPutMVar mvar ans
                       return ans

invalidate :: MonadIO m => Reloadable a -> m ()
invalidate res = liftIO $ do
  takeMVar (resource res)
  return ()
