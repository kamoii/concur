module Concur.Core.Control where

import Concur.Core.Types (MonadSafeBlockingIO (liftSafeBlockingIO), MonadUnsafeBlockingIO (liftUnsafeBlockingIO), MultiAlternative (orr))
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)

-- Make a Widget, which can be pushed to remotely
remoteWidget ::
    ( MultiAlternative m
    , MonadUnsafeBlockingIO m
    , MonadSafeBlockingIO m
    ) =>
    m b ->
    (a -> m b) ->
    IO (a -> m (), m b)
remoteWidget d f = do
    var <- newEmptyMVar
    return (proxy var, wid var d)
  where
    proxy var = liftUnsafeBlockingIO . putMVar var
    wid var ui = orr [Left <$> ui, Right <$> liftSafeBlockingIO (takeMVar var)] >>= either return (wid var . f)
