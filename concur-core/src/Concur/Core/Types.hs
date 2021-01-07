{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Concur.Core.Types (
    Widget (..),
    continue,
    wrapView,
    SuspendF (..),
    awaitViewAction,
    MultiAlternative (..),
    remoteWidget,
    unsafeBlockingIO,
    MonadUnsafeBlockingIO (..),
    MonadSafeBlockingIO (..),
    MonadView (..),
) where

import Control.Applicative (Alternative, empty, (<|>))
import Control.Concurrent (MVar, ThreadId, forkIO, killThread, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (Exception, throwIO, try)
import Control.Monad (MonadPlus (..), forM)
import Control.Monad.Catch (MonadCatch (..), MonadThrow (..))
import Control.Monad.Free (Free (..), hoistFree, liftF)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadTrans (lift), ReaderT, mapReaderT)
import Control.MultiAlternative (MultiAlternative, never, orr)

import qualified Concur.Core.Notify as N

{- | Functor for Widegt Free Monad

 `Forever' constructor should be semantically same as `StepBlock (forever (threadDelay maxBound))'.
 It gives us more information than opaque StepBlock constructor, making some optimization possible.
 It makes interpreters possible to terminate.
-}
data SuspendF v next
    = StepView v next
    | StepBlock (IO next)
    | StepIO (IO next)
    | Forever

deriving instance Functor (SuspendF v)

newtype Widget v a = Widget {unWidget :: Free (SuspendF v) a}
    deriving (Functor, Applicative, Monad)

_view :: v -> Widget v ()
_view v = Widget $ liftF $ StepView v ()

effect :: IO a -> Widget v a
effect a = Widget $ liftF $ StepBlock a

io :: IO a -> Widget v a
io a = Widget $ liftF $ StepIO a

forever :: Widget v a
forever = Widget $ liftF Forever

continue :: SuspendF v a -> Widget v a
continue = Widget . liftF

_display :: v -> Widget v a
_display v = _view v >> forever

-- Change the view of a Widget
_mapView :: (u -> v) -> Widget u a -> Widget v a
_mapView f (Widget w) = Widget $ go w
  where
    go = hoistFree g
    g (StepView v next) = StepView (f v) next
    g (StepIO a) = StepIO a
    g (StepBlock a) = StepBlock a
    g Forever = Forever

-- Generic widget view wrapper
-- DEPRECATE: Use mapView (pure . f) directly
-- This function isn't worth adding operation to this module. Also it can't used with MonadView.
wrapView :: Applicative f => (u -> v) -> Widget u a -> Widget (f v) a
wrapView f = _mapView (pure . f)

_throwM :: Exception e => e -> Widget v a
_throwM = io . throwIO

{- | Catch exception (Experimental implementation)
 Catches syncrounous exception caused by `io' or `effect'.
 scoped operation なので当然 interpreter が必要になる。
 同期例外は補足可能。
 非同期例外は補足できる場合とそうでない場合が多分ある。
-}
_catch :: forall a e v. Exception e => Widget v a -> (e -> Widget v a) -> Widget v a
_catch (Widget w) handler = step w
  where
    step :: Free (SuspendF v) a -> Widget v a
    step (Free (StepView v next)) = _view v *> step next
    step (Free (StepIO a)) = io (try @e a) >>= either handler step
    step (Free (StepBlock a)) = effect (try @e a) >>= either handler step
    step (Free Forever) = forever
    step (Pure a) = pure a

{- | IMPORTANT: Blocking IO is dangerous as it can block the entire UI from updating.
   It should only be used for *very* quick running IO actions like creating MVars.
-}
unsafeBlockingIO :: IO a -> Widget v a
unsafeBlockingIO = io

-- This is a safe use for blockingIO, and is exported
awaitViewAction :: (N.Notify a -> v) -> Widget v a
awaitViewAction f = do
    n <- io N.newNotify
    _view (f n)
    effect $ N.await n

-- Make a Widget, which can be pushed to remotely
remoteWidget ::
    ( MultiAlternative m
    , MonadUnsafeBlockingIO m
    , MonadSafeBlockingIO m
    , Monad m
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

instance MonadIO (Widget v) where
    liftIO = effect

instance MonadThrow (Widget v) where
    throwM = _throwM

instance MonadCatch (Widget v) where
    catch = _catch

-- IMPORTANT NOTE: This Alternative instance is NOT the same one as that for Free.
-- That one simply uses Alternative for Suspend. But that one isn't sufficient for us.
-- Verify laws:
--         Right distributivity (of <*>):  (f <|> g) <*> a = (f <*> a) <|> (g <*> a)
--         Right absorption (for <*>):  empty <*> a = empty
--         Left distributivity (of fmap):  f <$> (a <|> b) = (f <$> a) <|> (f <$> b)
--  OK     Left absorption (for fmap):  f <$> empty = empty
instance Monoid v => Alternative (Widget v) where
    empty = never
    f <|> g = orr [f, g]

-- To avoid Either-blindness and Maybe-blindness,
-- define data types solely for implementating orr.
data ChildWidget v a
    = Idling (Free (SuspendF v) a)
    | Running v ThreadId
    | Terminated v

--
stepW :: v -> Free (SuspendF v) a -> IO (Either a (v, Maybe (IO (Free (SuspendF v) a))))
stepW _ (Free (StepView v next)) = stepW v next
stepW v (Free (StepIO a)) = a >>= stepW v
stepW v (Free (StepBlock a)) = pure $ Right (v, Just a)
stepW v (Free Forever) = pure $ Right (v, Nothing)
stepW _ (Pure a) = pure $ Left a

instance Monoid v => MultiAlternative (Widget v) where
    never = _display mempty

    -- Addhing this pattarn-match doesn't change semantics.
    -- Instead of foerver-blocking StepBlock, we get Forever, which can be used to optimize.
    orr [] = _display mempty
    -- Single child widget
    -- Following commented out code is the previous implementation for case [w].
    -- I don't think we need to interpret given Widget.
    -- Just past it bellow should be enough and also most efficient.
    -- But I'm not sure so I'll keep this code as comment.
    orr [w] = w
    -- Single child fast path without threads
    -- orr [w] = go (unWidget w)
    --   where
    --     go widget = do
    --         stepped <- io $ stepW mempty widget

    --         case stepped of
    --             Left a -> pure a
    --             Right (v, Nothing) -> _view v >> forever
    --             Right (v, Just await) -> do
    --                 _view v
    --                 next <- effect await
    --                 go next

    -- General threaded case
    orr ws = do
        mvar <- io newEmptyMVar
        comb mvar $ fmap (Idling . unWidget) ws
      where
        comb ::
            MVar (Int, Free (SuspendF v) a) ->
            [ChildWidget v a] ->
            Widget v a
        comb mvar widgets = do
            -- 外側の Either の Left が値を得たときに必要
            -- 例え一番目の Widget が pure で即座に値を返そうが,後続のWidget の先頭の io は実行される。
            -- 公平性の観点からはいいのか？ただ どうせ 複数の Widget が 同時に値を返す場合に左偏重である。
            stepped <- io $
                forM widgets $ \case
                    Idling suspended -> either Left (Right . Left) <$> stepW mempty suspended
                    Running displayed tid -> pure $ Right $ Right (displayed, Just tid)
                    Terminated displayed -> pure $ Right $ Right (displayed, Nothing)

            case sequence stepped of
                -- A widget finished, kill all running threads
                Left a -> do
                    io $
                        sequence_
                            [ killThread tid
                            | Right (Right (_, Just tid)) <- stepped
                            ]
                    pure a
                Right next -> do
                    -- Display all current views
                    _view $ mconcat $ map (either fst fst) next

                    tids <- io $
                        forM (zip [0 ..] next) $ \(i, v) -> case v of
                            -- Start a new thread on encountering StepBlock
                            Left (dv, Just await) -> fmap (Running dv) $
                                forkIO $ do
                                    a <- await
                                    putMVar mvar (i, a)

                            -- Neverending Widget, pass on
                            Left (dv, Nothing) -> pure $ Terminated dv
                            -- Already running, pass on
                            Right (dv, Just tid) -> pure $ Running dv tid
                            Right (dv, Nothing) -> pure $ Terminated dv

                    (i, newWidget) <- effect $ takeMVar mvar
                    comb mvar (take i tids ++ [Idling newWidget] ++ drop (i + 1) tids)

-- The default instance derives from Alternative
instance Monoid v => MonadPlus (Widget v)

-- | MonadUnsafeBlockingIO
class Monad m => MonadUnsafeBlockingIO m where
    liftUnsafeBlockingIO :: IO a -> m a

instance MonadUnsafeBlockingIO (Widget v) where
    liftUnsafeBlockingIO = io

instance MonadUnsafeBlockingIO m => MonadUnsafeBlockingIO (ReaderT r m) where
    liftUnsafeBlockingIO = lift . liftUnsafeBlockingIO

-- instance MonadUnsafeBlockingIO m => MonadUnsafeBlockingIO (ExceptT e m) where
--     liftUnsafeBlockingIO = lift . liftUnsafeBlockingIO

-- | MonadSafeBlockingIO
class Monad m => MonadSafeBlockingIO m where
    liftSafeBlockingIO :: IO a -> m a

instance MonadSafeBlockingIO (Widget v) where
    liftSafeBlockingIO = effect

instance MonadSafeBlockingIO m => MonadSafeBlockingIO (ReaderT r m) where
    liftSafeBlockingIO = lift . liftSafeBlockingIO

-- instance MonadSafeBlockingIO m => MonadSafeBlockingIO (ExceptT e m) where
--     liftSafeBlockingIO = lift . liftSafeBlockingIO

-- | MonadView

-- TODO: Maybe `view' shouldn't be exposed to user. Only `display'
-- `view' might encorouage imprative style of coding.
class Monad m => MonadView v m | m -> v where
    view :: v -> m ()
    display :: v -> m a
    mapView :: (v -> v) -> m a -> m a

instance MonadView v (Widget v) where
    view = _view
    display = _display
    mapView = _mapView

instance MonadView v m => MonadView v (ReaderT r m) where
    view = lift . view
    display = lift . display
    mapView f = mapReaderT (mapView f)

-- instance MonadView v m => MonadView v (ExceptT e m) where
--     view = lift . view
--     display = lift . display
--     mapView f = mapExceptT (mapView f)
