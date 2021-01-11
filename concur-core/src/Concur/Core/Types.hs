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
    SuspendF (..),
    continue,
    wrapView,
    awaitViewAction,
    MultiAlternative (..),
    andd,
    remoteWidget,
    unsafeBlockingIO,
    MonadUnsafeBlockingIO (..),
    MonadSafeBlockingIO (..),
    MonadView (..),
    -- re-export from exceptions
    MonadThrow (..),
    MonadCatch (..),
    -- re-export from resourct
    MonadResource (..),
    ResourceT,
    runResourceT,
    allocate,
    release,
) where

import Control.Applicative (Alternative, empty, (<|>))
import Control.Concurrent (MVar, ThreadId, forkIO, killThread, newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Monad (MonadPlus (..), forM)
import Control.Monad.Catch (MonadCatch (..), MonadThrow (..))
import Control.Monad.Free (Free (..), hoistFree, liftF)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadTrans (lift), ReaderT, mapReaderT)
import Control.Monad.Trans.Resource (MonadResource (..), ReleaseKey, ResourceT, allocate, release, runResourceT)
import Control.MultiAlternative (MultiAlternative, never, orr)
import Data.Bifunctor (Bifunctor (first))
import Data.Maybe (fromMaybe)
import UnliftIO.Exception as Safe

import qualified Concur.Core.Notify as N
import Data.Function ((&))

{- | Functor for Widegt Free Monad

 `(ThreadId -> IO ())' of `StepBlock' is called canceller. For the details refer
 "note about cancelling". Its purely for internall implemenation. Top-level
 intepreter could just ignore it.

 `Forever' constructor should be semantically same as `StepBlock (forever (threadDelay maxBound))'.
 It gives us more information than opaque StepBlock constructor, making some optimization possible.
 It makes interpreters possible to terminate.

 TODO: Add note about ResourceT inside `StepIO (ResourceT IO next)'

 Main purpose is to prevent thread-leaks when main thread was interupted by
 asynchronous exception. Also user can use MonadResource methods.
-}
data SuspendF v next
    = StepView v next
    | StepBlock Canceller (IO next)
    | StepIO (ResourceT IO next)
    | Forever

type Canceller = ThreadId -> MVar () -> IO ()

deriving instance Functor (SuspendF v)

newtype Widget v a = Widget {unWidget :: Free (SuspendF v) a}
    deriving (Functor, Applicative, Monad)

_view :: v -> Widget v ()
_view v = Widget $ liftF $ StepView v ()

{- | IO witch will exectued concurrently

  * effect is usually executed in a different thread for concurrency.
    Exception is that when there is only one effect, which doesn't need to run concurrently.
  * Do not catch asynchronous exception and continue. It will leak thread.
    Cleaning resource with `bracket'-pattern is ok, but while cleaning concur will stop.
    Asynchronous exception are throwned by concur model, not external.
  * Raising a synchronous exception, if handled, will propagate upwords the
    concur tree formed by `orr' inside main thread, cancelling subling and its descending effect threads.
    Be aware that not all excpetion is handled (see the note bellow).
    If catched by `catch`, it will stop there. Otherwise, main thread of concur would raise exception.
-}
effect :: IO a -> Widget v a
effect = effect' _defaultCanceller

-- Its important `killThread' even if the doneVar is already fill.
-- See implemnatation note for `orr'.
_defaultCanceller :: Canceller
_defaultCanceller tid doneVar = do
    killThread tid
    takeMVar doneVar

effect' :: Canceller -> IO a -> Widget v a
effect' c a = Widget $ liftF $ StepBlock c a

{- | IO witch will executed synchronously

 Of course the exact semenatic is determined by the free monad interpreter.
 Here we assume the interpreter will simply execute IO synchonously.

  * io is executed by the main thread of concur.
  * While exectuing io it will block the whole concur model, so be sure to make it short.
  * Do not catch asynchronous exception and continue. It will keap concur alive when it should die.
    Cleaning resource with `bracket'-pattern is ok, but while cleaning concur will stop.
    Asynchronous exception are throwned by external.
  * Raising a synchronous exception will propagate upwords the concur tree formed by `orr',
    cancelling subling and its descending effect threads.
    If catched by `catch`, it will stop there. Otherwise, main thread of concur would raise exception.
-}
io :: IO a -> Widget v a
io = Widget . liftF . StepIO . liftIO

io' :: ResourceT IO a -> Widget v a
io' = Widget . liftF . StepIO

{- | Note about synchronous exception

 1) Not all exception raised by effect will propagated to main thread.
    Only ones that its result value, which is exception, was handled by main thread.

 For example, we have a orr widget with two child widget with effect,
 widgetA and widgetB:

    orr [ widgetA, widgetB ]

 say, widgetA's effect completed first. While the main thread handling widgetA,
 processing its continuation, widgetB's effect might raise a synchronous excpetion.
 If the result of processing widgetA continuation leads to termination with a value,
 then `orr' widget itself will terminate with this value, ignoring exeption rasied by
 widgetB.

 The reasoning behind this semantic is that, other than it is easy to implement,
 if processing widgets in main thread had ideally zero-time cost, then widgetB should
 had been cancelled before it raised an exception.

 Also, there is more complex scenario where two are more effects raises exception almost
 at the same time, or `catch' is involed, but the key thing is that exception are not
 recongised until main thread takes that information out of mvar.

 2) All exception raised inside `io' will be propagated.
    Since `io' is always executed by main thread.
-}
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
    g (StepBlock c a) = StepBlock c a
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
 Only syncrounous exception is catched.
 Syncrounous exception is not determined by exception has `SomeAsyncException' in its hierarchy.
-}
_catch :: forall a e v. Exception e => Widget v a -> (e -> Widget v a) -> Widget v a
_catch (Widget w) handler = step w
  where
    step :: Free (SuspendF v) a -> Widget v a
    step (Free (StepView v next)) = _view v *> step next
    step (Free (StepIO a)) = io' (Safe.try @_ @e a) >>= either handler step
    step (Free (StepBlock c a)) = effect' c (Safe.try @_ @e a) >>= either handler step
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

instance MonadResource (Widget v) where
    liftResourceT = io'

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
data ChildWidget a
    = Running ReleaseKey
    | Terminated

data BlockingIO v a
    = BlockingIO Canceller (IO (Free (SuspendF v) a))
    | BlockingForever

instance Monoid v => MultiAlternative (Widget v) where
    never = _display mempty

    -- Addhing this pattarn-match doesn't change semantics.
    -- Instead of foerver-blocking StepBlock, we get Forever, which can be used to optimize.
    orr [] = _display mempty
    -- Single child widget case.
    -- orr [w] should be semanticaly same to w.
    orr [w] = w
    -- General case.
    orr ws_ = firstStep ws_
      where
        firstStep :: [Widget v a] -> Widget v a
        firstStep ws0 = do
            ws' <- io' $ traverse (step mempty . unWidget) ws0
            case sequence ws' of
                Left a ->
                    pure a
                Right ws'' -> do
                    -- We will always update view before executing block ios.
                    -- If a child widget didn't raise any view, `mempty' will be used.
                    let ws = fmap (first (fromMaybe mempty)) ws''
                    _view $ mconcat $ map fst ws
                    if all isBlockingForever (fmap snd ws)
                        then forever
                        else running0 ws

        -- There was atleast one BlockingIO.
        -- Running all BlockingIO's
        running0 :: [(v, BlockingIO v a)] -> Widget v a
        running0 ws = do
            mvar <- io newEmptyMVar
            child <- io' $ forM (zip [0 ..] ws) $ \(i, (v, b)) -> (v,) <$> forkBlockingIO mvar i v b
            running mvar child

        -- Wait for one of child effect thread to terminate and puts its widget
        -- continuation (or exception value) in MVar. This "waiting" is done by
        -- `effect', which we'll call ""intermediate waiting". If there is a
        -- parent `orr', this effect will be executed in a thread by its parent,
        -- and if it terminates first, `orr''s widget continuation after point
        -- (1) will be putted to parents MVar (Yay! Recursion :)). If there is
        -- no parent `orr', this effect will executed inside main thread by
        -- top-level interpreter.
        --
        -- Note about cancelling:
        --
        -- When one of the child widget terminates with a value or an exception,
        -- we need to terminate all other child widget's and its decending
        -- widget's effect threads before we terminate `orr' widget with given
        -- value or propaget exception with `io'. Or else we might leak threads.
        --
        -- Just sending asynchronous exception with `killThread' is not enough.
        -- If all of the subling threads were "real effect thread", then thats
        -- enough, but if any of them are "intermediate waiting thread", then
        -- its decendants threads might leak.
        --
        -- One solution you might think is to imitate thread tree from `async'
        -- library, where thread forms a tree, and terminating a node thread
        -- will cause killing entire subtree to termiante, preventing
        -- thread-leaks. We can't take this approuch since "intermediate waiting
        -- thread" can terminate eariler than its desending threads, making
        -- "orphan thread"s. (note 1).
        --
        -- Thus, to solve this problem, we'll make `effect' have another
        -- argument "Canceller", which has type `ThreadId -> IO ()'. Canceller's
        -- responsbility is to terminate its corresponding effect thread given
        -- by its thread id, including its descnedants. be carefull that:
        --
        --  * Corresponding effect thread might be termianted at the point
        --    when canceller is invoked (note 2)
        --  * Canceller will be executed inside main thread
        --  * Canceller should not raise any syncrounous exception
        --
        -- note 1:
        --
        -- There might be a way not to terminate "intermediate waiting threads"
        -- before its descedants, but haven't figured it out. Changing `takeMVar'
        -- of (1) to `readMVar' might look like it'll work, but still there is
        -- case which orphan thread occurs.
        --
        -- note 2:
        --
        -- This means its result(continuation) chould be already putted to
        -- MVar. Meaning we might accidenatly revoke the widget we are trying to
        -- cancel. But since the thread pass from current `orr' to root is all
        -- terminated, the result should't be propageted further.
        --
        -- We need (2) since Syncrounous exception raised in `effect' will be
        -- raised through `io'. Actually `onException' handles asynchronous
        -- exception too. We don't need to handle asynchronous exception here
        -- since cleanup for asynchronous exceptions are handled otherwise(This
        -- is because asynchronous exception could be thrown to main thread when
        -- exectuing view or other things). Anyway it doesn't cause any harm.
        --
        -- TODO: I'm not exactly sure about note 2. Maybe we need to adjust we to empty those MVar's...
        -- TODO: If there is only one Widget left, we could just return that widget with proper `mapView'
        -- This will reduce the ammount of thread needed.
        running ::
            MVar (Either SomeException (Int, v, Free (SuspendF v) a)) ->
            [(v, ChildWidget a)] ->
            Widget v a
        running mvar child = do
            r0 <- effect' canceller $ takeMVar mvar -- (1)
            case r0 of
                Left e -> do
                    io $ cancelChilds *> Safe.throwIO e
                Right (i, v0, w) -> do
                    r1 <- io' $ step Nothing w `Safe.onException` liftIO cancelChilds -- (2)
                    case r1 of
                        Left a -> do
                            io cancelChilds
                            pure a
                        Right (mv, blocking) -> do
                            -- Only update view if widget yeild any view before we reach the next blocking.
                            -- There is no meaning updating view with the previous view (v0).
                            -- TODO: I'm not exactly sure about this case. Maybe there is case where we must to update view.
                            v <- case mv of
                                Just v -> do
                                    _view $ mconcat $ take i (map fst child) <> [v] <> drop (i + 1) (map fst child)
                                    pure v
                                Nothing -> pure v0
                            c <- io' $ forkBlockingIO mvar i v blocking
                            let newChild = take i child <> [(v, c)] <> drop (i + 1) child
                            if isTerminated c && all isTerminated (fmap snd newChild)
                                then forever
                                else running mvar newChild
          where
            cancelChilds = sequence_ [release rkey | (_, Running rkey) <- child]
            canceller tid doneVar = cancelChilds *> _defaultCanceller tid doneVar

        -- Only capture syncrhrouns exception, and put it in resultVar.
        -- Asynchronous exception are throwned by concur main thread to stop
        -- these, so it safe to just terminate a thread in such case.
        --
        -- Either case, doneVar should be filled when given io is termianted
        -- (value or exception). `doneVar' will be used by the canceller to wait
        -- of completion of cancelling. Even if `doneVar' is filled, its
        -- paossible `putMVar resultVar' is blocking. So killThread first and
        -- wait for doneVar to be filled afterwards.
        --
        -- Because of this requirement, we can't use `async' library.
        --
        forkBlockingIO resultVar index view' = \case
            BlockingIO canceller blockIO -> do
                (releaseKey, _) <-
                    allocate
                        ( do
                            doneVar <- newEmptyMVar
                            tid <- forkIO $ do
                                r <- Safe.try $ blockIO `Safe.finally` putMVar doneVar ()
                                putMVar resultVar $ fmap (index,view',) r
                            pure (tid, doneVar)
                        )
                        (uncurry canceller)
                pure $ Running releaseKey
            BlockingForever ->
                pure Terminated

        -- Import thing is that we evaluate `StepIO's effect.
        -- Result v is the last View before Blocking(StepBlock, StepForever).
        step :: Maybe v -> Free (SuspendF v) a -> ResourceT IO (Either a (Maybe v, BlockingIO v a))
        step _ (Free (StepView v next)) = step (Just v) next
        step v (Free (StepIO a)) = a >>= step v
        step v (Free (StepBlock c a)) = pure $ Right (v, BlockingIO c a)
        step v (Free Forever) = pure $ Right (v, BlockingForever)
        step _ (Pure a) = pure $ Left a

        isTerminated Terminated = True
        isTerminated _ = False

        isBlockingForever (BlockingIO _ _) = False
        isBlockingForever BlockingForever = True

-- | Wait for all widgets to terminate with a value.
-- Widgets that termianted early will udpate its view with mempty and wait for other widgets.
andd ::
    ( MonadUnsafeBlockingIO m
    , MonadSafeBlockingIO m
    , MultiAlternative m
    , MonadView v m
    , Monoid v
    ) =>
    [m a] ->
    m [a]
andd [] = pure []
andd [w] = (: []) <$> w
andd ws0 = do
    ws <- liftUnsafeBlockingIO $ traverse (\w -> (w,) <$> newEmptyMVar) ws0
    orr
        [ orr $
            map
                ( \(w, var) -> do
                    liftUnsafeBlockingIO . putMVar var =<< w
                    display mempty
                )
                ws
        , liftSafeBlockingIO $ traverse (readMVar . snd) ws
        ]

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
-- `view' might encorouage imprative style coding.
class Monad m => MonadView v m | m -> v where
    -- view :: v -> m ()
    display :: v -> m a
    mapView :: (v -> v) -> m a -> m a

instance MonadView v (Widget v) where
    -- view = _view
    display = _display
    mapView = _mapView

instance MonadView v m => MonadView v (ReaderT r m) where
    -- view = lift . view
    display = lift . display
    mapView f = mapReaderT (mapView f)

-- instance MonadView v m => MonadView v (ExceptT e m) where
--     view = lift . view
--     display = lift . display
--     mapView f = mapExceptT (mapView f)
