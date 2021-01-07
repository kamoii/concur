{-# LANGUAGE TypeApplications #-}

import Concur.Core
import Control.Applicative (Alternative ((<|>)))
import Control.Concurrent (threadDelay)
import Control.Exception (Exception, throwIO, try)
import Control.Monad.Catch (MonadCatch (catch), MonadThrow (throwM))
import Control.Monad.Free (Free (Free, Pure))
import Data.Functor (($>))
import qualified Data.IORef as IORef
import Data.Void (Void)
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

data WidgetOp v a
    = WOView v
    | WODone a
    | WOForever
    deriving (Show, Eq)

runWidget :: Widget String a -> IO [WidgetOp String a]
runWidget (Widget w) = go w
  where
    go :: Free (SuspendF v) a -> IO [WidgetOp v a]
    go (Free (StepView v next)) = (WOView v :) <$> go next
    go (Free (StepIO io)) = io >>= go
    go (Free (StepBlock io)) = io >>= go
    go (Free Forever) = pure [WOForever]
    go (Pure a) = pure [WODone a]

main :: IO ()
main =
    defaultMain $
        testGroup
            "Widget"
            [ testDisplay
            , testIO
            , testAlternative
            , testException
            ]

testAlternative :: TestTree
testAlternative =
    testGroup
        "orr/Alternative"
        [ testCase "immediate(1)" $ do
            -- left-biased
            ops <- runWidget $ pure "a" <|> pure "b"
            ops @?= [WODone "a"]
        , testCase "immediate(2)" $ do
            ops <- runWidget $ display "a" <|> pure ()
            ops @?= [WODone ()]
        , testCase "immediate(3)" $ do
            ops <- runWidget $ pure () <|> display "a"
            ops @?= [WODone ()]
        , testCase "immediate(4)" $ do
            -- When a Widget returns immediately,
            -- should we evaluate following Widgets io effect?
            -- Currently it does, but I think it cuold be otherwise.
            ref <- IORef.newIORef (1 :: Int)
            ops <- runWidget $ orr [pure (), liftUnsafeBlockingIO (IORef.writeIORef ref 2)]
            ops @?= [WODone ()]
            val <- IORef.readIORef ref
            val @?= 2
        , testCase "zero-time" $ do
            ops <- runWidget $ display "a" <|> waitFor 0
            ops @?= [WOView "a", WODone ()]
        , testCase "zero-time'" $ do
            ops <- runWidget $ waitFor 0 <|> display "a"
            ops @?= [WOView "a", WODone ()]
        , testCase "empty" $ do
            ops <- runWidget @() $ orr []
            ops @?= [WOView "", WOForever]
        , testCase "concat" $ do
            ops <- runWidget $ display "a" <|> display "b" <|> waitFor 10
            ops @?= [WOView "ab", WODone ()]
        , testCase "nested" $ do
            ops <-
                runWidget $
                    orr
                        [ display "a"
                        , orr
                            [waitFor t1 *> display "b"]
                        , orr
                            [ waitFor t2 *> display "c"
                            , waitForever
                            ]
                        , waitFor (t1 + t2 + t1)
                        ]
            ops @?= [WOView "a", WOView "ab", WOView "abc", WODone ()]
        , testCase "race" $ do
            ops <- runWidget $ (waitFor1 $> (1 :: Int)) <|> (waitFor2 $> 2)
            ops @?= [WOView "", WODone 1]
        , testCase "race'" $ do
            ops <- runWidget $ (waitFor2 $> (1 :: Int)) <|> (waitFor1 $> 2)
            ops @?= [WOView "", WODone 2]
        , testCase "cancelling" $ do
            ref <- IORef.newIORef (1 :: Int)
            _ps <-
                runWidget $
                    orr
                        [ waitFor1
                        , liftSafeBlockingIO $ threadDelay t2 *> IORef.writeIORef ref 2
                        ]
            val0 <- IORef.readIORef ref
            val0 @?= 1
            threadDelay t2
            val1 <- IORef.readIORef ref
            val1 @?= 1
        , testCase "cancelling'" $ do
            ref <- IORef.newIORef (1 :: Int)
            _ps <-
                runWidget $
                    orr
                        [ waitFor1
                        , orr
                            [ liftSafeBlockingIO $ threadDelay t2 *> IORef.writeIORef ref 2
                            , waitForever
                            ]
                        ]
            val0 <- IORef.readIORef ref
            val0 @?= 1
            threadDelay t2
            val1 <- IORef.readIORef ref
            val1 @?= 1
        ]

-- 10000 microsecond is 0.01s.
-- これ以上に短かくすると差が小さすぎて,スレッド生成等の時間で埋まってしまい
-- 時偶あるべき順序とは逆の順序になってしまう。
t1 = 10000
t2 = 20000
waitFor1 = waitFor t1
waitFor2 = waitFor t2
waitFor n = liftSafeBlockingIO (threadDelay n)
waitForever :: MonadSafeBlockingIO f => f a
waitForever = waitFor maxBound *> waitForever

testIO :: TestTree
testIO = testCase "io" $ do
    ops0 <- runWidget $ liftSafeBlockingIO (threadDelay 10)
    ops0 @?= [WODone ()]
    ops1 <- runWidget $ liftUnsafeBlockingIO (threadDelay 10)
    ops1 @?= [WODone ()]

testDisplay :: TestTree
testDisplay = testCase "display" $ do
    ops <- runWidget (display "foo" :: Widget String Void)
    ops @?= [WOView "foo", WOForever]

data TestException = TestException
    deriving (Show, Eq)
instance Exception TestException

data TestException2 = TestException2
    deriving (Show, Eq)
instance Exception TestException2

testException :: TestTree
testException =
    testGroup
        "Exception"
        [ testNoThrow
        , testCatch
        , testCatchMiss
        , testCatchAlternative
        , testCatchAlternative'
        , testCatchEffect
        -- TODO: Currently freezes
        -- , testCatchEffect'
        ]
  where
    testNoThrow = testCase "no throw" $ do
        ops <-
            runWidget $
                catch @_ @TestException
                    (pure (1 :: Int))
                    (\_ -> pure 2)
        ops @?= [WODone 1]

    testCatch = testCase "catch" $ do
        ops <-
            runWidget $
                catch @_ @TestException
                    (throwM TestException $> (1 :: Int))
                    (\_ -> pure 2)
        ops @?= [WODone 2]

    testCatchMiss = testCase "catch miss" $ do
        ops <-
            try $
                runWidget $
                    catch @_ @TestException2
                        (throwM TestException $> (1 :: Int))
                        (\_ -> pure 2)
        ops @?= Left TestException

    testCatchAlternative = testCase "catch (<|>)" $ do
        ops <-
            runWidget $ do
                let w0 = display "a" <|> waitFor2 $> 2
                let w1 = waitFor1 *> throwM TestException $> (1 :: Int)
                catch @_ @TestException (w0 <|> w1) (\_ -> pure 2)
        ops @?= [WOView "a", WODone 2]

    testCatchAlternative' = testCase "catch (<|>)'" $ do
        ops <-
            runWidget $ do
                let w0 = waitFor1 *> throwM TestException $> (1 :: Int)
                let w1 = waitFor2 *> throwM TestException2 $> (2 :: Int)
                catch @_ @TestException (w0 <|> w1) (\_ -> pure 3)
        ops @?= [WOView "", WODone 3]

    testCatchEffect = testCase "catch from effect" $ do
        ops <-
            runWidget $
                catch @_ @TestException
                    (liftSafeBlockingIO (throwIO TestException) $> (1 :: Int))
                    (\_ -> pure 2)
        ops @?= [WODone 2]

    testCatchEffect' = testCase "catch from effect'" $ do
        ops <-
            runWidget $ do
                let w0 = waitFor1 *> liftSafeBlockingIO (throwIO TestException) $> (1 :: Int)
                let w1 = waitFor2 *> liftSafeBlockingIO (throwIO TestException2) $> (2 :: Int)
                catch @_ @TestException (w0 <|> w1) (\_ -> pure 3)
        ops @?= [WOView "", WODone 3]
