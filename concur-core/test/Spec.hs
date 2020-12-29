{-# LANGUAGE TypeApplications #-}

import Concur.Core
import Control.Applicative (Alternative ((<|>)))
import Control.Concurrent (threadDelay)
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
    go (Free (StepIO io next)) = (next <$> io) >>= go
    go (Free (StepBlock io next)) = (next <$> io) >>= go
    go (Free Forever) = pure [WOForever]
    go (Pure a) = pure [WODone a]

main :: IO ()
main =
    defaultMain $
        testGroup
            "All"
            [ testDisplay
            , testIO
            , testAlternative
            ]

testAlternative :: TestTree
testAlternative =
    testGroup
        "Alternative"
        [ testCase "immediate" $ do
            -- left-biased
            ops <- runWidget $ pure "a" <|> pure "b"
            ops @?= [WODone "a"]
        , testCase "immediate'" $ do
            ops <- runWidget $ display "a" <|> pure ()
            ops @?= [WODone ()]
        , testCase "immediate''" $ do
            ops <- runWidget $ pure () <|> display "a"
            ops @?= [WODone ()]
        , testCase "zero-time" $ do
            ops <- runWidget $ display "a" <|> waitFor 0
            ops @?= [WOView "a", WODone ()]
        , testCase "zero-time'" $ do
            ops <- runWidget $ waitFor 0 <|> display "a"
            ops @?= [WOView "a", WODone ()]
        , testCase "concat" $ do
            ops <- runWidget $ display "a" <|> display "b" <|> waitFor 10
            ops @?= [WOView "ab", WODone ()]
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
                    waitFor1
                        <|> ( waitFor2
                                *> liftSafeBlockingIO (IORef.writeIORef ref 2)
                            )
            val0 <- IORef.readIORef ref
            val0 @?= 1
            threadDelay 20000
            val1 <- IORef.readIORef ref
            val1 @?= 1
        ]
  where
    -- 10000 microsecond is 0.01s.
    -- これ以上に短かくすると差が小さすぎて,スレッド生成等の時間で埋まってしまい
    -- 時偶あるべき順序とは逆の順序になってしまう。
    waitFor1 = waitFor 10000
    waitFor2 = waitFor 20000
    waitFor n = liftSafeBlockingIO (threadDelay n)

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
