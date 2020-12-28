import Concur.Core
import Control.Monad.Free (Free (Free, Pure))
import Data.Void (Void)
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))
import Control.Concurrent (threadDelay)

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
            ]

testIO = testCase "io" $ do
    ops <- runWidget $ liftSafeBlockingIO (threadDelay 10)
    ops @?= [WODone ()]
    ops <- runWidget $ liftUnsafeBlockingIO (threadDelay 10)
    ops @?= [WODone ()]

testDisplay = testCase "display" $ do
    ops <- runWidget (display "foo" :: Widget String Void)
    ops @?= [WOView "foo", WOForever]
