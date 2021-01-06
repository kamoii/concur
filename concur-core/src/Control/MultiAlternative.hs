{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.MultiAlternative where

import Control.Applicative (Alternative, empty, (<|>))
import Control.Monad.Reader (MonadReader (ask), MonadTrans (lift), ReaderT (ReaderT, runReaderT))

class MultiAlternative f where
    never :: f a
    orr :: [f a] -> f a

instance (MultiAlternative m, Monad m) => MultiAlternative (ReaderT r m) where
    never = lift never
    orr xs = do
        r <- ask
        lift . orr $ map (flip runReaderT r) xs

{-
For reference
https://hackage.haskell.org/package/transformers-0.5.6.2/docs/src/Control.Monad.Trans.Except.html#line-202

instance (Functor m, Monad m, Monoid e) => Alternative (ExceptT e m) where
    empty = ExceptT $ return (Left mempty)
    ExceptT mx <|> ExceptT my = ExceptT $ do
        ex <- mx
        case ex of
            Left e -> liftM (either (Left . mappend e) Right) my
            Right x -> return (Right x)
-}

instance {-# OVERLAPPABLE #-} Alternative f => MultiAlternative f where
    never = empty
    orr = foldl (<|>) empty
