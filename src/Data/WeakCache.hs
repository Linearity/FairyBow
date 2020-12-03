{-# LANGUAGE UndecidableInstances #-}
module Data.WeakCache where

import           Control.Monad.IO.Class
import qualified Data.Map as M
import           System.Mem.Weak

class Ord (Name a) => WeakCacheValue m a where
    type Name a
    load        :: Name a -> m a

newtype WeakCache a = WeakCache (M.Map (Name a) (Weak a))

newCache :: WeakCache a
newCache = WeakCache M.empty

cacheLoad :: (WeakCacheValue m a, MonadIO m)
                => Name a -> WeakCache a -> m (a, WeakCache a)
cacheLoad a (WeakCache cache0)
    = case M.lookup a cache0 of
        Nothing     -> miss a cache0
        Just weak   -> do   cached  <- liftIO (deRefWeak weak)
                            case cached of
                                Nothing     -> miss a cache0
                                Just v      -> return (v, WeakCache cache0)
    where   miss a cache0 = do  v       <- load a
                                weak    <- liftIO (mkWeakPtr v Nothing)
                                return (v, WeakCache (M.insert a weak cache0))