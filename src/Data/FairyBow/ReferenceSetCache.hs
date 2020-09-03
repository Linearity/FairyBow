{-# LANGUAGE UndecidableInstances #-}
module Data.ReferenceSetCache where

import           Control.Exception
import           Control.Monad.Trans
import qualified Data.ByteString as B
import           Data.IORef
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Vector.Storable as SV ((!))
import qualified Data.Vector.Storable as SV
import           Data.Word

class (Ord a, Applicative (Load a)) => ReferenceName a where
    data Reference a
    type ReferenceValue a
    type ReferenceSource a
    type Load a :: * -> *
    newRef      :: Int -> a -> ReferenceValue a -> Reference a
    name        :: Reference a -> a
    key         :: Reference a -> Int
    deref       :: Reference a -> ReferenceValue a
    load        :: a -> ReferenceSource a -> Load a b

data ReferenceSetCache a b
    = ReferenceSetCache (M.Map a (b, IS.IntSet, Int)) (ReferenceSource a)

newCache :: ReferenceName a
                => ReferenceSource a -> ReferenceSetCache a (ReferenceValue a)
newCache s = ReferenceSetCache M.empty s

newReference :: ReferenceName a
                    => a    -> ReferenceSetCache a (ReferenceValue a)
                            -> Load a (Reference a, ReferenceSetCache a (ReferenceValue a))
newReference a (ReferenceSetCache cache0 source)
    = case M.lookup a cache0 of
        Just (b, keys, next)    -> let  entry   = (b, IS.insert next keys, succ next)
                                        cache1  = M.insert a entry cache0
                                    in pure (newRef next a b, ReferenceSetCache cache1 source)
        Nothing                 -> (\b -> let   key     = 0
                                                next    = succ key
                                                entry   = (b, IS.empty, next)
                                                cache1  = M.insert a entry cache0
                                            in (newRef key a b, ReferenceSetCache cache1 source))
                                        <$> load a source

closeReference :: ReferenceName a
                    => Reference a -> ReferenceSetCache a b -> ReferenceSetCache a b
closeReference r (ReferenceSetCache cache0 source)
    = case M.lookup (name r) cache0 of
        Nothing                 -> ReferenceSetCache cache0 source
        Just (b, keys0, next)   -> let  keys1   = IS.delete (key r) keys0
                                        cache1  = if IS.null keys1
                                                    then M.delete (name r) cache0
                                                    else M.insert (name r) (b, keys1, next) cache0
                                    in ReferenceSetCache cache1 source
