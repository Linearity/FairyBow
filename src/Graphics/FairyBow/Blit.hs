{-# LANGUAGE UndecidableInstances #-}

module Graphics.FairyBow.Blit where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Lightarrow.Bitmap (dimensions)
import Data.Map.Strict (singleton)
import Graphics.Lightarrow.Blit
import Graphics.GPipe
import FairyBowPlatformType
import System.FairyBow.Actuation
import System.FairyBow.Platform
import System.Lightarrow.Actuation

instance ActuatePlatform (FairyBow os)
            => BlitPlatform (FairyBow os) where
    blit b c s r x@(V3 _ _ z)
        = Actuation (singleton (BlitBatch z b c) action)
        where   action = do     buf <- asks (vrBlitVBuffer . rVideo . fst)
                                (start, s2, s3, s4, s5) <- get
                                lift (lift (writeBuffer buf start vs))
                                put (start + 6, s2, s3, s4, s5)
                vs = rect (V2 (fromIntegral wB) (fromIntegral hB) * s) r x
                (wB, hB) = dimensions b
