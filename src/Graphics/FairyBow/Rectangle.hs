{-# LANGUAGE UndecidableInstances #-}

module Graphics.FairyBow.Rectangle where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Map.Strict (singleton)
import Graphics.Lightarrow.Rectangle
import Graphics.GPipe
import FairyBowPlatformType
import System.FairyBow.Actuation
import System.FairyBow.Platform
import System.Lightarrow.Actuation

instance ActuatePlatform (FairyBow os)
            => RectanglePlatform (FairyBow os) where
    rectangle c s@(V2 w h) pos
        = Actuation (singleton (RectangleBatch c) action)
        where   action = do     buf <- asks (vrRectVBuffer . rVideo . fst)
                                (start, s2, s3, s4, s5) <- get
                                lift (lift (writeBuffer buf start vs))
                                put (start + 6, s2, s3, s4, s5)
                vs = [ x | (x, _uv) <- rect s 0 (pos + V3 (w/2) (h/2) 0) ]