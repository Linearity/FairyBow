{-# LANGUAGE UndecidableInstances #-}

module Graphics.FairyBow.Rasterize where

import Data.Lightarrow.Color
import FairyBowPlatformType
import Graphics.GPipe hiding (Color)
import Graphics.Lightarrow.Rasterize
import Linear.Affine
import System.FairyBow.Actuation
import System.Lightarrow.Actuation

instance ActuatePlatform (FairyBow os)
            => RasterizePlatform (Point V3 Float, Color) (M44 Float, M44 Float) (FairyBow os) where
    rasterize m s = Actuation [Rasterize m s]
