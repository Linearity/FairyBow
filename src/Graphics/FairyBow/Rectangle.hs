{-# LANGUAGE UndecidableInstances #-}

module Graphics.FairyBow.Rectangle where

import Graphics.Lightarrow.Rectangle
import FairyBowPlatformType
import System.FairyBow.Actuation
import System.Lightarrow.Actuation

instance ActuatePlatform (FairyBow os)
            => RectanglePlatform (FairyBow os) where
    rectangle c d x = Actuation [Rectangle c d x]