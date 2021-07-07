{-# LANGUAGE UndecidableInstances #-}

module Graphics.FairyBow.Blit where

import Graphics.Lightarrow.Blit
import FairyBowPlatformType
import System.FairyBow.Actuation
import System.Lightarrow.Actuation

instance ActuatePlatform (FairyBow os)
            => BlitPlatform (FairyBow os) where
    blit b c d r x = Actuation [Blit b c d r x]
