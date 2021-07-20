{-# LANGUAGE UndecidableInstances #-}

module Graphics.FairyBow.Rasterize where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.FairyBow.Mesh
import Data.FairyBow.Shading
import Data.Lightarrow.Color
import Data.Map.Strict (singleton)
import FairyBowPlatformType
import Graphics.GPipe hiding (Color)
import Graphics.Lightarrow.Rasterize
import Linear.Affine
import System.FairyBow.Actuation
import System.FairyBow.Platform
import System.Lightarrow.Actuation

instance ActuatePlatform (FairyBow os)
            => RasterizePlatform (Point V3 Float, Color) (M44 Float, M44 Float) (FairyBow os) where
    rasterize (Mesh _ _vs _fs mbs) (Shading (eqsC, f))
        = Actuation (singleton (RasterizeBatch eqsC) action)
        where
            action = do     toBufV <- asks (vrRasterizeVBuffer . rVideo . fst)
                            toBufI <- asks (vrRasterizeIBuffer . rVideo . fst)
                            bufU <- asks (vrRasterizeUBuffer . rVideo . fst)
                            dF <- asks snd
                            (fromBufV, fromBufI) <- case mbs of
                                Just bs -> return bs
                                Nothing -> do   bufV <- asks (vrDummyVBuffer . rVideo . fst)
                                                bufI <- asks (vrDummyIBuffer . rVideo . fst)
                                                return (bufV, bufI)
                            (startV, startI, startU, lengthsV, lengthsI) <- get
                            let sizeV = bufferLength fromBufV
                                sizeI = bufferLength fromBufI
                            lift $
                                lift $
                                    do  copyBuffer fromBufV 0 toBufV startV sizeV
                                        copyBuffer fromBufI 0 toBufI startI sizeI
                                        writeBuffer bufU startU [f dF]
                            put ( startV + sizeV
                                , startI + sizeI
                                , startU + 1
                                , sizeV : lengthsV
                                , sizeI : lengthsI
                                )