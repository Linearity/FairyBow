{-# LANGUAGE UndecidableInstances #-}
--We define how to transform actuation signals into `IO` actions that effect the actuation via GLFW.

module System.FairyBow.Actuation  where

import           Control.Monad.State
import           Data.FairyBow.Audio
import           Data.FairyBow.Bitmap
import           Data.FairyBow.Mesh
import           Data.FairyBow.Shading
import           Data.Lightarrow.Artifact
import           Data.Lightarrow.Color
import           Data.Lightarrow.Mesh
import           Data.Lightarrow.Shading as LA
import           Data.Map (Map, empty, foldlWithKey, insertWith)
import           Data.Maybe
import           Data.Typeable
import           FairyBowPlatformType
import           Graphics.GPipe hiding (Color)
import           Linear.Affine
import           SDL.Mixer hiding (Audio)
import           System.Lightarrow.Actuation
import           System.FairyBow.Platform

instance ActuatePlatform (FairyBow os) where
    data Actuation (FairyBow os)
                        = Actuation [Command (FairyBow os)]
    actuate r (Actuation cs) = do   render (do  clearWindowColor win (V4 1 1 1 1)
                                                clearWindowDepth win 1)
                                    batch r cs
                                    swapWindowBuffers win
        where   win = vrWindow (rVideo r)

instance Semigroup (Actuation (FairyBow os)) where
    Actuation cs1 <> Actuation cs2 = Actuation (cs1 <> cs2)

instance Monoid (Actuation (FairyBow os)) where
    mempty = Actuation []

data Command a where
    Blit        :: Bitmap a
                        -> Color
                        -> (Double, Double)
                        -> (Double, Double, Double)
                        -> Command a
    DirectOut   :: IO () -> Command a
    Rasterize   :: (Vertex v, LA.Uniform u) => Mesh v a -> Shading v u a -> Command a

data BatchKey os    =   RasterizeBatch (Shading (Point V3 Float, Color) (M44 Float, M44 Float) (FairyBow os))
                    |   BlitBatch Double (Bitmap (FairyBow os)) Color
                    |   DirectBatch
    deriving (Eq, Ord)

data BatchElement os where
    RasterizeElement    :: Mesh (Point V3 Float, Color) (FairyBow os) -> (UniformFunc (M44 Float, M44 Float)) -> BatchElement os
    BlitElement         :: (Double, Double) -> (Double, Double, Double) -> BatchElement os
    DirectElement       :: IO () -> BatchElement os

batch r cs  = foldlWithKey accumulate (return ()) table
    where   table   = foldl tabulate empty cs
            accumulate x k es
                    = x >> executeBatch r k es
            tabulate :: Map (BatchKey os) [BatchElement os]
                            -> Command (FairyBow os)
                            -> Map (BatchKey os) [BatchElement os]
            tabulate t (Rasterize   (m :: Mesh v (FairyBow os))
                                    (s :: Shading v u (FairyBow os)))
                | Just Refl  <- eqT :: Maybe (u :~: (M44 Float, M44 Float)),
                  Just Refl  <- eqT :: Maybe (v :~: (Point V3 Float, Color))
                    = let Shading (sC, f) = s
                        in insertWith (++) (RasterizeBatch s) [RasterizeElement m f] t
                | otherwise
                    = t
            tabulate t (Blit b c (w, h) (x, y, z))
                    = insertWith (++) (BlitBatch z b c) [BlitElement (w, h) (x, y, z)] t
            tabulate t (DirectOut a)
                    = insertWith (++) DirectBatch [DirectElement a] t

executeBatch r (RasterizeBatch s) es
    = execRasterize r s [(m, f) | RasterizeElement m f <- es]
executeBatch r (BlitBatch z b c) es
    = execBlit r b c [(s, x) | BlitElement s x <- es]
executeBatch r DirectBatch es
    = sequence_ [liftIO a | DirectElement a <- es]

execRasterize r s mfs
            = do    V2 w h      <- getFrameBufferSize win
                    bU          <- newBuffer (length ms)
                    let  draw (aI, aV, k) = sC (Env (V2 w h) aP tex (bU, k))  
                             where   aP = toPrimitiveArrayIndexed TriangleList aI aV
                    writeBuffer bU 0 (map ($ (V2 w h)) fs)
                    let     bs = map (let f m = fromMaybe (f (dummy r)) (meshBuffers m)
                                        in f) ms
                    (bV, bI)    <- joinBuffers bs
                    render (do   aV  <- newVertexArray bV
                                 aI  <- newIndexArray bI Nothing
                                 let  (arraysV, arraysI) = splitArrays aV aI bs
                                 mapM_ draw (zip3 arraysI arraysV [0..]))
    where  (ms, fs)         = unzip mfs
           Shading (EqCompiledShader (k, sC), f)  = s
           win              = vrWindow (rVideo r)
           tex              = vrDummyTexture (rVideo r)

joinBuffers [b] = return b
joinBuffers bs  = do    bV  <- newBuffer nV
                        bI  <- newBuffer nI
                        runStateT (traverse (catB bV) bvs) 0
                        runStateT (traverse (catB bI) bis) 0
                        return (bV, bI)
    where   bvs         = map fst bs
            bis         = map snd bs
            catB t f    = do    k0 <- get
                                lift (copyBuffer f 0 t k0 (bufferLength f))
                                put (k0 + bufferLength f)
            nV          = sum (map bufferLength bvs)
            nI          = sum (map bufferLength bis)

splitArrays aV aI bs    = (arraysV, arraysI)
    where   lengthsV = map (bufferLength . fst) bs
            offsetsV = scanl (+) 0 (init lengthsV)
            lengthsI = map (bufferLength . snd) bs
            offsetsI = scanl (+) 0 (init lengthsI)
            arraysV  = [takeVertices n (dropVertices k aV) | (n, k) <- lengthsV `zip` offsetsV]
            arraysI  = [takeIndices n (dropIndices k aI) | (n, k) <- lengthsI `zip` offsetsI]

execBlit r b@(Bitmap { bitmapTexture = Nothing }) c sxs
    = execBlit r (b { bitmapTexture = bitmapTexture (dummy r) }) c sxs
--execBlit r (Bitmap _ _ (wB, hB) (Just t)) _ sxs
execBlit r (Bitmap _ (wB, hB) (Just t)) _ sxs
    = do   V2 wF hF     <- getFrameBufferSize win
           let view        = identity
               V2 wF' hF'  = fmap fromIntegral (V2 wF hF)
               proj        = ortho (-wF'/2) (wF'/2) (-hF'/2) (hF'/2) (-100000) 100000
           bufU         <- newBuffer 1
           writeBuffer bufU 0 [(view, proj)]
           bufV :: Buffer os (B4 Float, B2 Float) <- newBuffer (6 * length sxs)
           let  vs         = concat [zip    (pos    (realToFrac w * fromIntegral wB)
                                                    (realToFrac h * fromIntegral hB)
                                                    x
                                                    y
                                                    z)
                                            uv
                                        | ((w, h), (x, y, z)) <- sxs]
           writeBuffer bufV 0 vs
           render (do  arrayV  <- newVertexArray bufV
                       let  p  = toPrimitiveArray TriangleList arrayV
                            u  = bufU
                       sC (V2 wF hF, u, p, t))
    where  win             = vrWindow (rVideo r)
           pos w h x y z   = [V4 (realToFrac x + w * u) (realToFrac y + v * h) (realToFrac z) 1
                                | V2 u v <- corners]
           corners         = [V2 0 1, V2 0 0, V2 1 1, V2 1 1, V2 0 0, V2 1 0]
           uv              = [V2 0 0, V2 0 1, V2 1 0, V2 1 0, V2 0 1, V2 1 1]
           sC              = vrBlitShader (rVideo r)

instance Eq (Command a) where
    _ == _  = False

instance Ord (Command a) where
    Blit _ _ _ (_, _,z1)  `compare`   Blit _ _ _ (_, _, z2) = compare z1 z2
    Blit _ _ _ _          `compare`   Rasterize _ _       = LT
    Rasterize _ _       `compare`   DirectOut _         = LT
    Rasterize _ _       `compare`   Blit _ _ _ _          = GT
    DirectOut _         `compare`   Rasterize _ _       = GT
    _                   `compare`   _                   = EQ
