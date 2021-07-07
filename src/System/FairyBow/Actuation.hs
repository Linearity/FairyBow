{-# LANGUAGE UndecidableInstances #-}
--We define how to transform actuation signals into `IO` actions that effect the actuation via GLFW.

module System.FairyBow.Actuation  where

import           Control.Monad.State
import           Data.FairyBow.Bitmap
import           Data.FairyBow.Color
import           Data.FairyBow.Mesh
import           Data.FairyBow.Shading
import           Data.Lightarrow.Artifact
import           Data.Lightarrow.Color
import           Data.Lightarrow.Mesh
import           Data.Lightarrow.Shading as LA
import           Data.Map (Map, empty, foldlWithKey, insertWith)
import           Data.Typeable
import           FairyBowPlatformType
import           Graphics.GPipe hiding (Color)
import           Linear.Affine
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
                        -> Double
                        -> (Double, Double, Double)
                        -> Command a
    DirectOut   :: IO () -> Command a
    Rasterize   :: (Vertex v, LA.Uniform u) =>
                        Mesh v a
                            -> Shading v u a
                            -> Command a
    Rectangle   :: Color
                        -> (Double, Double)
                        -> (Double, Double, Double)
                        -> Command a

data BatchKey os    =   BlitBatch Double (Bitmap (FairyBow os)) Color
                    |   DirectBatch
                    |   RasterizeBatch (EqCompiledShader os
                                            (Env os
                                                (B4 Float, B4 Float)
                                                (V4 (B4 Float), V4 (B4 Float))))
                    |   RectangleBatch Color
    deriving (Eq, Ord)

data BatchElement os where
    BlitElement         :: (Double, Double)
                                -> Double
                                -> (Double, Double, Double)
                                -> BatchElement os
    DirectElement       :: IO () -> BatchElement os
    RasterizeElement    :: Mesh (Point V3 Float, Color) (FairyBow os)
                                -> UniformFunc (M44 Float, M44 Float)
                                -> BatchElement os
    RectangleElement    :: (Double, Double)
                                -> (Double, Double, Double)
                                -> BatchElement os

batch r cs  = foldlWithKey accumulate (return ()) table
    where   table   = foldl tabulate empty cs
            accumulate x k es
                    = x >> executeBatch r k es
            tabulate :: Map (BatchKey os) [BatchElement os]
                            -> Command (FairyBow os)
                            -> Map (BatchKey os) [BatchElement os]
            tabulate t (Blit b c (w, h) r (x, y, z))
                    = insertWith    (++)
                                    (BlitBatch z b c)
                                    [BlitElement (w, h) r (x, y, z)]
                                    t
            tabulate t (DirectOut a)
                    = insertWith    (++)
                                    DirectBatch
                                    [DirectElement a]
                                    t
            tabulate t (Rasterize   (m :: Mesh v (FairyBow os))
                                    (s :: Shading v u (FairyBow os)))
                | Just Refl  <- eqT :: Maybe (u :~: (M44 Float, M44 Float)),
                  Just Refl  <- eqT :: Maybe (v :~: (Point V3 Float, Color))
                    = let Shading (eqsC, f) = s
                        in insertWith   (++)
                                        (RasterizeBatch eqsC)
                                        [RasterizeElement m f]
                                        t
                | otherwise
                    = t
            tabulate t (Rectangle c (w, h) (x, y, z))
                    = insertWith    (++)
                                    (RectangleBatch c)
                                    [RectangleElement (w, h) (x, y, z)]
                                    t

executeBatch r (BlitBatch z b c) es
    = execBlit r b c [(s, r, x) | BlitElement s r x <- es]
executeBatch r DirectBatch es
    = sequence_ [liftIO a | DirectElement a <- es]
executeBatch r (RasterizeBatch (EqCompiledShader (_, sC))) es
    = execRasterize r sC [(m, f) | RasterizeElement m f <- es]
executeBatch r (RectangleBatch c) es
    = execRectangle r c [(d, x) | RectangleElement d x <- es]

execRasterize r sC mfs
            = do    V2 w h      <- getFrameBufferSize win
                    bU          <- newBuffer (length ms)
                    let  draw (aI, aV, k) = sC (Env (V2 w h) aP tex (bU, k))  
                             where   aP = toPrimitiveArrayIndexed TriangleList aI aV
                    writeBuffer bU 0 (map ($ V2 w h) fs)
                    bs          <- mapM (getBuffers r) ms
                    (bV, bI)    <- joinBuffers bs
                    render (do   aV  <- newVertexArray bV
                                 aI  <- newIndexArray bI Nothing
                                 let  (arraysV, arraysI) = splitArrays aV aI bs
                                 mapM_ draw (zip3 arraysI arraysV [0..]))
    where  (ms, fs)     = unzip mfs
           win          = vrWindow (rVideo r)
           tex          = vrDummyTexture (rVideo r)
{-

We extract from a mesh its vertex and index buffers with a separate action
called |getBuffers|.  This allows us to create new buffers at the last minute
for meshes with vertices and faces but no buffers.  We could, rather than
creating new buffers each frame for each such mesh, keep large buffers available
for these last-minute entries instead.  This is left to the reader as an exercise.

-}
getBuffers r (Mesh _p vs fs mbs)
    | Just bs <- mbs            = return bs
    | not (null vs || null fs)  = do    let is = concatMap indices fs
                                        bV  <- newBuffer (length vs)
                                        bI  <- newBuffer (length is)
                                        writeBuffer bV 0 (map attrib vs)
                                        writeBuffer bI 0 (map fromIntegral is)
                                        return (bV, bI)
    | otherwise                 = return (  vrDummyVBuffer (rVideo r),
                                            vrDummyIBuffer (rVideo r)   )

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
            arraysV  = [    takeVertices n (dropVertices k aV)
                                | (n, k) <- lengthsV `zip` offsetsV     ]
            arraysI  = [    takeIndices n (dropIndices k aI)
                                | (n, k) <- lengthsI `zip` offsetsI     ]

execBlit r b@Bitmap { bitmapTexture = Nothing } c srxs
    = execBlit r (b { bitmapTexture = bitmapTexture (dummy r) }) c srxs
execBlit r (Bitmap _ (wB, hB) (Just t)) c srxs
    = do    dF          <- getFrameBufferSize win
            let proj    = screenSpaceOrtho dF
            bufU        <- singleUniform (proj, convert c)
            bufV :: Buffer os (B4 Float, B2 Float) <- newBuffer (6 * length srxs)
            let wBF     = fromIntegral wB
                hBF     = fromIntegral hB 
                vs      = concat [ rect (w * wBF, h * hBF) r x | ((w, h), r, x) <- srxs ]
            writeBuffer bufV 0 vs
            render (do  arrayV  <- newVertexArray bufV
                        let  p  = toPrimitiveArray TriangleList arrayV
                             u  = bufU
                        sC (dF, u, p, t))
    where   win     = vrWindow (rVideo r)
            sC      = vrBlitShader (rVideo r)

execRectangle r c dxs
    = do    dF          <- getFrameBufferSize win
            let proj    = screenSpaceOrtho dF 
            bufU        <- singleUniform (proj, convert c)
            bufV :: Buffer os (B4 Float) <- newBuffer (6 * length dxs)
            let vs      = concat [ [ x | (x, _uv) <- rect (w,h) 0 (x + w/2, y + h/2, z) ] | ((w,h), (x,y,z)) <- dxs ]
            writeBuffer bufV 0 vs
            render (do  arrayV  <- newVertexArray bufV
                        let  p  = toPrimitiveArray TriangleList arrayV
                             u  = bufU
                        sC (dF, u, p))
    where   win     = vrWindow (rVideo r)
            sC      = vrFillShader (rVideo r)

screenSpaceOrtho d = ortho (-wF'/2) (wF'/2) (-hF'/2) (hF'/2) (-100000) 100000
    where V2 wF' hF' = fmap fromIntegral d 

singleUniform x = do    bufU <- newBuffer 1
                        writeBuffer bufU 0 [x]
                        return bufU

rect (w, h) r (x, y, z) = [     (V4 x' y' z' 1, uv)
                                    |   (V2 xC yC, uv) <- zip corners texCoords,
                                        let V2 x' y' = fmap realToFrac (V2 x y) + vX + vY
                                            vX  = fmap realToFrac (w * xC *^ angle r)
                                            vY  = fmap realToFrac (h * yC *^ perp (angle r))
                                            --x'  = realToFrac (x + w * xC)
                                            --y'  = realToFrac (y + h * yC)
                                            z'  = realToFrac z   ]
    -- where   corners     = [V2 0 1, V2 0 0, V2 1 1, V2 1 1, V2 0 0, V2 1 0]
    where   corners     = [V2 (-0.5) 0.5, V2 (-0.5) (-0.5), V2 0.5 0.5, V2 0.5 0.5, V2 (-0.5) (-0.5), V2 0.5 (-0.5)]
            texCoords   = [V2 0 0, V2 0 1, V2 1 0, V2 1 0, V2 0 1, V2 1 1]

instance Eq (Command a) where
    _ == _  = False

instance Ord (Command a) where
    Blit _ _ _ _ (_,_,z1)       `compare`   Blit _ _ _ _ (_,_,z2)       = compare z1 z2
    Blit {}                     `compare`   Rasterize _ _               = LT
    Rasterize _ _               `compare`   DirectOut _                 = LT
    Rasterize _ _               `compare`   Blit {}                     = GT
    Rectangle _ _ (_,_,z1)      `compare`   Rectangle _ _ (_,_,z2)      = compare z1 z2
    DirectOut _                 `compare`   Rasterize _ _               = GT
    _                           `compare`   _                           = EQ