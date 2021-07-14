{-# LANGUAGE UndecidableInstances #-}
--We define how to transform actuation signals into `IO` actions that effect the actuation via GLFW.

module System.FairyBow.Actuation  where

import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.FairyBow.Bitmap
import           Data.FairyBow.Color
import           Data.FairyBow.Mesh
import           Data.FairyBow.Shading
import           Data.Lightarrow.Artifact
import           Data.Lightarrow.Color
import           Data.Lightarrow.Mesh
import           Data.Lightarrow.Shading as LA
import           Data.Map.Strict (empty, Map, traverseWithKey, unionWith)
import           Data.Typeable
import           FairyBowPlatformType
import           Graphics.GPipe hiding (Color)
import           Linear.Affine
import           System.Lightarrow.Actuation
import           System.FairyBow.Platform

instance ActuatePlatform (FairyBow os) where
    data Actuation (FairyBow os)
        = Actuation !(Map (BatchKey os) (StateT (Int, Int, Int, [Int], [Int])
                                            (ReaderT (Resources (FairyBow os), V2 Int)
                                                (FairyBow os))
                                            ()))
    actuate r (Actuation batches)
        = do    render (do  clearWindowColor win (V4 1 1 1 1)
                            clearWindowDepth win 1)
                dF <- getFrameBufferSize win
                traverseWithKey (executeBatch r dF) batches
                swapWindowBuffers win
        where   win = vrWindow (rVideo r)

instance Semigroup (Actuation (FairyBow os)) where
    Actuation batches1 <> Actuation batches2 = Actuation (unionWith (>>) batches1 batches2)

instance Monoid (Actuation (FairyBow os)) where
    mempty = Actuation empty 

data BatchKey os    =   BlitBatch Double (Bitmap (FairyBow os)) Color
                    |   DirectBatch
                    |   RasterizeBatch (EqCompiledShader os
                                            (Env os
                                                (B4 Float, B4 Float)
                                                (V4 (B4 Float), V4 (B4 Float))))
                    |   RectangleBatch Color
    deriving (Eq, Ord)

executeBatch r dF (BlitBatch z b c)
    = execBlit r dF b c
executeBatch r dF DirectBatch
    = void . flip runReaderT (r, dF) . flip runStateT (0, 0, 0, [], [])
executeBatch r dF (RasterizeBatch (EqCompiledShader (_, sC)))
    = execRasterize r dF sC
executeBatch r dF (RectangleBatch c)
    = execRectangle r dF c

execRasterize r dF sC writeVertsIndicesUniforms
            = do    let draw (aI, aV, k) = sC (Env dF aP tex (bU, k))  
                            where   aP = toPrimitiveArrayIndexed TriangleList aI aV
                        bV = vrRasterizeVBuffer (rVideo r)
                        bI = vrRasterizeIBuffer (rVideo r)
                        bU = vrRasterizeUBuffer (rVideo r)
                    (_, (_, _, _, lengthsV, lengthsI)) <- do
                        flip runReaderT (r, dF)
                            (flip runStateT (0, 0, 0, [], [])
                                writeVertsIndicesUniforms)
                    render (do   aV  <- newVertexArray bV
                                 aI  <- newIndexArray bI Nothing
                                 let  (arraysV, arraysI) = splitArrays aV aI lengthsV lengthsI
                                 mapM_ draw (zip3 arraysI arraysV [0..]))
    where  win          = vrWindow (rVideo r)
           tex          = vrDummyTexture (rVideo r)

splitArrays aV aI lengthsV lengthsI = (arraysV, arraysI)
    where   offsetsV = scanl (+) 0 (init lengthsV)
            offsetsI = scanl (+) 0 (init lengthsI)
            arraysV  = [    takeVertices n (dropVertices k aV)
                                | (n, k) <- lengthsV `zip` offsetsV     ]
            arraysI  = [    takeIndices n (dropIndices k aI)
                                | (n, k) <- lengthsI `zip` offsetsI     ]

execBlit r dF b@Bitmap { bitmapTexture = Nothing } c writeVertices
    = execBlit r dF (b { bitmapTexture = bitmapTexture (dummy r) }) c writeVertices
execBlit r dF (Bitmap _ (wB, hB) (Just t)) c writeVertices
    = do    let proj    = screenSpaceOrtho dF
                bufU    = vrBlitUBuffer (rVideo r)
                wBF     = fromIntegral wB
                hBF     = fromIntegral hB
            writeBuffer bufU 0 [(proj, convert c)]
            (_, (len, _, _, _, _)) <- do
                flip runReaderT (r, dF)
                    (flip runStateT (0, 0, 0, [], [])
                        writeVertices)
            render (do  arrayV  <- newVertexArray (vrBlitVBuffer (rVideo r))
                        let  p  = toPrimitiveArray TriangleList (takeVertices len arrayV)
                             u  = bufU
                        sC (dF, u, p, t))
    where   win     = vrWindow (rVideo r)
            sC      = vrBlitShader (rVideo r)

execRectangle r dF c writeVertices
    = do    let proj = screenSpaceOrtho dF 
                bufU = vrBlitUBuffer (rVideo r)
            writeBuffer bufU 0 [(proj, convert c)]
            (_, (len, _, _, _, _)) <- do
                flip runReaderT (r, dF)
                    (flip runStateT (0, 0, 0, [], [])
                        writeVertices)
            render (do  arrayV  <- newVertexArray (vrRectVBuffer (rVideo r))
                        let  p  = toPrimitiveArray TriangleList (takeVertices len arrayV)
                             u  = bufU
                        sC (dF, u, p))
    where   win     = vrWindow (rVideo r)
            sC      = vrFillShader (rVideo r)

screenSpaceOrtho d = ortho (-wF'/2) (wF'/2) (-hF'/2) (hF'/2) (-100000) 100000
    where V2 wF' hF' = fmap fromIntegral d 

rect (V2 w h) !r (V3 x y z) = [ (V4 x' y' z' 1, uv)
                                    |   (V2 xC yC, uv) <- zip corners texCoords,
                                        let !(V2 x' y') = fmap realToFrac (V2 x y) + vX + vY
                                            !vX = fmap realToFrac (w * xC *^ angle r)
                                            !vY = fmap realToFrac (h * yC *^ perp (angle r))
                                            !z' = realToFrac z   ]
    where   corners     = [V2 (-0.5) 0.5, V2 (-0.5) (-0.5), V2 0.5 0.5, V2 0.5 0.5, V2 (-0.5) (-0.5), V2 0.5 (-0.5)]
            texCoords   = [V2 0 0, V2 0 1, V2 1 0, V2 1 0, V2 0 1, V2 1 1]