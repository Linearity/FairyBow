{-# LANGUAGE UndecidableInstances #-}

module Data.FairyBow.Mesh where

import qualified Codec.Wavefront as WF
import           Data.Lightarrow.Artifact
import           Data.Lightarrow.Color
import           Data.FairyBow.Color as Color
import           Data.IORef
import           Data.Typeable
import qualified Data.Vector as V
import           Data.WeakCache
import           Data.Word
import           Data.Lightarrow.Mesh
import           FairyBowPlatformType
import           Graphics.GPipe hiding (Color)
import           Graphics.GPipe.Context.GLFW
import           Linear.Affine
import {-# SOURCE #-} System.FairyBow.Platform
import           System.Lightarrow.Load
import           System.IO                      ( hFlush
                                                , stdout
                                                )
--import           System.Lightarrow.Platform
import           Control.Monad.IO.Class

class Vertex a => FairyBowVertex a where
    type HostAttribute a
    type BufferAttribute a
    attrib :: a -> HostAttribute a

instance Typeable a => Vertex (Point V3 a) where
    -- empty class

instance (Num a, Typeable a) => FairyBowVertex (Point V3 a) where
    type HostAttribute (Point V3 a) = V4 a
    type BufferAttribute (Point V3 a) = B4 a
    attrib (P x) = point x

instance Vertex (Point V3 Float, Color) where
    -- empty class

instance FairyBowVertex (Point V3 Float, Color) where
    type HostAttribute (Point V3 Float, Color) = (V4 Float, V4 Float)
    type BufferAttribute (Point V3 Float, Color) = (B4 Float, B4 Float)
    attrib (P x, c) = (point x, Color.convert c)

instance ArtifactPlatform   (Mesh (Point V3 Float, Color) (FairyBow os))
                            (FairyBow os)     where
    dummy r = Mesh "dummy" [] [] (Just (bufV, bufI))
        where   bufV    = vrDummyVBuffer (rVideo r)
                bufI    = vrDummyIBuffer (rVideo r)

instance (  ArtifactPlatform (Mesh v (FairyBow os)) (FairyBow os),
            FairyBowVertex v    )
                => MeshPlatform v (FairyBow os) where
    data Mesh v (FairyBow os)
        = Mesh {    meshPath        :: FilePath,
                    meshVertices    :: [v],
                    meshFaces       :: [Face],
                    meshBuffers     :: Maybe (  Buffer os (BufferAttribute v),
                                                Buffer os (B Word32)    )   }
                
    --fromTris ts r = Mesh "fromTris" vs fs  ()
    --    where   vs0     = concat [[v1, v2, v3] | (v1, v2, v3) <- ts]
    --            table   = foldl' (\t (v, k) -> M.insertWith (const id) v k t)
    --                        M.empty (zip vs0 [0 ..])
    --            vs      = nub vs0
    --            fs      = [Triangle     (fromJust (M.lookup a table))
    --                                    (fromJust (M.lookup b table))
    --                                    (fromJust (M.lookup c table)) | (a, b, c) <- ts]

instance (Eq v) => Eq (Mesh v (FairyBow os)) where
    Mesh _ vs1 fs1 _ == Mesh _ vs2 fs2 _ = vs1 == vs2 && fs1 == fs2

data Face = Triangle Int Int Int
    deriving (Eq, Ord)

indices :: Face -> [Int]
indices (Triangle a b c) = [a, b, c]

--convert ::  Vertex v
--            =>  MeshFB os v Face
--                -> ([HostAttribute v], [Word32])
--convert (MeshFB vs fs _ _) = (map attrib vs, concatMap (map fromIntegral . indices) fs)

instance WeakCacheValue (FairyBow os) (Mesh (Point V3 Float, Color) (FairyBow os)) where
    type Name (Mesh (Point V3 Float, Color) (FairyBow os)) = FilePath
    load p = do     objOrMsg    <- liftIO (WF.fromFile p)
                    liftIO (do  putStr ("Load OBJ from " ++ show p ++ "...")
                                hFlush stdout)
                    case objOrMsg of
                        Left _  -> do   liftIO (putStrLn ("failed."))
                                        return (Mesh p [] [] Nothing)
                        Right object
                            -> do   liftIO (putStrLn ("done."))
                                    let     vs      = zip (V.toList (V.map convertV ls)) colors
                                            is      = concatMap indices fs
                                            colors  = repeat Black
                                            fs      = V.toList (V.map (convertF . WF.elValue) ofs)
                                            convertV (WF.Location x y z _)
                                                = P (V3 x y z)
                                            convertF (WF.Face a b c r)
                                                = Triangle  (WF.faceLocIndex a - 1)
                                                            (WF.faceLocIndex b - 1)
                                                            (WF.faceLocIndex c - 1)
                                            ls      = WF.objLocations object
                                            ps      = WF.objPoints object
                                            ofs     = WF.objFaces object
                                    bufV        <- newBuffer (length vs)
                                                    :: ContextT Handle os IO (Buffer os (B4 Float, B4 Float))
                                    bufI        <- newBuffer (length is)
                                                    :: ContextT Handle os IO (Buffer os (B Word32))
                                    liftIO (do  putStr ("Write " ++ show (length vs)
                                                                        ++ " vertices to buffer...")
                                                hFlush stdout)
                                    writeBuffer bufV 0 (map attrib vs)
                                    liftIO (putStrLn "done.")
                                    liftIO (do  putStr ("Write " ++ show (length is)
                                                                        ++ " indices to buffer...")
                                                hFlush stdout)
                                    writeBuffer bufI 0 (map fromIntegral is)
                                    liftIO (putStrLn "done.")
                                    return (Mesh p vs fs (Just (bufV, bufI)))

instance (  FileLocation (Location (Mesh (Point V3 Float, Color) (FairyBow os))),
            WeakCacheValue (FairyBow os) (Mesh (Point V3 Float, Color) (FairyBow os))   )
                => LoadPlatform (Mesh (Point V3 Float, Color) (FairyBow os)) (FairyBow os) where
    request r l
        = return (Mesh (toPath l) [] [] Nothing)
    load r m@(Mesh p _ _ (Just _))
        = return m
    load r (Mesh p _ _ Nothing)
        = do    let cacheRef = lrMeshCache (rLoading r)
                cache0          <- liftIO (readIORef cacheRef)
                (m, cache1)     <- cacheLoad p cache0
                liftIO (writeIORef cacheRef cache1)
                return m
    unload _ (Mesh p vs fs _) = return (Mesh p [] [] Nothing)
