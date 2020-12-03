{-# LANGUAGE UndecidableInstances #-}

module Data.FairyBow.Shading where

import Data.FairyBow.Mesh
import Data.Lightarrow as LA
import FairyBowPlatformType
import Graphics.GPipe hiding (Color)
import Linear
import Linear.Affine
import Shaders
import System.FairyBow.Platform
import System.Lightarrow.Load

coerceMatrix = fmap (fmap (fromRational . toRational))

instance LA.Uniform (M44 Float, M44 Float) where
    mapView f (v, p) = (coerceMatrix (f (coerceMatrix v)), p)
    mapProj f (v, p) = (v, coerceMatrix (f (coerceMatrix p)))

tuView :: (a, b) -> a
tuView (v, _) = v

tuProj :: (a, b) -> b
tuProj (_, p) = p

data Env os v u = Env { eFrameSize     :: V2 Int,
                        ePrimitives    :: PrimitiveArray Triangles v,
                        eTexture       :: Texture2D os (Format RGBAFloat),
                        eUniform       :: (Buffer os (Graphics.GPipe.Uniform u), Int) }

type UniformFunc u = V2 Int -> u


defaultUniformFunc :: UniformFunc (M44 Float, M44 Float)
defaultUniformFunc d = (Linear.identity, Linear.ortho (-w/2) (w/2) (-h/2) (h/2) (-10000) 10000)
    where   V2 w h = fmap fromIntegral d

newtype EqCompiledShader os e = EqCompiledShader (Int, CompiledShader os e)

instance ArtifactPlatform   (Shading    (Point V3 Float, Color)
                                        (M44 Float, M44 Float)
                                        (FairyBow os))
                            (FairyBow os)     where
    dummy r = Shading (EqCompiledShader (0, sC), defaultUniformFunc)
        where   sC = vrDummyShader (rVideo r)

instance ArtifactPlatform (Shading v u (FairyBow os)) (FairyBow os)
            => ShadingPlatform v u (FairyBow os)  where
    data Shading v u (FairyBow os)
                        = Shading (EqCompiledShader os
                                    (Env os
                                        (BufferAttribute v)
                                        (V4 (B4 Float), V4 (B4 Float))),
                                    UniformFunc u )
    mapUniform f (Shading (s, g)) = Shading (s, f . g)

instance Eq (EqCompiledShader os e) where
    EqCompiledShader (k1, _) == EqCompiledShader (k2, _) = k1 == k2

instance Ord (EqCompiledShader os e) where
    EqCompiledShader (k1, _) <= EqCompiledShader (k2, _) = k1 <= k2

instance Eq (Shading v (M44 Float, M44 Float) (FairyBow os)) where
    Shading (s1, _) == Shading (s2, _) = s1 == s2

instance Ord (Shading v (M44 Float, M44 Float) (FairyBow os)) where
    Shading (s1, _) <= Shading (s2, _) = s1 <= s2

instance Enum (Location (Shading (Point V3 Float, Color) (M44 Float, M44 Float) (FairyBow os)))
            => LoadPlatform     (Shading    (Point V3 Float, Color)
                                            (M44 Float, M44 Float)
                                            (FairyBow os))
                                (FairyBow os)     where
    request r l     = do    if k >= 0 && k < length vertexColorShadings
                            then do  let (s, f) = (vertexColorShadings !! k) (vrWindow (rVideo r))
                                     sC <- compileShader s
                                     return (Shading (EqCompiledShader (k, sC), f))
                            else return (dummy r)
        where k = fromEnum l
    load _ d        = return d
    unload _ d      = return d
