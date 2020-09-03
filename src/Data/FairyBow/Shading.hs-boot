{-# LANGUAGE UndecidableInstances #-}
module Data.FairyBow.Shading where

import Data.Lightarrow
import Graphics.GPipe hiding (Color)
import FairyBowPlatformType
import Linear.Affine
import System.Lightarrow.Load

tuView :: (a, b) -> a

tuProj :: (a, b) -> b

data Env os v u = Env { eFrameSize     :: V2 Int,
                        ePrimitives    :: PrimitiveArray Triangles v,
                        eTexture       :: Texture2D os (Format RGBAFloat),
                        eUniform       :: (Buffer os (Graphics.GPipe.Uniform u), Int) }

type UniformFunc u = V2 Int -> u

defaultUniformFunc :: UniformFunc (M44 Float, M44 Float)

data EqCompiledShader os e = EqCompiledShader (Int, CompiledShader os e)

instance ArtifactPlatform (Shading v u (FairyBow os)) (FairyBow os)
            => ShadingPlatform v u (FairyBow os)

instance Eq (EqCompiledShader os e)

instance Ord (EqCompiledShader os e)

instance Eq (Shading v (M44 Float, M44 Float) (FairyBow os))

instance Ord (Shading v (M44 Float, M44 Float) (FairyBow os))

instance Enum (Location (Shading    (Point V3 Float, Color)
                                    (M44 Float, M44 Float)
                                    (FairyBow os)))
            => LoadPlatform     (Shading    (Point V3 Float, Color)
                                            (M44 Float, M44 Float)
                                            (FairyBow os))
                                (FairyBow os)
