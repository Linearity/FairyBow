module System.FairyBow.Platform where

import           Data.IORef
import {-# SOURCE #-} Data.FairyBow.Shading
import           Data.Lightarrow.Bitmap
import           Data.Lightarrow.Color
import           Data.Lightarrow.Mesh
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Time.Clock.System
import           Data.WeakCache
import           Data.Word
import           FairyBowPlatformType
import           Graphics.GPipe hiding (Color)
import           Graphics.GPipe.Context.GLFW
import           Linear.Affine
import           System.Lightarrow.Platform
import           System.Lightarrow.Timing

instance Platform (FairyBow os)

rInput :: Resources (FairyBow os) -> InputResources
rLoading :: Resources (FairyBow os) -> LoadingResources os
rTiming :: Resources (FairyBow os) -> TimingResources
rVideo :: Resources (FairyBow os) -> VideoResources os

data InputResources
    = InputResources {  irKeyTable      :: IORef (S.Set Key),
                        irMouseTable    :: IORef (S.Set MouseButton)    }

data VideoResources os
    = VideoResources {  vrBlitShader    :: CompiledShader os
                                            (   V2 Int,
                                                Buffer os (Uniform (    V4 (B4 Float),
                                                                        V4 (B4 Float)   )),
                                                PrimitiveArray Triangles (B4 Float, B2 Float),
                                                Texture2D os (Format RGBAFloat)     ),
                        vrDummyVBuffer  :: Buffer os (B4 Float, B4 Float),
                        vrDummyIBuffer  :: Buffer os (B Word32),
                        vrDummyShader   :: CompiledShader os
                                                (Env    os
                                                        (B4 Float, B4 Float)
                                                        (V4 (B4 Float), V4 (B4 Float))),
                        vrDummyTexture  :: Texture2D os (Format RGBAFloat),
                        vrWindow        :: Window os RGBAFloat Depth    }

data TimingResources
    = TimingResources {     trTimestamps    :: IORef (M.Map TimeGroup [SystemTime])   }

data LoadingResources os
    = LoadingResources {    lrBitmapCache   :: IORef (WeakCache (Bitmap (FairyBow os))),
                            lrMeshCache     :: IORef (WeakCache (Mesh (Point V3 Float, Color) (FairyBow os)))   }
