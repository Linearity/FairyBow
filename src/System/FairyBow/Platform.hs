{-# LANGUAGE UndecidableInstances #-}
module System.FairyBow.Platform where

import           Control.Monad.IO.Class
import           Data.IORef
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.FairyBow.Audio
import           Data.FairyBow.Bitmap
import           Data.FairyBow.Mesh
import {-# SOURCE #-} Data.FairyBow.Shading
import           Data.Lightarrow.Color
import qualified Data.Text as T
import           Data.Time.Clock.System
import           Data.WeakCache
import           Data.Word
import           FairyBowPlatformType
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           Graphics.GPipe hiding (Color)
import           Graphics.GPipe.Context.GLFW as GLFW
import           Linear.Affine
import           SDL.Mixer hiding (Format, Audio)
import qualified SDL.Mixer (Audio(..))
import           SDL.Raw.Mixer (Chunk(..))
import           System.Lightarrow.Platform
import           System.Lightarrow.Timing
import           System.Random.TF
import           Shaders.Blit
import           Shaders.Fill
import           Shaders.ColorDepthOnScreen

instance Platform (FairyBow os) where
    data Resources (FairyBow os)
            = Resources {   rAudio      :: AudioResources,
                            rLoading    :: LoadingResources os,
                            rInput      :: InputResources,
                            rRandom     :: RandomResources,
                            rTiming     :: TimingResources,
                            rVideo      :: VideoResources os     }
    setup = do  t               <- liftIO getSystemTime
                tR              <- liftIO (newIORef M.empty)
                w               <- newWindow (WindowFormatColorDepth RGBA8 Depth32)
                                      ((defaultWindowConfig "You found the Fairy Bow!")
                                        {   configWidth     = 160 * 4,
                                            configHeight    = 144 * 4   })
                liftIO (openAudio (SDL.Mixer.Audio {    audioFrequency = 44100,
                                                        audioFormat = FormatS16_Sys,
                                                        audioOutput = Stereo            })
                                  1024)
                kR              <- liftIO (newIORef S.empty)
                mR              <- liftIO (newIORef S.empty)
                rR              <- liftIO (newTFGen >>= newIORef)
                acR             <- liftIO (newIORef newCache)
                bcR             <- liftIO (newIORef newCache)
                mcR             <- liftIO (newIORef newCache)
                tcR             <- liftIO (newIORef newCache)
                sB              <- compileShader (blit w)
                sF              <- compileShader (fill w)
                (bVB, bVR, bVRa, bIRa, bURa) <- newBatchBuffers
                bUB             <- newBuffer 1
                (bVD, bID)      <- newDummyBuffers
                t               <- newDummyTexture
                c               <- newDummyChunk
                sD              <- compileShader (fst (colorDepthOnScreen w))
                let ar                                  = AudioResources c
                    ir                                  = InputResources kR mR
                    lr                                  = LoadingResources acR bcR mcR tcR
                    rr                                  = RandomResources rR
                    tr                                  = TimingResources tR
                    vr                                  = VideoResources bUB bVB sB bVD bID sD t sF bVRa bIRa bURa bVR w
                    r                                   = Resources ar lr ir rr tr vr
                    key k _ KeyState'Released _         = modifyIORef' kR (S.delete k)
                    key k _ KeyState'Pressed _          = modifyIORef' kR (S.insert k)
                    key k x KeyState'Repeating y        = key k x KeyState'Pressed y
                    mouse b MouseButtonState'Pressed _  = modifyIORef' mR (S.insert b)
                    mouse b MouseButtonState'Released _ = modifyIORef' mR (S.delete b)
                setKeyCallback w (Just key)
                setMouseButtonCallback w (Just mouse)
                return r

data InputResources
    = InputResources {  irKeyTable      :: IORef (S.Set Key),
                        irMouseTable    :: IORef (S.Set MouseButton)    }

data AudioResources
    = AudioResources {  arDummyChunk    :: SDL.Mixer.Chunk    }

data VideoResources os
    = VideoResources {  vrBlitUBuffer   :: Buffer os (Uniform (V4 (B4 Float), B4 Float)),
                        vrBlitVBuffer   :: Buffer os (B4 Float, B2 Float),
                        vrBlitShader    :: CompiledShader os
                                            (   V2 Int,
                                                Buffer os (Uniform (V4 (B4 Float), B4 Float)),
                                                PrimitiveArray Triangles (B4 Float, B2 Float),
                                                Texture2D os (Format RGBAFloat)     ),
                        vrDummyVBuffer  :: Buffer os (B4 Float, B4 Float),
                        vrDummyIBuffer  :: Buffer os (B Word32),
                        vrDummyShader   :: CompiledShader os
                                            (Env    os
                                                    (B4 Float, B4 Float)
                                                    (V4 (B4 Float), V4 (B4 Float))),
                        vrDummyTexture  :: Texture2D os (Format RGBAFloat),
                        vrFillShader    :: CompiledShader os
                                            (   V2 Int,
                                                Buffer os (Uniform (    V4 (B4 Float),
                                                                        B4 Float)),
                                                PrimitiveArray Triangles (B4 Float)     ),  
                        vrRasterizeVBuffer :: Buffer os (B4 Float, B4 Float),
                        vrRasterizeIBuffer :: Buffer os (B Word32),
                        vrRasterizeUBuffer :: Buffer os ( Uniform
                                                            ( V4 (B4 Float)
                                                            , V4 (B4 Float)
                                                            )
                                                        ),
                        vrRectVBuffer   :: Buffer os (B4 Float),
                        vrWindow        :: Window os RGBAFloat Depth    }

data RandomResources
    = RandomResources {     rrGenerator     :: IORef TFGen  }

data TimingResources
    = TimingResources {     trTimestamps    :: IORef (M.Map TimeGroup [SystemTime])   }

data LoadingResources os
    = LoadingResources {    lrAudioCache    :: IORef (WeakCache (Audio (FairyBow os))),
                            lrBitmapCache   :: IORef (WeakCache (Bitmap (FairyBow os))),
                            lrMeshCache     :: IORef (WeakCache (Mesh (Point V3 Float, Color) (FairyBow os))),
                            lrTextCache     :: IORef (WeakCache (T.Text))   }

instance MonadPlatform (FairyBow os) (FairyBow os) where
    liftPlatform = id

newDummyChunk = do  pc  <- liftIO (malloc :: IO (Ptr SDL.Raw.Mixer.Chunk))
                    let c = SDL.Raw.Mixer.Chunk {   chunkAllocated  = 0,
                                                    chunkAbuf       = nullPtr,
                                                    chunkAlen       = 0,
                                                    chunkVolume     = 0     }
                    liftIO (poke pc c)
                    return (SDL.Mixer.Chunk pc)

newDummyTexture = do    tex     <- newTexture2D RGBA8 (V2 8 8) 1
                        let  black   = V4 minBound minBound minBound maxBound
                             white   = V4 maxBound maxBound maxBound maxBound
                             row1    = cycle [white, black] :: [V4 Word8]
                             row2    = tail row1
                             ps      = cycle (take 8 row1 ++ take 8 row2)
                        writeTexture2D tex 0 0 (V2 8 8) ps
                        return tex

newDummyBuffers = do    let  vs :: [(Point V3 Float, Color)]
                             vs  = [  (P (V3 (-180) (-60) 0), Red),
                                      (P (V3 180 (-60) 0), Blue),
                                      (P (V3 0 180 0), Green) ]
                             fs  = [Triangle 0 1 2]
                             is  = [0, 1, 2]
                        bufV    <- newBuffer (length vs) :: ContextT Handle os IO (Buffer os (B4 Float, B4 Float))
                        bufI    <- newBuffer (3 * length fs) :: ContextT Handle os IO (Buffer os (B Word32))
                        writeBuffer bufV 0 (map attrib vs)
                        writeBuffer bufI 0 (map fromIntegral is)
                        return (bufV, bufI)

newBatchBuffers = do    bufVBlit    <- newBuffer (floor (2 ** 15)) :: ContextT Handle os IO (Buffer os (B4 Float, B2 Float))
                        bufVRect    <- newBuffer (floor (2 ** 19)) :: ContextT Handle os IO (Buffer os (B4 Float))
                        bufVRaster  <- newBuffer (floor (2 ** 15)) :: ContextT Handle os IO (Buffer os (B4 Float, B4 Float))
                        bufIRaster  <- newBuffer (floor (2 ** 21)) :: ContextT Handle os IO (Buffer os (B Word32))
                        bufURaster  <- newBuffer (floor (2 ** 15)) :: ContextT Handle os IO (Buffer os (Uniform (V4 (B4 Float), V4 (B4 Float))))
                        return (bufVBlit, bufVRect, bufVRaster, bufIRaster, bufURaster)