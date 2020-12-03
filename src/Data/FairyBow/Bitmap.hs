{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.FairyBow.Bitmap where

import           Codec.Picture
import           Control.Exception
import           Control.Monad.Trans
import qualified Data.ByteString as B
import           Data.IORef
import           Data.Lightarrow.Artifact
import           Data.Lightarrow.Bitmap
import           Data.Vector.Storable as SV ((!))
import qualified Data.Vector.Storable as SV
import           Data.WeakCache
import           FairyBowPlatformType
import           Graphics.GPipe
import           System.Lightarrow.Load
import {-# SOURCE #-} System.FairyBow.Platform
import           System.IO                      ( hFlush
                                                , stdout
                                                )

instance ArtifactPlatform (Bitmap (FairyBow os)) (FairyBow os) where 
    dummy r = Bitmap "dummy" (w, h) (Just t)
        where   t       = vrDummyTexture (rVideo r)
                sizes   = texture2DSizes t
                V2 w h  = head sizes

instance BitmapPlatform (FairyBow os) where
    data Bitmap (FairyBow os)
        = Bitmap {  bitmapPath          :: FilePath,
                    bitmapDimensions    :: (Int, Int),
                    bitmapTexture       :: Maybe (Texture2D os (Format RGBAFloat))  }
    dimensions (Bitmap _ (w, h) _) = (w, h)

instance Eq (Bitmap (FairyBow os)) where
    b1 == b2    = bitmapPath b1 == bitmapPath b2

instance Ord (Bitmap (FairyBow os)) where
    b1 <= b2    = bitmapPath b1 <= bitmapPath b2

deriving instance Show (Codec.Picture.Image Pixel8)
deriving instance Show (Codec.Picture.Image Pixel16)
deriving instance Show (Codec.Picture.Image Pixel32)
deriving instance Show (Codec.Picture.Image PixelF)
deriving instance Show (Codec.Picture.Image PixelYA8)
deriving instance Show (Codec.Picture.Image PixelYA16)
deriving instance Show (Codec.Picture.Image PixelRGB8)
deriving instance Show (Codec.Picture.Image PixelRGB16)
deriving instance Show (Codec.Picture.Image PixelRGBF)
deriving instance Show (Codec.Picture.Image PixelRGBA8)
deriving instance Show (Codec.Picture.Image PixelRGBA16)
deriving instance Show (Codec.Picture.Image PixelYCbCr8)
deriving instance Show (Codec.Picture.Image PixelCMYK8)
deriving instance Show (Codec.Picture.Image PixelCMYK16)
deriving instance Show DynamicImage

instance WeakCacheValue (FairyBow os) (Bitmap (FairyBow os)) where
    type Name (Bitmap (FairyBow os)) = FilePath
    load p = do     let f (SomeException _) = return B.empty
                    bs  <- liftIO (catch (B.readFile p) f)
                    liftIO (do  putStr ("Load / decode " ++ show (B.length bs)
                                            ++ " PNG bytes from " ++ show p ++ "...")
                                hFlush stdout)
                    case decodePng bs of
                        Left _      -> do   liftIO (putStrLn "failed.")
                                            return (Bitmap p (-1, -1) Nothing)
                        Right (ImageRGBA8 image)
                                    -> do   liftIO (putStrLn "done.")
                                            let     colors  = imageData image
                                                    w       = imageWidth image
                                                    h       = imageHeight image
                                            t   <- writeNewTexture colors w h
                                            return (Bitmap p (w, h) (Just t))
                        Right (ImageRGB8 image)
                                    -> do   liftIO (putStrLn "done.")
                                            let     colors  = SV.generate n mkColor
                                                    w       = imageWidth image
                                                    h       = imageHeight image
                                                    n       = 4 * SV.length solids `div` 3
                                                    mkColor k
                                                        | k `mod` 4 == 3
                                                            = 255
                                                        | otherwise
                                                            = solids ! (k - (k `div` 4))
                                                    solids  = imageData image
                                            t   <- writeNewTexture colors w h
                                            return (Bitmap p (w, h) (Just t))
                        Right (ImageY8 image)
                                    -> do   liftIO (putStrLn "done.")
                                            let     colors  = SV.generate n mkColor
                                                    w       = imageWidth image
                                                    h       = imageHeight image
                                                    n       = 4 * SV.length grays
                                                    mkColor k
                                                        | k `mod` 4 == 3
                                                            = 255
                                                        | otherwise
                                                            = grays ! (k `div` 4)
                                                    grays   = imageData image
                                            t   <- writeNewTexture colors w h
                                            return (Bitmap p (w, h) (Just t))
                        Right other -> do   liftIO (putStrLn "failed.")
                                            liftIO (putStr "Unhandled format: ")
                                            liftIO (print other)
                                            return (Bitmap p (-1, -1) Nothing)

writeNewTexture colors w h
    = do    t   <- newTexture2D RGBA8 (V2 w h) 1
            liftIO (do  putStr ("Write " ++ show (SV.length colors `div` 4)
                                        ++ " pixels to texture...")
                        hFlush stdout)
            writeTexture2D t 0 0 (V2 w h) (pixels colors)
            liftIO (putStrLn "done.")
            return t

instance (  FileLocation (Location (Bitmap (FairyBow os))),
            WeakCacheValue (FairyBow os) (Bitmap (FairyBow os))     )
                => LoadPlatform (Bitmap (FairyBow os)) (FairyBow os) where
    request _r a
        = return (Bitmap (toPath a) (-1, -1) Nothing)
    load _r b@(Bitmap _p _ (Just _))
        = return b
    load r (Bitmap p _ Nothing)
        = do    let cacheRef = lrBitmapCache (rLoading r)
                cache0          <- liftIO (readIORef cacheRef)
                (b, cache1)     <- cacheLoad p cache0
                liftIO (writeIORef cacheRef cache1)
                return b
    unload _r _b@(Bitmap p _d _) = return (Bitmap p (-1,-1) Nothing)


pixels :: SV.Storable a => SV.Vector a -> [V4 a]
pixels cs
    | SV.length cs >= 4     = let p = SV.take 4 cs
                                in V4 (p!0) (p!1) (p!2) (p!3) : pixels (SV.drop 4 cs)
    | otherwise             = []