{-# LANGUAGE UndecidableInstances #-}
module Data.FairyBow.Audio where

import           Control.Exception
import           Control.Monad.Trans
import qualified Data.ByteString as B
import           Data.IORef
import           Data.Lightarrow.Artifact
import           Data.Lightarrow.Audio
import           Data.Vector.Storable as SV ((!))
import qualified Data.Vector.Storable as SV
import           Data.WeakCache
import           FairyBowPlatformType
import           SDL.Mixer hiding (load, Audio(..))
import           System.Lightarrow.Load
import {-# SOURCE #-} System.FairyBow.Platform
import           System.IO                      ( hFlush
                                                , stdout
                                                )

instance ArtifactPlatform (Audio (FairyBow os)) (FairyBow os) where 
    dummy r = Audio "dummy" (Just a)
        where   a = arDummyChunk (rAudio r)

instance AudioPlatform (FairyBow os) where
    data Audio (FairyBow os)
        = Audio {   audioPath       :: FilePath,
                    audioChunk      :: Maybe Chunk  }

instance Eq (Audio (FairyBow os)) where
    a1 == a2    = audioPath a1 == audioPath a2

instance Ord (Audio (FairyBow os)) where
    a1 <= a2    = audioPath a1 <= audioPath a2

instance WeakCacheValue (FairyBow os) (Audio (FairyBow os)) where
    type Name (Audio (FairyBow os)) = FilePath
    load p = do     let f (SomeException _) = return B.empty
                    bs  <- liftIO (catch (B.readFile p) f)
                    liftIO (do  putStr ("Load / decode " ++ show (B.length bs)
                                            ++ " WAV bytes from " ++ show p ++ "...")
                                hFlush stdout)
                    c   <- decode bs
                    liftIO (putStrLn "done.")
                    return (Audio p (Just c))

instance (  FileLocation (Location (Audio (FairyBow os))),
            WeakCacheValue (FairyBow os) (Audio (FairyBow os))     )
                => LoadPlatform (Audio (FairyBow os)) (FairyBow os) where
    request r a
        = return (Audio (toPath a) Nothing)
    load r b@(Audio p (Just _))
        = return b
    load r (Audio p Nothing)
        = do    let cacheRef = lrAudioCache (rLoading r)
                cache0          <- liftIO (readIORef cacheRef)
                (b, cache1)     <- cacheLoad p cache0
                liftIO (writeIORef cacheRef cache1)
                return b
    unload r b@(Audio p _) = return (Audio p Nothing)
