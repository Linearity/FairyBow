{-# LANGUAGE UndecidableInstances #-}

module Data.FairyBow.Text where

import           Control.Exception
import           Control.Monad.Trans
import           Data.IORef
import           Data.Lightarrow.Artifact
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.WeakCache
import           FairyBowPlatformType
import           System.Lightarrow.Load
import {-# SOURCE #-} System.FairyBow.Platform
import           System.IO                      ( hFlush
                                                , stdout
                                                )

instance ArtifactPlatform T.Text (FairyBow os) where 
    dummy _r = T.pack "dummy"

instance WeakCacheValue (FairyBow os) T.Text where
    type Name T.Text = FilePath
    load p = do     let f (SomeException _) = do    putStrLn "failed."
                                                    return (T.pack "")
                    liftIO (do  putStr ("Load text from " ++ show p ++ "...")
                                hFlush stdout)
                    liftIO (handle f (do    t   <- TIO.readFile p
                                            putStrLn "done."
                                            return t))

instance (FileLocation (Location T.Text), WeakCacheValue (FairyBow os) T.Text)
        => LoadPlatform T.Text (FairyBow os) where
    request _r n    = return (T.pack (toPath n))
    load r p        = do    let cacheRef = lrTextCache (rLoading r)
                            cache0          <- liftIO (readIORef cacheRef)
                            (b, cache1)     <- cacheLoad (T.unpack p) cache0
                            liftIO (writeIORef cacheRef cache1)
                            return b
    unload _r _t    = return (T.pack "")
