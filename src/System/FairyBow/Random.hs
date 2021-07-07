module System.FairyBow.Random where

import Control.Monad.IO.Class
import Data.IORef
import System.FairyBow.Platform
import System.Lightarrow.Random
import System.Random.TF.Gen

import FairyBowPlatformType

instance RandomPlatform (FairyBow os) where
    getRandom r = liftIO (readRand rr)
        where Resources { rRandom = RandomResources { rrGenerator = rr } } = r

readRand ref = do   g   <- readIORef ref
                    let (x, g') = next g
                    writeIORef ref g'
                    return x
