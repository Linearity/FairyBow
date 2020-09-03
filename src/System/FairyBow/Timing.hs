{-# LANGUAGE UndecidableInstances #-}

module System.FairyBow.Timing where

import           Control.Monad.IO.Class
import           Data.IORef
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Time.Clock.System
import           FairyBowPlatformType
import           System.FairyBow.Platform
import           System.Lightarrow.Timing

type instance TimeGroup = ()

instance Ord TimeGroup => TimingPlatform (FairyBow os) where
    timeStep r g = liftIO (getStep g tR)
        where   Resources { rTiming = TimingResources { trTimestamps = tR } } = r
    stepRate r g = liftIO (readIORef tR >>= return . rate g)
        where   Resources { rTiming = TimingResources { trTimestamps = tR } } = r

{- The time step comes from the `getTimeStep` action, to which we provide the `IORef` we allocated in `main`. -}

getStep :: Ord TimeGroup =>
                TimeGroup -> IORef (M.Map TimeGroup [SystemTime]) -> IO Double
getStep g ref = do  !t1     <- getSystemTime
                    groups  <- readIORef ref
                    let addNew      = maybe (Just (replicate 30 t1)) (return . (t1 :) . init)
                        newGroups   = M.alter addNew g groups
                        t0:_        = fromMaybe [t1] (M.lookup g groups) -- but always Just
                    writeIORef ref newGroups
                    return (timeInSeconds t1 - timeInSeconds t0)

{- To keep track of the current frame rate, we use an `IORef` to record the most recent times at which we finished a frame. In an `IO` action we read and update this list of times and report the mean difference between successive frames. -}

rate :: Ord TimeGroup
                => TimeGroup -> M.Map TimeGroup [SystemTime] -> Double
rate g groups = fromMaybe 0
                    (do     timestamps   <- M.lookup g groups
                            let times   = map timeInSeconds timestamps
                                diffs   = zipWith (-) times (tail times)
                            return (sum diffs / fromIntegral (length diffs)))

{- A simple formula converts the system-defined representation of time to our canonical representation. -}

timeInSeconds :: SystemTime -> Double
timeInSeconds tS = s + ns
    where  s   = fromIntegral (systemSeconds tS)
           ns  = fromIntegral (systemNanoseconds tS) / 1000000000
