module System.FairyBow.Sensation where

import           Control.Monad.Trans
import           Data.Maybe
import qualified Data.Set as S
import           Data.IORef
import           FairyBowPlatformType
import           Graphics.GPipe
import           Graphics.GPipe.Context.GLFW as GLFW
import           System.Lightarrow.Joystick
import           System.Lightarrow.Keyboard
import           System.Lightarrow.Mouse
import           System.Lightarrow.Sensation
import           System.FairyBow.Joystick()
import           System.FairyBow.Keyboard()
import           System.FairyBow.Mouse()
import           System.FairyBow.Platform

instance SensePlatform (FairyBow os) where
    data Sensation (FairyBow os)
        = Sensation {   senseCursorPosition     :: (Double, Double),
                        senseKeys               :: S.Set Key, --M.Map Key KeyState,
                        senseMouseButtons       :: S.Set GLFW.MouseButton, -- M.Map MouseButton MouseButtonState,
                        senseJoystickPosition   :: [Double],
                        senseJoystickButtons    :: [JoystickButtonState]   }
    sense r     = do    keys            <- liftIO (readIORef kR)
                        mouseButtons    <- liftIO (readIORef mR)
                        mJoyAxes        <- liftIO (getJoystickAxes Joystick'1)
                        mJoyButtons     <- liftIO (getJoystickButtons Joystick'1)
                        mCursor         <- getCursorPos w
                        mWindowDims     <- getWindowSize w
                        V2 wF hF        <- getFrameBufferSize w
                        let     joyButtons      = fromMaybe [] mJoyButtons
                                joyAxes         = fromMaybe [] mJoyAxes ++ repeat 0
                                (wW, hW)        = fromMaybe (0, 0) mWindowDims
                                wR              = fromIntegral wF / fromIntegral wW
                                hR              = fromIntegral hF / fromIntegral hW
                                offset (x, y)   = (wR * (x - fromIntegral wW / 2),
                                                    hR * (fromIntegral hW / 2 - y))
                                cursor          = maybe (0, 0) offset mCursor
                        return (Sensation cursor keys mouseButtons joyAxes joyButtons)
        where   InputResources { irKeyTable = kR, irMouseTable = mR }
                            = rInput r
                VideoResources { vrWindow = w }
                            = rVideo r

instance Semigroup (Sensation (FairyBow os)) where
    s1 <> s2 = Sensation {  senseCursorPosition     = let   (x1, y1)    = senseCursorPosition s1
                                                            (x2, y2)    = senseCursorPosition s2
                                                        in (x1 + x2, y1 + y2),
                            senseKeys               = let   ks1     = senseKeys s1
                                                            ks2     = senseKeys s2
                                                        in S.union ks1 ks2,
                            senseMouseButtons       = let   bs1     = senseMouseButtons s1
                                                            bs2     = senseMouseButtons s2
                                                        in S.union bs1 bs2,
                            senseJoystickPosition   = let   ps1     = senseJoystickPosition s1
                                                            ps2     = senseJoystickPosition s2
                                                        in zipWith (+) ps1 ps2,
                            senseJoystickButtons    = let   bs1     = senseJoystickButtons s1
                                                            bs2     = senseJoystickButtons s2
                                                        in zipWith min bs1 bs2    }

instance Monoid (Sensation (FairyBow os)) where
    mempty = Sensation {    senseCursorPosition     = (0, 0),
                            senseKeys               = S.empty,
                            senseMouseButtons       = S.empty,
                            senseJoystickPosition   = repeat 0,
                            senseJoystickButtons    = repeat JoystickButtonState'Released   }

instance KeyboardPlatform (FairyBow os) where
    type KeyCode (FairyBow os) = Key
    keyPressed k s = S.member k (senseKeys s)

instance MousePlatform (FairyBow os) where
    type MouseButtonCode (FairyBow os) = GLFW.MouseButton
    cursorPosition s                = senseCursorPosition s
    setCursorPosition s x           = s { senseCursorPosition = x }
    mouseVelocity _                 = (0, 0) -- velocity not directly available in GLFW, must compute ourselves
    setMouseVelocity s _            = s
    mousePressed b s                = S.member b (senseMouseButtons s)
    setMousePressed b s True        = s { senseMouseButtons
                                            = S.insert b (senseMouseButtons s) }
    setMousePressed b s False       = s { senseMouseButtons
                                            = S.delete b (senseMouseButtons s) }

instance JoystickPlatform (FairyBow os) where
    type JoystickAxisCode (FairyBow os) = Int
    type JoystickButtonCode (FairyBow os) = Int
    joystickPressed b s
        | b < 0         = False
        | otherwise     = (map (== JoystickButtonState'Pressed) (senseJoystickButtons s)
                                ++ repeat False) !! b
    joystickTilt a s
        | a < 0         = 0
        | otherwise     = (senseJoystickPosition s ++ repeat 0) !! a
