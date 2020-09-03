module System.FairyBow.Mouse where

import           Graphics.GPipe.Context.GLFW hiding (MouseButton)
import qualified Graphics.GPipe.Context.GLFW as GLFW (MouseButton)
import           System.Lightarrow.Mouse

instance MouseButton (GLFW.MouseButton) where
    leftMouseButton     = MouseButton'1
    middleMouseButton   = MouseButton'2 
    otherMouseButton n  = toEnum (fromEnum MouseButton'1 + (n - 1))
    rightMouseButton    = MouseButton'3
