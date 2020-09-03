module System.Lightarrow.Run where

import FairyBow()
import Graphics.GPipe
import Graphics.GPipe.Context.GLFW

type Lightarrow os = ContextT Handle os IO

run = runContextT defaultHandleConfig
