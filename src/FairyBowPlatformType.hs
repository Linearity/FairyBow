module FairyBowPlatformType where

import Graphics.GPipe
import Graphics.GPipe.Context.GLFW

type FairyBow os = ContextT Handle os IO