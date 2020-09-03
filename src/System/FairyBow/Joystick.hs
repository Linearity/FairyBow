module System.FairyBow.Joystick where

import System.Lightarrow.Joystick

instance JoystickButton Int where
    otherJoyButton = id

instance JoystickAxis Int where
    joyAxisX        = 0
    joyAxisY        = 1
    joyAxisLeftX    = 0
    joyAxisLeftY    = 1
    joyAxisRightX   = 2
    joyAxisRightY   = 5
    otherJoyAxis    = id
