module System.FairyBow.Keyboard where

import           Data.Char
import           Graphics.GPipe.Context.GLFW
import           System.Lightarrow.Keyboard

instance Keyboard Key where
    altKey LeftSide         = Key'LeftAlt
    altKey RightSide        = Key'RightAlt
    arrowDownKey            = Key'Down
    arrowLeftKey            = Key'Left
    arrowRightKey           = Key'Right
    arrowUpKey              = Key'Up
    backspaceKey            = Key'Backspace
    capsLockKey             = Key'CapsLock
    charKey ' '             = Key'Space
    charKey '\''            = Key'Apostrophe
    charKey ','             = Key'Comma
    charKey '-'             = Key'Minus
    charKey '.'             = Key'Period
    charKey '/'             = Key'Slash
    charKey ';'             = Key'Semicolon
    charKey '='             = Key'Equal
    charKey c
        | isUpper c         = charKey (toLower c)
        | isDigit c         = toEnum (fromEnum Key'0 + fromEnum c - fromEnum '0') -- from Unicode
        | isLetter c        = toEnum (fromEnum Key'A + (fromEnum c - fromEnum 'a')) -- from Unicode
    commandKey LeftSide     = Key'LeftSuper
    commandKey RightSide    = Key'RightSuper
    controlKey LeftSide     = Key'LeftControl
    controlKey RightSide    = Key'RightControl
    deleteKey               = Key'Delete
    enterKey                = Key'Enter
    escapeKey               = Key'Escape
    fnKey                   = Key'Unknown
    functionKey n
        | n > 0 && n < 26   = toEnum (fromEnum Key'F1 + (n - 1))
        | otherwise         = Key'Unknown
    optionKey LeftSide      = Key'LeftAlt
    optionKey RightSide     = Key'LeftAlt
    otherKey _              = Key'Unknown
    returnKey               = Key'Enter
    shiftKey LeftSide       = Key'LeftShift
    shiftKey RightSide      = Key'RightShift
    tabKey                  = Key'Tab
    windowsKey              = Key'LeftSuper
