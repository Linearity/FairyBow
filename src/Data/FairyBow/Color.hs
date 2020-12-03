{-

Let us define how FairyBow interprets the standard |Color| type in Lightarrow.

-}
module Data.FairyBow.Color where

import Graphics.GPipe hiding (Color)

import Data.Lightarrow.Color
{-

To convert between Lightarrow's representation of colors and GPipe’s
representation, we define integer values for each of the red, green, blue,
and alpha channels. For the constant colors these are defined by fiat.

-}
convert :: Color -> V4 Float
convert  Black     = V4 0 0 0 1
convert  Blue      = V4 0 0 1 1
convert  Cyan      = V4 0 1 1 1
convert  Gray      = V4 0.25 0.25 0.25 1
convert  Green     = V4 0 1 0 1
convert  Magenta   = V4 1 0 1 1
convert  Red       = V4 1 0 0 1
convert  White     = V4 1 1 1 1
convert  Yellow    = V4 1 1 0 1
{-

Naturally, for those constructors with a parameter for each channel, we
convert the parameters to integers.

-}
convert  (Color r g b a)   = fmap realToFrac (V4 r g b a)
convert  (ColorB r g b a)  = fmap ((/ 255) . fromIntegral) (V4 r g b a)
{-

We define the channels of the wrapper constructors recursively. We scale
the channels of the wrapped color by factors of 2.

-}
convert  (Dark color)         = V4 (darken r) (darken g) (darken b) a
    where  darken      = (/ 2)
           V4 r g b a  = convert color
convert  (Light color)        = V4 (lighten r) (lighten g) (lighten b) a
    where  lighten     = (* 2)
           V4 r g b a  = convert color
convert  (Translucent color)  = V4 r g b (a / 2)
    where  V4 r g b a = convert color
{-

Translating |Color|s into 4-vectors is a pretty general first step toward
setting pixel colors on a display. Lightarrow platforms that don't use Linear
or GPipe can likely reuse this scheme without much change.

-}