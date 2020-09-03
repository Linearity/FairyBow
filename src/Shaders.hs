module Shaders (    module Shaders.Blit,
                    module Shaders.ColorsOnScreen,
                    module Shaders.ColorDepthOnScreen,
                    VertexColorShading,
                    vertexColorShadings ) where

import Graphics.GPipe hiding (Color)

import {-# SOURCE #-} Data.FairyBow.Shading
import Shaders.Blit
import Shaders.ColorsOnScreen
import Shaders.ColorDepthOnScreen

type VertexColorShading os = (  Shader os
                                    (Env os
                                        (B4 Float, B4 Float)
                                        (V4 (B4 Float), V4 (B4 Float)))
                                    (),
                                UniformFunc (M44 Float, M44 Float)  )

vertexColorShadings :: [Window os RGBAFloat Depth -> VertexColorShading os]
vertexColorShadings = [colorsOnScreen, colorDepthOnScreen]


