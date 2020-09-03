module Shaders.ColorsOnScreen where

import {-# SOURCE #-} Data.FairyBow.Shading
import Graphics.GPipe hiding (Color)

colorsOnScreen :: Window os RGBAFloat Depth
                    -> (    Shader os
                                (Env os
                                    (B4 Float, B4 Float)
                                    (V4 (B4 Float), V4 (B4 Float)))
                                (),
                            UniformFunc (M44 Float, M44 Float)  )
colorsOnScreen win = (shader, defaultUniformFunc)
    where  shader         =  do  streamP  <- toPrimitiveStream ePrimitives
                                 u        <- getUniform eUniform
                                 let  view      = tuView u
                                      proj      = tuProj u
                                      xform     = proj !*! view
                                      f (p, c)  = (xform !* p, c)
                                      streamP2  = fmap f streamP
                                 streamF  <- rasterize optS streamP2
                                 drawWindowColor (const (win, optC)) streamF
           optS env       = let d = eFrameSize env in (side, port d, depth)
           side           = FrontAndBack
           port d         = ViewPort (V2 0 0) d
           depth          = DepthRange 0 1
           optC           = ContextColorOption NoBlending (V4 True True True True)
