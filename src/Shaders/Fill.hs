module Shaders.Fill where

import Graphics.GPipe
{-

The shader environment below is just a pair, not the |Env| structure
defined in the |Shading| module.

-}
fill :: Window os RGBAFloat Depth
            -> Shader os
                (   V2 Int,
                    Buffer os (Uniform (V4 (B4 Float), B4 Float)),
                    PrimitiveArray Triangles (B4 Float)   )
                ()
fill win = do  streamP      <- toPrimitiveStream (\(_, _, p) -> p)
               (proj, c)    <- getUniform (\(_, u, _) -> (u, 0))
               let  f p         = (proj !* p, c)
                    streamP2    = fmap f streamP
               streamF      <- rasterize optS streamP2
               let  streamF2    = withRasterizedInfo
                                    (\a r -> let V4 _ _ z _ = rasterizedFragCoord r
                                                in (a, z))
                                    streamF
               drawWindowColorDepth (const (win, optC, optD)) streamF2
    where   optS (d,_,_)    = (side, port d, depth)
            optC            = ContextColorOption blending colorMask
            optD            = DepthOption Lequal True
            side            = FrontAndBack
            port d          = ViewPort (V2 0 0) d
            depth           = DepthRange 0 1
            blending        = BlendRgbAlpha (funcRGB, funcA)
                                            (factorRGB, factorA)
                                            (V4 0 0 0 1)
            colorMask       = V4 True True True True
            funcRGB         = FuncAdd
            funcA           = FuncAdd
            factorRGB       = BlendingFactors SrcAlpha OneMinusSrcAlpha
            factorA         = BlendingFactors SrcAlpha OneMinusSrcAlpha  
