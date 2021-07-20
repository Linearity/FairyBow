module Shaders.Blit where

import Graphics.GPipe

{- The shader environment below is just a quadruple, not the |Env| structure
 - defined in the |Shading| module. -}

blit :: Window os RGBAFloat Depth
            -> Shader os
                (   V2 Int,
                    Buffer os (Uniform (V4 (B4 Float), B4 Float)),
                    PrimitiveArray Triangles (B4 Float, B2 Float),
                    Texture2D os (Format RGBAFloat)     )
                ()
blit win = do  streamP      <- toPrimitiveStream (\(_, _, p, _) -> p)
               (proj, _)    <- getUniform (\(_, u, _, _) -> (u, 0))
               let  f (p, uv)   = (proj !* p, uv)
                    streamP2    = fmap f streamP
               streamF      <- rasterize optS streamP2
               let  filter      = SamplerFilter Nearest Nearest Nearest Nothing
                    edge        = (pure Repeat, undefined)
               samp         <- newSampler2D (\(_, _, _, t) -> (t, filter, edge))
               (_, c)       <- getUniform (\(_, u, _, _) -> (u, 0))
               let  sampling    = sample2D samp SampleAuto Nothing Nothing
                    streamF2    = fmap sampling streamF 
                    streamF3    = fmap (* fmap toFloat c) streamF2
                    streamF4    = withRasterizedInfo
                                    (\a r -> let V4 _ _ z _ = rasterizedFragCoord r
                                                in (a, z))
                                    streamF3
               drawWindowColorDepth (const (win, optC, optD)) streamF4
    where   optS (d,_,_,_)  = (side, port d, depth)
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
