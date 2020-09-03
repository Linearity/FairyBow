module Shaders.Blit where

import Graphics.GPipe

{- The shader environment below is just a quadruple, not the |Env| structure
 - defined in the |Shading| module. -}

blit :: Window os RGBAFloat Depth
            -> Shader os
                (   V2 Int,
                    Buffer os (Uniform (V4 (B4 Float), V4 (B4 Float))),
                    PrimitiveArray Triangles (B4 Float, B2 Float),
                    Texture2D os (Format RGBAFloat)     )
                ()
blit win = do  streamP  <- toPrimitiveStream (\(_, _, p, _) -> p)
               u        <- getUniform (\(_, u, _, _) -> (u, 0))
               let  (_, proj) = u
                    f (p, uv) = (proj !* p, uv)
                    streamP2  = fmap f streamP
               streamF  <- rasterize optS streamP2
               let  filter    = SamplerFilter Nearest Nearest Nearest Nothing
                    edge      = (pure Repeat, undefined)
               samp     <- newSampler2D (\(_, _, _, t) -> (t, filter, edge))
               let  sampling  = sample2D samp SampleAuto Nothing Nothing
                    streamF2  = fmap sampling streamF 
                    streamF3  = withRasterizedInfo (\a r -> let V4 x y z w = rasterizedFragCoord r in (a, z)) streamF2
               drawWindowColorDepth (const (win, optC, optD)) streamF3
    where  optS (d, _, _, _) =  (side, port d, depth)
           side           = FrontAndBack
           port d         = ViewPort (V2 0 0) d
           depth          = DepthRange 0 1
           optC           = ContextColorOption (BlendRgbAlpha (FuncAdd, FuncAdd) (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors SrcAlpha OneMinusSrcAlpha) (V4 0 0 0 1)) (V4 True True True True)
           optD           = DepthOption Lequal True


