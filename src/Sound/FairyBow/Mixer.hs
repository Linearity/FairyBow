{-# LANGUAGE UndecidableInstances #-}

module Sound.FairyBow.Mixer where

import              Data.FairyBow.Audio
import              FairyBowPlatformType
import              SDL.Mixer hiding (Audio, Channel)
import qualified    SDL.Mixer (Channel)
import              Sound.Lightarrow.Mixer
import              System.FairyBow.Actuation
import              System.Lightarrow.Actuation

instance ActuatePlatform (FairyBow os)
            => MixerPlatform (FairyBow os) where
    data Channel (FairyBow os) = Channel SDL.Mixer.Channel
    channel x           = Channel (fromIntegral x :: SDL.Mixer.Channel)
    stop (Channel c)    = Actuation [DirectOut (halt c)]
    fade (Channel c) x  = let v = floor (fromUnitReal x * 128)
                            in Actuation [DirectOut (setVolume v c)]
    play (Channel chan) (Audio _ mc)
        = Actuation (case mc of
                        Just chunk  -> [    DirectOut (do   playOn chan Once chunk
                                                            return ())  ]
                        Nothing     -> [])
    onLoop (Channel chan) (Audio _ mc) 
        = Actuation (case mc of
                        Just chunk  -> [    DirectOut (do   playOn chan Forever chunk
                                                            return ())  ]
                        Nothing     -> [])