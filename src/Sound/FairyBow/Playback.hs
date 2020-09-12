{-# LANGUAGE UndecidableInstances #-}

module Sound.FairyBow.Playback where

import              Data.FairyBow.Audio
import              FairyBowPlatformType
import              SDL.Mixer hiding (Audio, Channel)
import qualified    SDL.Mixer (Audio, Channel)
import              Sound.Lightarrow.Playback
import              System.FairyBow.Actuation
import              System.Lightarrow.Actuation

instance ActuatePlatform (FairyBow os)
            => MixerPlatform (FairyBow os) where
    data Channel (FairyBow os) = Channel SDL.Mixer.Channel
    channel x                       = Channel (fromIntegral x :: SDL.Mixer.Channel)
    stop (Channel c)                = Actuation [DirectOut (halt c)]
    fade (Channel c) (UnitReal x)   = let v = floor (x * 128)
                                        in Actuation [DirectOut (setVolume v c)]

instance (ActuatePlatform (FairyBow os), MixerPlatform (FairyBow os))
            => PlaybackPlatform (FairyBow os) where
    play (Channel chan) (Audio _ mc)
        = Actuation (case mc of
                        Just chunk  -> [    DirectOut (do   playOn chan Once chunk
                                                            return ())  ]
                        Nothing     -> [])
    onLoop (Channel c) (Audio _ mc) 
        = Actuation (case mc of
                        Just chunk  -> [    DirectOut (do   playOn c Forever chunk
                                                            return ())  ]
                        Nothing     -> [])
