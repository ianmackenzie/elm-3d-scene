module Scene3d.Exposure exposing
    ( Exposure
    , fromEv100, photographic, sunny16, fromMaxLuminance
    , maxLuminance
    , srgb
    )

{-|

@docs Exposure

@docs fromEv100, photographic, sunny16, fromMaxLuminance

@docs maxLuminance

-}

import Duration exposing (Duration)
import Luminance exposing (Luminance)
import Quantity


type Exposure
    = Exposure { maxLuminance : Luminance }


fromEv100 : Float -> Exposure
fromEv100 ev100 =
    -- from https://media.contentapi.ea.com/content/dam/eacom/frostbite/files/course-notes-moving-frostbite-to-pbr-v2.pdf
    Exposure { maxLuminance = Luminance.nits (1.2 * 2 ^ ev100) }


fromMaxLuminance : Luminance -> Exposure
fromMaxLuminance givenMaxLuminance =
    Exposure { maxLuminance = Quantity.abs givenMaxLuminance }


photographic : { fStop : Float, shutterSpeed : Duration, isoSpeed : Float } -> Exposure
photographic { fStop, shutterSpeed, isoSpeed } =
    let
        t =
            Duration.inSeconds shutterSpeed
    in
    -- from https://media.contentapi.ea.com/content/dam/eacom/frostbite/files/course-notes-moving-frostbite-to-pbr-v2.pdf
    fromEv100 (logBase 2 ((100 * fStop ^ 2) / (t * isoSpeed)))


sunny16 : Exposure
sunny16 =
    photographic
        { fStop = 16
        , shutterSpeed = Duration.seconds 0.01
        , isoSpeed = 100
        }


srgb : Exposure
srgb =
    fromMaxLuminance (Luminance.nits 80)


maxLuminance : Exposure -> Luminance
maxLuminance (Exposure exposure) =
    exposure.maxLuminance
