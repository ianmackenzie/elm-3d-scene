module Scene3d.Material.Channel exposing
    ( Channel
    , red, green, blue
    , luminance, average
    )

{-|

@docs Channel

@docs red, green, blue

@docs luminance, average

-}

import Math.Vector4
import Scene3d.Types as Types


{-| TODO
-}
type alias Channel =
    Types.Channel


channel : Float -> Float -> Float -> Channel
channel r g b =
    Types.Channel (Math.Vector4.vec4 r g b 0)


{-| TODO
-}
red : Channel
red =
    channel 1 0 0


{-| TODO
-}
green : Channel
green =
    channel 0 1 0


{-| TODO
-}
blue : Channel
blue =
    channel 0 0 1


{-| TODO
-}
luminance : Channel
luminance =
    channel 0.2126 0.7152 0.0722


{-| TODO
-}
average : Channel
average =
    channel (1 / 3) (1 / 3) (1 / 3)
