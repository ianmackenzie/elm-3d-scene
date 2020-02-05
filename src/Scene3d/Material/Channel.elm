module Scene3d.Material.Channel exposing
    ( Channel
    , average
    , blue
    , green
    , luminance
    , red
    )

import Math.Vector4
import Scene3d.Types as Types


type alias Channel =
    Types.Channel


channel : Float -> Float -> Float -> Channel
channel r g b =
    Types.Channel (Math.Vector4.vec4 r g b 0)


red : Channel
red =
    channel 1 0 0


green : Channel
green =
    channel 0 1 0


blue : Channel
blue =
    channel 0 0 1


luminance : Channel
luminance =
    channel 0.2126 0.7152 0.0722


average : Channel
average =
    channel (1 / 3) (1 / 3) (1 / 3)
