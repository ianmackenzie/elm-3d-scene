module Scene3d.Color exposing
    ( Xyz(..), LinearRgb(..)
    , fromLinearRgb, toLinearRgb
    , fromXyz, toXyz
    , xyzToLinearRgb, linearRgbToXyz
    )

{-|

@docs Xyz, LinearRgb

@docs fromLinearRgb, toLinearRgb

@docs fromXyz, toXyz

@docs xyzToLinearRgb, linearRgbToXyz

-}

import Color exposing (Color)


type Xyz
    = Xyz Float Float Float


type LinearRgb
    = LinearRgb Float Float Float


toLinearRgb : Color -> LinearRgb
toLinearRgb color =
    let
        srgb =
            Color.toRgba color
    in
    LinearRgb
        (inverseGamma srgb.red)
        (inverseGamma srgb.green)
        (inverseGamma srgb.blue)


fromLinearRgb : LinearRgb -> Color
fromLinearRgb (LinearRgb linearR linearG linearB) =
    Color.rgb
        (gammaCorrect linearR)
        (gammaCorrect linearG)
        (gammaCorrect linearB)


toXyz : Color -> Xyz
toXyz color =
    color |> toLinearRgb |> linearRgbToXyz


fromXyz : Xyz -> Color
fromXyz xyz =
    xyz |> xyzToLinearRgb |> fromLinearRgb


xyzToLinearRgb : Xyz -> LinearRgb
xyzToLinearRgb (Xyz bigX bigY bigZ) =
    LinearRgb
        (3.2406 * bigX - 1.5372 * bigY - 0.4986 * bigZ)
        (-0.9689 * bigX + 1.8758 * bigY + 0.0415 * bigZ)
        (0.0557 * bigX - 0.204 * bigY + 1.057 * bigZ)


linearRgbToXyz : LinearRgb -> Xyz
linearRgbToXyz (LinearRgb linearR linearG linearB) =
    Xyz
        (0.4124 * linearR + 0.3576 * linearG + 0.1805 * linearB)
        (0.2126 * linearR + 0.7152 * linearG + 0.0722 * linearB)
        (0.0193 * linearR + 0.1192 * linearG + 0.9505 * linearB)


gammaCorrect : Float -> Float
gammaCorrect u =
    clamp 0 1 <|
        if u <= 0.0031308 then
            12.92 * u

        else
            1.055 * (u ^ (1 / 2.4)) - 0.055


inverseGamma : Float -> Float
inverseGamma u =
    clamp 0 1 <|
        if u <= 0.04045 then
            u / 12.92

        else
            ((u + 0.055) / 1.055) ^ 2.4
