module Scene3d.ColorConversions exposing
    ( linearRgbToColor, colorToLinearRgb
    , cieXyzToColor, colorToCieXyz
    , cieXyzToLinearRgb, linearRgbToCieXyz
    , chromaticityToCieXyz, chromaticityToLinearRgb
    )

{-|

@docs linearRgbToColor, colorToLinearRgb

@docs cieXyzToColor, colorToCieXyz

@docs cieXyzToLinearRgb, linearRgbToCieXyz

@docs chromaticityToCieXyz, chromaticityToLinearRgb

-}

import Color exposing (Color)
import Math.Vector3 as Vector3 exposing (vec3)
import Quantity exposing (Quantity(..), Unitless)
import Scene3d.Types as Types exposing (Chromaticity(..), CieXyz(..), LinearRgb(..))


colorToLinearRgb : Color -> LinearRgb Unitless
colorToLinearRgb color =
    let
        { red, green, blue } =
            Color.toRgba color
    in
    LinearRgb <|
        vec3
            (inverseGamma red)
            (inverseGamma green)
            (inverseGamma blue)


linearRgbToColor : LinearRgb Unitless -> Color
linearRgbToColor (LinearRgb linearRgb) =
    Color.rgb
        (gammaCorrect (Vector3.getX linearRgb))
        (gammaCorrect (Vector3.getY linearRgb))
        (gammaCorrect (Vector3.getZ linearRgb))


colorToCieXyz : Color -> CieXyz Unitless
colorToCieXyz color =
    color |> colorToLinearRgb |> linearRgbToCieXyz


cieXyzToColor : CieXyz Unitless -> Color
cieXyzToColor xyz =
    xyz |> cieXyzToLinearRgb |> linearRgbToColor


cieXyzToLinearRgb : CieXyz units -> LinearRgb units
cieXyzToLinearRgb (CieXyz bigX bigY bigZ) =
    LinearRgb <|
        vec3
            (3.2406 * bigX - 1.5372 * bigY - 0.4986 * bigZ)
            (-0.9689 * bigX + 1.8758 * bigY + 0.0415 * bigZ)
            (0.0557 * bigX - 0.204 * bigY + 1.057 * bigZ)


linearRgbToCieXyz : LinearRgb units -> CieXyz units
linearRgbToCieXyz (LinearRgb linearRgb) =
    let
        linearR =
            Vector3.getX linearRgb

        linearG =
            Vector3.getY linearRgb

        linearB =
            Vector3.getZ linearRgb
    in
    CieXyz
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


chromaticityToCieXyz : Quantity Float units -> Chromaticity -> CieXyz units
chromaticityToCieXyz (Quantity intensity) (Types.Chromaticity { x, y }) =
    CieXyz (intensity * x / y) intensity (intensity * (1 - x - y) / y)


chromaticityToLinearRgb : Quantity Float units -> Chromaticity -> LinearRgb units
chromaticityToLinearRgb intensity chromaticity =
    cieXyzToLinearRgb (chromaticityToCieXyz intensity chromaticity)
