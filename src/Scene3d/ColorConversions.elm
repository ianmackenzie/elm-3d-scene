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
import Scene3d.Types as Types exposing (Chromaticity(..), CieXyz(..), LinearRgb(..))


colorToLinearRgb : Color -> LinearRgb
colorToLinearRgb color =
    let
        ( red, green, blue ) =
            Color.toRGB color
    in
    LinearRgb <|
        vec3
            (inverseGamma (red / 255))
            (inverseGamma (green / 255))
            (inverseGamma (blue / 255))


linearRgbToColor : LinearRgb -> Color
linearRgbToColor (LinearRgb linearRgb) =
    Color.fromRGB
        ( 255 * gammaCorrect (Vector3.getX linearRgb)
        , 255 * gammaCorrect (Vector3.getY linearRgb)
        , 255 * gammaCorrect (Vector3.getZ linearRgb)
        )


colorToCieXyz : Color -> CieXyz
colorToCieXyz color =
    color |> colorToLinearRgb |> linearRgbToCieXyz


cieXyzToColor : CieXyz -> Color
cieXyzToColor xyz =
    xyz |> cieXyzToLinearRgb |> linearRgbToColor


cieXyzToLinearRgb : CieXyz -> LinearRgb
cieXyzToLinearRgb (CieXyz bigX bigY bigZ) =
    LinearRgb <|
        vec3
            (3.2406 * bigX - 1.5372 * bigY - 0.4986 * bigZ)
            (-0.9689 * bigX + 1.8758 * bigY + 0.0415 * bigZ)
            (0.0557 * bigX - 0.204 * bigY + 1.057 * bigZ)


linearRgbToCieXyz : LinearRgb -> CieXyz
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


chromaticityToCieXyz : Chromaticity -> CieXyz
chromaticityToCieXyz (Types.Chromaticity { x, y }) =
    CieXyz (x / y) 1 ((1 - x - y) / y)


chromaticityToLinearRgb : Chromaticity -> LinearRgb
chromaticityToLinearRgb chromaticity =
    cieXyzToLinearRgb (chromaticityToCieXyz chromaticity)
