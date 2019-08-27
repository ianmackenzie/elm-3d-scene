module Scene3d.Chromaticity exposing
    ( Chromaticity
    , daylight, incandescent
    , xy, srgb, fromTemperature, fromColor
    , toXyz, toLinearRgb
    )

{-|

@docs Chromaticity


## Constants

@docs daylight, incandescent, fluorescent


## Constructors

@docs xy, srgb, fromTemperature, fromColor


## Conversions

@docs toXyz, toLinearRgb

-}

import Color exposing (Color)
import Point2d exposing (Point2d)
import Quantity exposing (Unitless)
import Scene3d.Color as Color exposing (LinearRgb, Xyz(..))
import Temperature exposing (Temperature)


type Chromaticity
    = Chromaticity { x : Float, y : Float }


{-| Illuminant A, "Incandescent/Tungsten"
-}
incandescent : Chromaticity
incandescent =
    Chromaticity { x = 0.44757, y = 0.40745 }


{-| Illuminant D65, "Noon Daylight"
-}
daylight : Chromaticity
daylight =
    Chromaticity { x = 0.31271, y = 0.32902 }


{-| Illuminant F4, "Warm White Fluorescent"
-}
fluorescent : Chromaticity
fluorescent =
    Chromaticity { x = 0.44018, y = 0.40329 }


{-| See
<https://en.wikipedia.org/wiki/Standard_illuminant#White_points_of_standard_illuminants>
for several standard values
-}
xy : Float -> Float -> Chromaticity
xy x y =
    Chromaticity { x = x, y = y }


srgb : Float -> Float -> Float -> Chromaticity
srgb r g b =
    fromColor (Color.rgb r g b)


fromTemperature : Temperature -> Chromaticity
fromTemperature temperature =
    let
        t =
            clamp 1667 25000 (Temperature.inKelvins temperature)

        x =
            if t <= 4000 then
                (-0.2661239 * 1.0e9 / (t * t * t))
                    - (0.2343589 * 1.0e6 / (t * t))
                    + (0.8776956 * 1.0e3 / t)
                    + 0.17991

            else
                (-3.0258469 * 1.0e9 / (t * t * t))
                    + (2.1070379 * 1.0e6 / (t * t))
                    + (0.2226347 * 1.0e3 / t)
                    + 0.24039

        y =
            if t <= 2222 then
                (-1.1063814 * (x * x * x))
                    - (1.3481102 * (x * x))
                    + (2.18555832 * x)
                    - 0.20219683

            else if t <= 4000 then
                (-0.9549476 * (x * x * x))
                    - (1.37418593 * (x * x))
                    + (2.09137015 * x)
                    - 0.16748867

            else
                (3.081758 * (x * x * x))
                    - (5.8733867 * (x * x))
                    + (3.75112997 * x)
                    - 0.37001483
    in
    Chromaticity { x = x, y = y }


fromColor : Color -> Chromaticity
fromColor color =
    let
        (Xyz bigX bigY bigZ) =
            Color.toXyz color

        sum =
            bigX + bigY + bigZ
    in
    Chromaticity
        { x = bigX / sum
        , y = bigY / sum
        }


toXyz : Chromaticity -> Xyz
toXyz (Chromaticity { x, y }) =
    Xyz (x / y) 1 ((1 - x - y) / y)


toLinearRgb : Chromaticity -> LinearRgb
toLinearRgb chromaticity =
    Color.xyzToLinearRgb (toXyz chromaticity)
