module Scene3d.Chromaticity exposing
    ( Chromaticity
    , sunlight, incandescent, tungsten, fluorescent, d65
    , xy, kelvins, fromTemperature, fromColor
    )

{-|

@docs Chromaticity


## Constants

@docs sunlight, incandescent, tungsten, fluorescent, d65


## Constructors

@docs xy, kelvins, fromTemperature, fromColor

-}

import Color exposing (Color)
import Point2d exposing (Point2d)
import Quantity exposing (Unitless)
import Scene3d.ColorConversions as ColorConversions
import Scene3d.Types as Types exposing (CieXyz(..), LinearRgb(..))
import Temperature exposing (Temperature)


type alias Chromaticity =
    Types.Chromaticity


{-| Illuminant A, "Incandescent/Tungsten"
-}
incandescent : Chromaticity
incandescent =
    xy 0.44757 0.40745


{-| Synonym for `incandescent`.
-}
tungsten : Chromaticity
tungsten =
    incandescent


{-| Illuminant D65, "Noon Daylight"
-}
d65 : Chromaticity
d65 =
    xy 0.31271 0.32902


{-| 'Sunlight' film apparently calibrated to 5600 K; falls at low end of
'vertical daylight' at <https://en.wikipedia.org/wiki/Color_temperature>, above
'horizon daylight' (so probably a decent representative value)
-}
sunlight : Chromaticity
sunlight =
    kelvins 5600


{-| Illuminant F4, "Warm White Fluorescent"
-}
fluorescent : Chromaticity
fluorescent =
    xy 0.44018 0.40329


{-| See
<https://en.wikipedia.org/wiki/Standard_illuminant#White_points_of_standard_illuminants>
for several standard values
-}
xy : Float -> Float -> Chromaticity
xy x y =
    Types.Chromaticity { x = x, y = y }


kelvins : Float -> Chromaticity
kelvins numKelvins =
    fromTemperature (Temperature.kelvins numKelvins)


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
    xy x y


fromColor : Color -> Chromaticity
fromColor color =
    let
        (CieXyz bigX bigY bigZ) =
            ColorConversions.colorToCieXyz color

        sum =
            bigX + bigY + bigZ
    in
    xy (bigX / sum) (bigY / sum)
