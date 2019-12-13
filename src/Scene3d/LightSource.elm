module Scene3d.LightSource exposing
    ( LightSource
    , directionalLight
    , pointLight
    )

import Direction3d exposing (Direction3d)
import Geometry.Interop.LinearAlgebra.Direction3d as Direction3d
import Geometry.Interop.LinearAlgebra.Point3d as Point3d
import Illuminance exposing (Illuminance)
import Length exposing (Meters)
import Luminance exposing (Luminance)
import LuminousFlux exposing (LuminousFlux)
import Math.Matrix4
import Math.Vector3 exposing (vec3)
import Point3d exposing (Point3d)
import Scene3d.Chromaticity as Chromaticity exposing (Chromaticity)
import Scene3d.ColorConversions as ColorConversions
import Scene3d.Types as Types exposing (LinearRgb(..))
import Task exposing (Task)
import WebGL.Texture exposing (Texture)


type alias LightSource coordinates =
    Types.LightSource coordinates



-- ## Lights
--
-- type:
--   0 : disabled
--   1 : directional (XYZ is direction to light, i.e. reversed light direction)
--   2 : point (XYZ is light position)
--
-- radius is unused for now (will hopefully add sphere lights in the future)
--
-- [ x_i     r_i       x_j     r_j      ]
-- [ y_i     g_i       y_j     g_j      ]
-- [ z_i     b_i       z_j     b_j      ]
-- [ type_i  radius_i  type_j  radius_j ]


directionalLight : Chromaticity -> Illuminance -> Direction3d coordinates -> LightSource coordinates
directionalLight chromaticity illuminance direction =
    let
        { x, y, z } =
            Direction3d.unwrap direction

        (LinearRgb rgb) =
            ColorConversions.chromaticityToLinearRgb chromaticity

        lux =
            Illuminance.inLux illuminance
    in
    Types.LightSource
        { type_ = 1
        , x = -x
        , y = -y
        , z = -z
        , r = lux * Math.Vector3.getX rgb
        , g = lux * Math.Vector3.getY rgb
        , b = lux * Math.Vector3.getZ rgb
        , radius = 0
        }


pointLight : Chromaticity -> LuminousFlux -> Point3d Meters coordinates -> LightSource coordinates
pointLight chromaticity luminousFlux position =
    let
        (LinearRgb rgb) =
            ColorConversions.chromaticityToLinearRgb chromaticity

        lumens =
            LuminousFlux.inLumens luminousFlux

        { x, y, z } =
            Point3d.unwrap position
    in
    Types.LightSource
        { type_ = 2
        , x = x
        , y = y
        , z = z
        , r = lumens * Math.Vector3.getX rgb
        , g = lumens * Math.Vector3.getY rgb
        , b = lumens * Math.Vector3.getZ rgb
        , radius = 0
        }
