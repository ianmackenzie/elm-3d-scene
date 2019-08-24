module Scene3d.Light exposing
    ( Light
    , directional
    , point
    )

import Direction3d exposing (Direction3d)
import Geometry.Interop.LinearAlgebra.Direction3d as Direction3d
import Geometry.Interop.LinearAlgebra.Point3d as Point3d
import Illuminance exposing (Illuminance)
import LuminousFlux exposing (LuminousFlux)
import Math.Vector3 exposing (vec3)
import Point3d exposing (Point3d)
import Scene3d.Chromaticity as Chromaticity exposing (Chromaticity)
import Scene3d.Types as Types
import Task exposing (Task)
import WebGL.Texture exposing (Texture)


type alias Light units coordinates =
    Types.Light units coordinates



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


directional : Chromaticity -> Illuminance -> Direction3d coordinates -> Light units coordinates
directional chromaticity illuminance direction =
    let
        { x, y, z } =
            Direction3d.unwrap (Direction3d.reverse direction)

        ( r, g, b ) =
            Chromaticity.toLinearRgb chromaticity

        lux =
            Illuminance.inLux illuminance
    in
    Types.Light
        { type_ = 1
        , x = x
        , y = y
        , z = z
        , r = r * lux
        , g = g * lux
        , b = b * lux
        , radius = 0
        }


point : Chromaticity -> LuminousFlux -> Point3d units coordinates -> Light units coordinates
point chromaticity luminousFlux position =
    let
        ( r, g, b ) =
            Chromaticity.toLinearRgb chromaticity

        lumens =
            LuminousFlux.inLumens luminousFlux

        { x, y, z } =
            Point3d.unwrap position
    in
    Types.Light
        { type_ = 2
        , x = x
        , y = y
        , z = z
        , r = r * lumens
        , g = g * lumens
        , b = b * lumens
        , radius = 0
        }
