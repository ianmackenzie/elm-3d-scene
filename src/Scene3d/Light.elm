module Scene3d.Light exposing
    ( AmbientLighting
    , Light
    , directional
    , overcast
    , point
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
import Scene3d.Color exposing (LinearRgb(..))
import Scene3d.Types as Types
import Task exposing (Task)
import WebGL.Texture exposing (Texture)


type alias Light coordinates =
    Types.Light coordinates


type alias AmbientLighting coordinates =
    Types.AmbientLighting coordinates


{-| Good default value for zenith luminance: 5000 nits
-}
overcast :
    { zenithDirection : Direction3d coordinates
    , chromaticity : Chromaticity
    , zenithLuminance : Luminance
    }
    -> AmbientLighting coordinates
overcast { zenithDirection, chromaticity, zenithLuminance } =
    let
        { x, y, z } =
            Direction3d.unwrap zenithDirection

        (LinearRgb r g b) =
            Chromaticity.toLinearRgb chromaticity

        lz =
            Luminance.inNits zenithLuminance
    in
    Types.AmbientLighting <|
        Math.Matrix4.fromRecord
            { m11 = x
            , m21 = y
            , m31 = z
            , m41 = 1
            , m12 = r * lz
            , m22 = g * lz
            , m32 = b * lz
            , m42 = 0
            , m13 = 0
            , m23 = 0
            , m33 = 0
            , m43 = 0
            , m14 = 0
            , m24 = 0
            , m34 = 0
            , m44 = 0
            }



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


directional : Chromaticity -> Illuminance -> Direction3d coordinates -> Light coordinates
directional chromaticity illuminance direction =
    let
        { x, y, z } =
            Direction3d.unwrap (Direction3d.reverse direction)

        (LinearRgb r g b) =
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


point : Chromaticity -> LuminousFlux -> Point3d Meters coordinates -> Light coordinates
point chromaticity luminousFlux position =
    let
        (LinearRgb r g b) =
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
