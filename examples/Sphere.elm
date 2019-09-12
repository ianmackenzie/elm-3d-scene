module Sphere exposing (main)

import Angle exposing (Angle)
import Camera3d exposing (Camera3d)
import Color
import Direction3d
import Html exposing (Html)
import Illuminance
import Length exposing (Meters)
import Luminance
import LuminousFlux
import Parameter1d
import Pixels
import Point3d
import Quantity
import Scene3d
import Scene3d.Chromaticity as Chromaticity
import Scene3d.Drawable as Drawable
import Scene3d.Exposure as Exposure
import Scene3d.Light as Light
import Scene3d.Mesh as Mesh exposing (Mesh, NoTangents, NoUV, ShadowsDisabled, Triangles, WithNormals)
import Scene3d.Shape as Shape
import Temperature
import Viewpoint3d exposing (Viewpoint3d)


type World
    = World


mesh : Mesh World (Triangles WithNormals NoUV NoTangents ShadowsDisabled)
mesh =
    Shape.sphere
        { radius = Length.centimeters 5
        , subdivisions = 72
        }


viewpoint : Viewpoint3d Meters World
viewpoint =
    Viewpoint3d.lookAt
        { focalPoint = Point3d.origin
        , eyePoint = Point3d.centimeters 20 10 20
        , upDirection = Direction3d.positiveZ
        }


sunlight =
    Light.directional
        Chromaticity.daylight
        (Illuminance.lux 10000)
        (Direction3d.yz (Angle.degrees -120))


ambientLighting =
    Light.overcast
        { chromaticity = Chromaticity.daylight
        , zenithDirection = Direction3d.positiveZ
        , zenithLuminance = Luminance.nits 5000
        }


camera : Camera3d Meters World
camera =
    Camera3d.perspective
        { viewpoint = viewpoint
        , verticalFieldOfView = Angle.degrees 30
        , clipDepth = Length.centimeters 0.5
        }


material =
    { baseColor = Color.blue
    , roughness = 0.2
    , metallic = False
    }


main : Html msg
main =
    Scene3d.render []
        { camera = camera
        , width = Pixels.pixels 800
        , height = Pixels.pixels 600
        , ambientLighting = Just ambientLighting
        , lights =
            Scene3d.oneLight sunlight { castsShadows = False }
        , exposure =
            Exposure.fromMaxLuminance
                (Luminance.nits 5000)
        , whiteBalance = Chromaticity.daylight
        }
        [ Drawable.lambertian material.baseColor mesh ]
