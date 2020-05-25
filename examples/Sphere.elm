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
import Palette.Tango as Tango
import Parameter1d
import Pixels
import Point3d
import Quantity
import Scene3d
import Scene3d.Light as Light exposing (Light)
import Scene3d.Material as Material exposing (Material)
import Sphere3d
import Temperature
import Viewpoint3d exposing (Viewpoint3d)


type World
    = World


viewpoint : Viewpoint3d Meters World
viewpoint =
    Viewpoint3d.lookAt
        { focalPoint = Point3d.origin
        , eyePoint = Point3d.centimeters 20 10 20
        , upDirection = Direction3d.positiveZ
        }


sunlight : Light World Bool
sunlight =
    Light.directional (Light.castsShadows False)
        { chromaticity = Light.sunlight
        , intensity = Illuminance.lux 10000
        , direction = Direction3d.yz (Angle.degrees -120)
        }


camera : Camera3d Meters World
camera =
    Camera3d.perspective
        { viewpoint = viewpoint
        , verticalFieldOfView = Angle.degrees 30
        }


material : Material.Uniform coordinates
material =
    Material.nonmetal
        { baseColor = Tango.skyBlue2
        , roughness = 0.4
        }


overheadLighting =
    Light.overhead
        { upDirection = Direction3d.positiveZ
        , intensity = Illuminance.lux 15000
        , chromaticity = Light.daylight
        }


main : Html msg
main =
    Scene3d.custom
        { camera = camera
        , clipDepth = Length.centimeters 0.5
        , dimensions = ( Pixels.pixels 800, Pixels.pixels 600 )
        , antialiasing = Scene3d.multisampling
        , lights = Scene3d.twoLights sunlight overheadLighting
        , exposure = Scene3d.maxLuminance (Luminance.nits 5000)
        , toneMapping = Scene3d.noToneMapping
        , whiteBalance = Light.daylight
        , background = Scene3d.transparentBackground
        , entities =
            [ Scene3d.sphere (Material.uniform material) <|
                Sphere3d.withRadius (Length.centimeters 5) Point3d.origin
            ]
        }
