module Spheres exposing (main)

import Angle
import Axis3d
import Block3d
import Camera3d exposing (Camera3d)
import Common.Materials as Materials
import Direction3d exposing (Direction3d)
import Html exposing (Html)
import Illuminance exposing (lux)
import Length exposing (Meters, meters)
import Luminance
import LuminousFlux exposing (lumens)
import Pixels exposing (pixels)
import Point3d
import Scene3d
import Scene3d.Chromaticity as Chromaticity
import Scene3d.Exposure as Exposure
import Scene3d.Mesh as Mesh exposing (Mesh)
import Sphere3d
import Vector3d
import Viewpoint3d


type World
    = World


floor : Scene3d.Entity World
floor =
    Scene3d.block
        (Block3d.with
            { x1 = meters -4
            , x2 = meters 4
            , y1 = meters -4
            , y2 = meters 4
            , z1 = meters -2.2
            , z2 = meters -2
            }
        )
        Materials.aluminum
        { castsShadow = False }


goldSphere : Scene3d.Entity World
goldSphere =
    Scene3d.sphere
        (Sphere3d.withRadius (meters 1) (Point3d.meters 2 2 0))
        Materials.gold
        { castsShadow = True }


aluminumSphere : Scene3d.Entity World
aluminumSphere =
    Scene3d.sphere
        (Sphere3d.withRadius (meters 1) (Point3d.meters 2 -2 0))
        Materials.aluminum
        { castsShadow = True }


blackPlasticSphere : Scene3d.Entity World
blackPlasticSphere =
    Scene3d.sphere
        (Sphere3d.withRadius (meters 1) (Point3d.meters -2 -2 0))
        Materials.blackPlastic
        { castsShadow = True }


whitePlasticSphere : Scene3d.Entity World
whitePlasticSphere =
    Scene3d.sphere
        (Sphere3d.withRadius (meters 1) (Point3d.meters -2 2 0))
        Materials.whitePlastic
        { castsShadow = True }


camera : Camera3d Meters World
camera =
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.lookAt
                { focalPoint = Point3d.meters 0 0 -2
                , eyePoint = Point3d.meters 10 10 10
                , upDirection = Direction3d.positiveZ
                }
        , verticalFieldOfView = Angle.degrees 30
        , clipDepth = meters 0.1
        }


sunlight : Scene3d.LightSource World
sunlight =
    Scene3d.directionalLight
        Chromaticity.d65
        (lux 20000)
        (Direction3d.negativeZ
            |> Direction3d.rotateAround Axis3d.x (Angle.degrees -30)
        )


environmentalLighting : Scene3d.EnvironmentalLighting World
environmentalLighting =
    Scene3d.softLighting
        { upDirection = Direction3d.positiveZ
        , above = ( Luminance.nits 3000, Chromaticity.d65 )
        , below = ( Luminance.nits 0, Chromaticity.d65 )
        }


main : Html msg
main =
    Scene3d.toHtml
        { environmentalLighting = environmentalLighting
        , directLighting = Scene3d.oneLightSource sunlight { castsShadows = True }
        , camera = camera
        , width = pixels 1024
        , height = pixels 768
        , exposure = Exposure.fromEv100 14
        , whiteBalance = Chromaticity.d65
        , backgroundColor = Scene3d.transparentBackground
        }
        [ goldSphere
        , aluminumSphere
        , blackPlasticSphere
        , whitePlasticSphere
        , floor
        ]
