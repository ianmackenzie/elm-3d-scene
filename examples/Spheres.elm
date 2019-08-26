module Spheres exposing (main)

import Angle
import Axis3d
import Camera3d exposing (Camera3d)
import Direction3d exposing (Direction3d)
import Html exposing (Html)
import Illuminance exposing (lux)
import Length exposing (Meters, meters)
import Luminance
import LuminousFlux exposing (lumens)
import Materials
import Pixels exposing (pixels)
import Point3d
import Scene3d
import Scene3d.Chromaticity as Chromaticity
import Scene3d.Drawable as Drawable exposing (Drawable)
import Scene3d.Exposure as Exposure
import Scene3d.Light as Light exposing (AmbientLighting, Light)
import Scene3d.Mesh as Mesh exposing (HasNormals, Mesh, NoTangents, NoUV)
import Scene3d.Shape as Shape
import Vector3d
import Viewpoint3d


type World
    = World


sphereMesh : Mesh World HasNormals NoUV NoTangents
sphereMesh =
    Shape.sphere { radius = meters 1, maxError = meters 0.001 }
        |> Mesh.withShadow


floorMesh : Mesh World HasNormals NoUV NoTangents
floorMesh =
    Shape.block (meters 8) (meters 8) (meters 0.2)


floor : Drawable World
floor =
    Drawable.physical Materials.aluminum floorMesh
        |> Drawable.translateBy (Vector3d.meters 0 0 -2)


goldSphere : Drawable World
goldSphere =
    Drawable.physical Materials.gold sphereMesh
        |> Drawable.translateBy (Vector3d.meters 2 2 0)


aluminumSphere : Drawable World
aluminumSphere =
    Drawable.physical Materials.aluminum sphereMesh
        |> Drawable.translateBy (Vector3d.meters 2 -2 0)


blackPlasticSphere : Drawable World
blackPlasticSphere =
    Drawable.physical Materials.blackPlastic sphereMesh
        |> Drawable.translateBy (Vector3d.meters -2 -2 0)


whitePlasticSphere : Drawable World
whitePlasticSphere =
    Drawable.physical Materials.whitePlastic sphereMesh
        |> Drawable.translateBy (Vector3d.meters -2 2 0)


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


sunlight : Light World
sunlight =
    Light.directional
        Chromaticity.daylight
        (lux 20000)
        (Direction3d.negativeZ
            |> Direction3d.rotateAround Axis3d.x (Angle.degrees -30)
        )


scene : Drawable World
scene =
    Drawable.group
        [ goldSphere
        , aluminumSphere
        , blackPlasticSphere
        , whitePlasticSphere
        , floor
        ]


ambientLighting : AmbientLighting World
ambientLighting =
    Light.overcast
        { zenithDirection = Direction3d.positiveZ
        , zenithLuminance = Luminance.nits 2000
        , chromaticity = Chromaticity.daylight
        }


main : Html msg
main =
    Scene3d.render
        { options = [ Scene3d.devicePixelRatio 2 ]
        , ambientLighting = Just ambientLighting
        , lights = Scene3d.oneLight sunlight { castsShadows = True }
        , camera = camera
        , screenWidth = pixels 1024
        , screenHeight = pixels 768
        , scene = scene
        , exposure = Exposure.fromEv100 14
        , whiteBalance = Chromaticity.daylight
        }
