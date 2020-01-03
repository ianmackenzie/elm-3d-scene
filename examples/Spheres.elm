module Spheres exposing (main)

import Angle
import Axis3d
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
import Scene3d.Drawable as Drawable exposing (Entity)
import Scene3d.Exposure as Exposure
import Scene3d.LightSource as LightSource exposing (LightSource)
import Scene3d.Mesh as Mesh exposing (Mesh, Yes)
import Scene3d.Shape as Shape
import Vector3d
import Viewpoint3d


type World
    = World


sphereMesh : Mesh World { hasNormals : Yes }
sphereMesh =
    Shape.sphere { radius = meters 1, subdivisions = 72 }


sphereShadow : Mesh.Shadow World
sphereShadow =
    Mesh.shadow sphereMesh


floorMesh : Mesh World { hasNormals : Yes }
floorMesh =
    Shape.block (meters 8) (meters 8) (meters 0.2)


floor : Entity World
floor =
    Drawable.physical Materials.aluminum floorMesh
        |> Drawable.translateBy (Vector3d.meters 0 0 -2)


goldSphere : Entity World
goldSphere =
    Drawable.physical Materials.gold sphereMesh
        |> Drawable.withShadow sphereShadow
        |> Drawable.translateBy (Vector3d.meters 2 2 0)


aluminumSphere : Entity World
aluminumSphere =
    Drawable.physical Materials.aluminum sphereMesh
        |> Drawable.withShadow sphereShadow
        |> Drawable.translateBy (Vector3d.meters 2 -2 0)


blackPlasticSphere : Entity World
blackPlasticSphere =
    Drawable.physical Materials.blackPlastic sphereMesh
        |> Drawable.withShadow sphereShadow
        |> Drawable.translateBy (Vector3d.meters -2 -2 0)


whitePlasticSphere : Entity World
whitePlasticSphere =
    Drawable.physical Materials.whitePlastic sphereMesh
        |> Drawable.withShadow sphereShadow
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


sunlight : LightSource World
sunlight =
    LightSource.directionalLight
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
