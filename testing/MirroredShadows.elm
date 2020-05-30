module MirroredShadows exposing (main)

import Angle
import Axis3d
import Block3d
import Browser
import Browser.Events
import Camera3d exposing (Camera3d)
import Color
import Direction3d exposing (Direction3d)
import Html exposing (Html)
import Html.Attributes
import Illuminance exposing (lux)
import Json.Decode as Decode
import Length exposing (Meters, meters)
import Luminance
import LuminousFlux exposing (lumens)
import Pixels exposing (pixels)
import Plane3d
import Point3d
import Scene3d
import Scene3d.Light as Light exposing (Light)
import Scene3d.Material as Material exposing (Material)
import Scene3d.Mesh as Mesh exposing (Mesh)
import Sphere3d
import Vector3d
import Viewpoint3d


type World
    = World


objectMaterial : Material.Uniform coordinates
objectMaterial =
    Material.nonmetal { baseColor = Color.lightBlue, roughness = 0.4 }


floorMaterial : Material.Uniform coordinates
floorMaterial =
    Material.matte Color.lightGreen


floor : Scene3d.Entity World
floor =
    Scene3d.block floorMaterial <|
        Block3d.from (Point3d.meters -7.5 -7.5 -0.2) (Point3d.meters 7.5 7.5 0)


initialBlock : Scene3d.Entity World
initialBlock =
    Scene3d.blockWithShadow objectMaterial <|
        Block3d.from (Point3d.meters 1 1 1) (Point3d.meters 2.5 2.5 2.5)


initialSphere : Scene3d.Entity World
initialSphere =
    Scene3d.sphereWithShadow (Material.uniform objectMaterial) <|
        Sphere3d.withRadius (Length.meters 1) (Point3d.meters 4 1.5 1.5)


initialQuad : Scene3d.Entity World
initialQuad =
    Scene3d.quadWithShadow (Material.uniform objectMaterial)
        (Point3d.meters 1 3.5 1)
        (Point3d.meters 2.5 3.5 1)
        (Point3d.meters 2.5 5 1)
        (Point3d.meters 1 5 1)


initialGroup : Scene3d.Entity World
initialGroup =
    Scene3d.group [ initialBlock, initialSphere, initialQuad ]
        |> Scene3d.translateBy (Vector3d.meters 0.25 0.25 0)


camera : Camera3d Meters World
camera =
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.lookAt
                { focalPoint = Point3d.meters 0 0 -2.5
                , eyePoint = Point3d.meters 16 16 16
                , upDirection = Direction3d.positiveZ
                }
        , verticalFieldOfView = Angle.degrees 30
        }


sunlight : Light World Bool
sunlight =
    Light.directional (Light.castsShadows True)
        { chromaticity = Light.daylight
        , intensity = lux 20000
        , direction =
            Direction3d.xyZ (Angle.degrees -90) (Angle.degrees -60)
        }


lightBulb : Light World Bool
lightBulb =
    Light.point (Light.castsShadows True)
        { chromaticity = Light.daylight
        , intensity = LuminousFlux.lumens 10000000
        , position = Point3d.meters 0 0 5
        }


daylight : Light World Never
daylight =
    Light.overhead
        { upDirection = Direction3d.positiveZ
        , chromaticity = Light.daylight
        , intensity = Illuminance.lux 9000
        }


main : Program () Bool ()
main =
    Browser.element
        { init = always ( True, Cmd.none )
        , update =
            \() usePointLight -> ( not usePointLight, Cmd.none )
        , view =
            \usePointLight ->
                Html.div []
                    [ Html.span [ Html.Attributes.style "user-select" "none" ]
                        [ Html.text "Click to toggle between point and directional light" ]
                    , Html.div []
                        [ Scene3d.custom
                            { lights =
                                --Scene3d.oneLight (Light.ambient { chromaticity = Light.daylight, intensity = Illuminance.lux 9000 })
                                Scene3d.twoLights
                                    (if usePointLight then
                                        lightBulb

                                     else
                                        sunlight
                                    )
                                    daylight
                            , camera = camera
                            , clipDepth = meters 0.1
                            , antialiasing = Scene3d.multisampling
                            , dimensions = ( pixels 480, pixels 320 )
                            , exposure = Scene3d.exposureValue 14
                            , toneMapping = Scene3d.noToneMapping
                            , whiteBalance = Light.daylight
                            , background = Scene3d.transparentBackground
                            , entities =
                                [ floor
                                , initialGroup
                                , initialGroup |> Scene3d.mirrorAcross Plane3d.zx
                                , initialGroup |> Scene3d.rotateAround Axis3d.z (Angle.degrees 75)
                                , initialGroup
                                    |> Scene3d.mirrorAcross Plane3d.zx
                                    |> Scene3d.mirrorAcross Plane3d.yz
                                ]
                            }
                        ]
                    ]
        , subscriptions = always (Browser.Events.onClick (Decode.succeed ()))
        }
