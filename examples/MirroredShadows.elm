module MirroredShadows exposing (main)

import Angle
import Axis3d
import Block3d
import Browser
import Browser.Events
import Camera3d exposing (Camera3d)
import Direction3d exposing (Direction3d)
import Html exposing (Html)
import Illuminance exposing (lux)
import Json.Decode as Decode
import Length exposing (Meters, meters)
import Luminance
import LuminousFlux exposing (lumens)
import Palette.Tango as Tango
import Pixels exposing (pixels)
import Plane3d
import Point3d
import Html.Attributes
import Scene3d
import Scene3d.Chromaticity as Chromaticity
import Scene3d.Exposure as Exposure
import Scene3d.Material as Material exposing (Material)
import Scene3d.Mesh as Mesh exposing (Mesh)
import Sphere3d
import Vector3d
import Viewpoint3d


type World
    = World


objectMaterial : Material { a | normals : () }
objectMaterial =
    Material.nonmetal { baseColor = Tango.skyBlue1, roughness = 0.4 }


floorMaterial : Material { a | normals : () }
floorMaterial =
    Material.diffuse Tango.chameleon1


floor : Scene3d.Entity World
floor =
    Scene3d.block Scene3d.doesNotCastShadows floorMaterial <|
        Block3d.from (Point3d.meters -7.5 -7.5 -0.2) (Point3d.meters 7.5 7.5 0)


initialBlock : Scene3d.Entity World
initialBlock =
    Scene3d.block Scene3d.castsShadows objectMaterial <|
        Block3d.from (Point3d.meters 1 1 1) (Point3d.meters 2.5 2.5 2.5)


initialSphere : Scene3d.Entity World
initialSphere =
    Scene3d.sphere Scene3d.castsShadows objectMaterial <|
        Sphere3d.withRadius (Length.meters 1) (Point3d.meters 4 1.5 1.5)


initialQuad : Scene3d.Entity World
initialQuad =
    Scene3d.quad Scene3d.castsShadows
        objectMaterial
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
                { focalPoint = Point3d.origin
                , eyePoint = Point3d.meters 20 20 20
                , upDirection = Direction3d.positiveZ
                }
        , verticalFieldOfView = Angle.degrees 30
        , clipDepth = meters 0.1
        }


sunlight : Scene3d.LightSource World (Scene3d.CastsShadows Scene3d.Yes)
sunlight =
    Scene3d.directionalLight Scene3d.castsShadows
        { chromaticity = Chromaticity.d65
        , intensity = lux 20000
        , direction =
            Direction3d.xyZ (Angle.degrees -90) (Angle.degrees -60)
        }


lightBulb : Scene3d.LightSource World (Scene3d.CastsShadows Scene3d.Yes)
lightBulb =
    Scene3d.pointLight Scene3d.castsShadows
        { chromaticity = Chromaticity.d65
        , intensity = LuminousFlux.lumens 10000000
        , position = Point3d.meters 0 0 5
        }


environmentalLighting : Scene3d.EnvironmentalLighting World
environmentalLighting =
    Scene3d.softLighting
        { upDirection = Direction3d.positiveZ
        , above = ( Luminance.nits 3000, Chromaticity.d65 )
        , below = ( Luminance.nits 0, Chromaticity.d65 )
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
                        [ Scene3d.toHtml
                            { environmentalLighting = environmentalLighting
                            , directLighting =
                                Scene3d.oneLightSource <|
                                    if usePointLight then
                                        lightBulb

                                    else
                                        sunlight
                            , camera = camera
                            , width = pixels 1024
                            , height = pixels 768
                            , exposure = Exposure.fromEv100 14
                            , whiteBalance = Chromaticity.d65
                            , backgroundColor = Scene3d.transparentBackground
                            }
                            [ floor
                            , initialGroup
                            , initialGroup |> Scene3d.mirrorAcross Plane3d.zx
                            , initialGroup |> Scene3d.rotateAround Axis3d.z (Angle.degrees 75)
                            , initialGroup
                                |> Scene3d.mirrorAcross Plane3d.zx
                                |> Scene3d.mirrorAcross Plane3d.yz
                            ]
                        ]
                    ]
        , subscriptions = always (Browser.Events.onClick (Decode.succeed ()))
        }
