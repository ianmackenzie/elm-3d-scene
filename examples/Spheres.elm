module Spheres exposing (main)

import Angle
import Axis3d
import Block3d
import Browser
import Browser.Events
import Camera3d exposing (Camera3d)
import Common.Materials as Materials
import Direction3d exposing (Direction3d)
import Html exposing (Html)
import Html.Attributes
import Illuminance exposing (lux)
import Json.Decode as Decode
import Length exposing (Meters, meters)
import Luminance
import LuminousFlux exposing (lumens)
import Pixels exposing (pixels)
import Point3d
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


floor : Scene3d.Entity World
floor =
    Scene3d.block Scene3d.doesNotCastShadows Materials.aluminum <|
        Block3d.with
            { x1 = meters -4
            , x2 = meters 4
            , y1 = meters -4
            , y2 = meters 4
            , z1 = meters -2.2
            , z2 = meters -2
            }


goldSphere : Scene3d.Entity World
goldSphere =
    Scene3d.sphere Scene3d.castsShadows (Material.uniform Materials.gold) <|
        Sphere3d.withRadius (meters 1) (Point3d.meters 2 2 0)


aluminumSphere : Scene3d.Entity World
aluminumSphere =
    Scene3d.sphere Scene3d.castsShadows (Material.uniform Materials.aluminum) <|
        Sphere3d.withRadius (meters 1) (Point3d.meters 2 -2 0)


blackPlasticSphere : Scene3d.Entity World
blackPlasticSphere =
    Scene3d.sphere Scene3d.castsShadows (Material.uniform Materials.blackPlastic) <|
        Sphere3d.withRadius (meters 1) (Point3d.meters -2 -2 0)


whitePlasticSphere : Scene3d.Entity World
whitePlasticSphere =
    Scene3d.sphere Scene3d.castsShadows (Material.uniform Materials.whitePlastic) <|
        Sphere3d.withRadius (meters 1) (Point3d.meters -2 2 0)


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


sunlight : Scene3d.LightSource World (Scene3d.CastsShadows Scene3d.Yes)
sunlight =
    Scene3d.directionalLight Scene3d.castsShadows
        { chromaticity = Chromaticity.d65
        , intensity = lux 20000
        , direction =
            Direction3d.negativeZ
                |> Direction3d.rotateAround Axis3d.x (Angle.degrees -30)
        }


lightBulb : Scene3d.LightSource World (Scene3d.CastsShadows Scene3d.Yes)
lightBulb =
    Scene3d.pointLight Scene3d.castsShadows
        { chromaticity = Chromaticity.d65
        , intensity = LuminousFlux.lumens 3000000
        , position = Point3d.meters 0 0 3
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
                            , background = Scene3d.transparentBackground
                            }
                            [ goldSphere
                            , aluminumSphere
                            , blackPlasticSphere
                            , whitePlasticSphere
                            , floor
                            , Scene3d.quad Scene3d.castsShadows
                                (Material.uniform Materials.aluminum)
                                (Point3d.meters 1 1 -0.5)
                                (Point3d.meters -1 1 0)
                                (Point3d.meters -1 -1 0.5)
                                (Point3d.meters 1 -1 0)
                            ]
                        ]
                    ]
        , subscriptions = always (Browser.Events.onClick (Decode.succeed ()))
        }
