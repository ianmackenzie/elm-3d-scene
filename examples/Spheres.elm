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
import Html.Events
import Illuminance exposing (lux)
import Json.Decode as Decode
import Json.Encode as Encode
import Length exposing (Meters, meters)
import Luminance
import LuminousFlux exposing (lumens)
import Pixels exposing (pixels)
import Point3d
import Scene3d
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
        }


sunlight : Scene3d.Light World (Scene3d.CastsShadows Scene3d.Yes)
sunlight =
    Scene3d.directionalLight Scene3d.castsShadows
        { chromaticity = Scene3d.sunlight
        , intensity = lux 20000
        , direction =
            Direction3d.negativeZ
                |> Direction3d.rotateAround Axis3d.x (Angle.degrees -30)
        }


lightBulb : Scene3d.Light World (Scene3d.CastsShadows Scene3d.Yes)
lightBulb =
    Scene3d.pointLight Scene3d.castsShadows
        { chromaticity = Scene3d.incandescentLighting
        , intensity = LuminousFlux.lumens 3000000
        , position = Point3d.meters 0 0 3
        }


overheadLighting : Scene3d.Light World (Scene3d.CastsShadows Scene3d.No)
overheadLighting =
    Scene3d.overheadLighting
        { upDirection = Direction3d.positiveZ
        , chromaticity = Scene3d.fluorescentLighting
        , intensity = Illuminance.lux 9000
        }


type Antialiasing
    = NoAntialiasing
    | Multisampling
    | Supersampling


type alias Model =
    { antialiasing : Antialiasing
    , ev100 : Float
    , dynamicRange : Float
    }


type Msg
    = ToggleAntialiasing
    | SetEv100 Float
    | SetDynamicRange Float


update : Msg -> Model -> Model
update message model =
    case message of
        ToggleAntialiasing ->
            { model
                | antialiasing =
                    case model.antialiasing of
                        NoAntialiasing ->
                            Multisampling

                        Multisampling ->
                            Supersampling

                        Supersampling ->
                            NoAntialiasing
            }

        SetEv100 value ->
            { model | ev100 = value }

        SetDynamicRange value ->
            { model | dynamicRange = value }


slider :
    List (Html.Attribute Float)
    -> { min : Float, max : Float }
    -> Float
    -> Html Float
slider attributes { min, max } =
    let
        targetValueDecoder currentValue =
            Decode.map (String.toFloat >> Maybe.withDefault currentValue)
                Html.Events.targetValue

        newValueDecoder currentValue =
            targetValueDecoder currentValue
                |> Decode.andThen
                    (\newValue ->
                        if newValue /= currentValue then
                            Decode.succeed newValue

                        else
                            Decode.fail "value did not change"
                    )

        commonAttributes =
            Html.Attributes.property "min" (Encode.float min)
                :: Html.Attributes.property "max" (Encode.float max)
                :: Html.Attributes.property "step" (Encode.string "any")
                :: attributes
    in
    \currentValue ->
        Html.input
            (Html.Attributes.type_ "range"
                :: Html.Attributes.property "value" (Encode.float currentValue)
                :: Html.Events.on "input" (newValueDecoder currentValue)
                :: Html.Events.on "change" (newValueDecoder currentValue)
                :: commonAttributes
            )
            []


antialiasString : Antialiasing -> String
antialiasString antialiasing =
    case antialiasing of
        NoAntialiasing ->
            "No antialiasing"

        Multisampling ->
            "Multisampling"

        Supersampling ->
            "Supersampling"


main : Program () Model Msg
main =
    Browser.element
        { init = always ( { antialiasing = Multisampling, ev100 = 14, dynamicRange = 1 }, Cmd.none )
        , update = \message model -> ( update message model, Cmd.none )
        , view =
            \{ antialiasing, ev100, dynamicRange } ->
                Html.div []
                    [ Html.div []
                        [ Scene3d.toHtml
                            [ Scene3d.multisampling (antialiasing == Multisampling)
                            , Scene3d.supersampling <|
                                if antialiasing == Supersampling then
                                    2

                                else
                                    1
                            ]
                            { lights = Scene3d.twoLights lightBulb overheadLighting
                            , camera = camera
                            , clipDepth = meters 0.1
                            , dimensions = ( pixels 1024, pixels 768 )
                            , exposure = Scene3d.exposureValue ev100
                            , toneMapping = Scene3d.reinhardToneMapping dynamicRange
                            , whiteBalance = Scene3d.fluorescentLighting
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
                    , Html.div []
                        [ Html.text "EV100:"
                        , slider [] { min = 10, max = 18 } ev100
                            |> Html.map SetEv100
                        ]
                    , Html.div []
                        [ Html.text "Dynamic range:"
                        , slider [] { min = 1, max = 10 } dynamicRange
                            |> Html.map SetDynamicRange
                        ]
                    , Html.button [ Html.Events.onClick ToggleAntialiasing ] [ Html.text (antialiasString antialiasing) ]
                    ]
        , subscriptions = always Sub.none
        }
