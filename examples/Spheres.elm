module Spheres exposing (main)

import Angle exposing (Angle)
import Array
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
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Length exposing (Meters, meters)
import Luminance
import LuminousFlux exposing (lumens)
import Pixels exposing (pixels)
import Point3d
import Quantity
import Scene3d
import Scene3d.Material as Material exposing (Material)
import Scene3d.Mesh as Mesh exposing (Mesh)
import SketchPlane3d exposing (SketchPlane3d)
import Sphere3d
import Vector3d
import Viewpoint3d


type World
    = World


floor : Scene3d.Entity World
floor =
    Scene3d.block (Scene3d.castsShadows False) Materials.aluminum <|
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
    Scene3d.sphere (Scene3d.castsShadows True) (Material.uniform Materials.gold) <|
        Sphere3d.withRadius (meters 1) (Point3d.meters 2 2 0)


aluminumSphere : Scene3d.Entity World
aluminumSphere =
    Scene3d.sphere (Scene3d.castsShadows True) (Material.uniform Materials.aluminum) <|
        Sphere3d.withRadius (meters 1) (Point3d.meters 2 -2 0)


blackPlasticSphere : Scene3d.Entity World
blackPlasticSphere =
    Scene3d.sphere (Scene3d.castsShadows True) (Material.uniform Materials.blackPlastic) <|
        Sphere3d.withRadius (meters 1) (Point3d.meters -2 -2 0)


whitePlasticSphere : Scene3d.Entity World
whitePlasticSphere =
    Scene3d.sphere (Scene3d.castsShadows True) (Material.uniform Materials.whitePlastic) <|
        Sphere3d.withRadius (meters 1) (Point3d.meters -2 2 0)


camera : Model -> Camera3d Meters World
camera model =
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.orbit
                { focalPoint = Point3d.meters 0 0 -2
                , groundPlane = SketchPlane3d.xy
                , azimuth = model.azimuth
                , elevation = model.elevation
                , distance = Length.meters 20
                }
        , verticalFieldOfView = Angle.degrees 30
        }


sunlight : Scene3d.Light World Bool
sunlight =
    Scene3d.directionalLight (Scene3d.castsShadows True)
        { chromaticity = Scene3d.sunlight
        , intensity = lux 20000
        , direction =
            Direction3d.negativeZ
                |> Direction3d.rotateAround Axis3d.x (Angle.degrees -30)
        }


lightBulb : Scene3d.Light World Bool
lightBulb =
    Scene3d.pointLight (Scene3d.castsShadows True)
        { chromaticity = Scene3d.incandescentLighting
        , intensity = LuminousFlux.lumens 3000000
        , position = Point3d.meters 0 0 3
        }


overheadLighting : Scene3d.Light World Never
overheadLighting =
    Scene3d.overheadLighting
        { upDirection = Direction3d.positiveZ
        , chromaticity = Scene3d.fluorescentLighting
        , intensity = Illuminance.lux 9000
        }


type ToneMapping
    = NoToneMapping
    | Reinhard
    | ReinhardPerChannel
    | HableFilmic


type alias Model =
    { exposureValue : Float
    , toneMapping : ToneMapping
    , azimuth : Angle
    , elevation : Angle
    , orbiting : Bool
    }


type Msg
    = SetExposureValue Float
    | SetToneMapping ToneMapping
    | MouseDown
    | MouseUp
    | MouseMove Float Float


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        SetExposureValue value ->
            ( { model | exposureValue = value }, Cmd.none )

        SetToneMapping value ->
            ( { model | toneMapping = value }, Cmd.none )

        MouseDown ->
            ( { model | orbiting = True }, Cmd.none )

        MouseUp ->
            ( { model | orbiting = False }, Cmd.none )

        MouseMove dx dy ->
            if model.orbiting then
                let
                    rotation numPixels =
                        Angle.degrees (0.25 * numPixels)

                    newAzimuth =
                        model.azimuth |> Quantity.minus (rotation dx)

                    newElevation =
                        model.elevation
                            |> Quantity.plus (rotation dy)
                            |> Quantity.clamp (Angle.degrees -90) (Angle.degrees 90)
                in
                ( { model | azimuth = newAzimuth, elevation = newElevation }
                , Cmd.none
                )

            else
                ( model, Cmd.none )


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


comboBox : List (Html.Attribute a) -> (a -> String) -> List a -> a -> Html a
comboBox attributes toStr allItems =
    let
        itemsArray =
            Array.fromList allItems

        selectedIndexDecoder =
            Decode.at [ "target", "selectedIndex" ] Decode.int

        newSelectionDecoder currentItem =
            selectedIndexDecoder
                |> Decode.andThen
                    (\selectedIndex ->
                        case Array.get selectedIndex itemsArray of
                            Just newItem ->
                                if newItem /= currentItem then
                                    Decode.succeed newItem

                                else
                                    Decode.fail "selected item did not change"

                            Nothing ->
                                Decode.fail "selected index out of range"
                    )
    in
    \currentItem ->
        let
            decoder =
                newSelectionDecoder currentItem

            onChange =
                Html.Events.on "change" decoder

            onKeyUp =
                Html.Events.on "keyup" decoder

            toOption item =
                Html.option [ Html.Attributes.selected (item == currentItem) ]
                    [ Html.text (toStr item) ]
        in
        Html.select (onChange :: onKeyUp :: attributes)
            (List.map toOption allItems)


toneMappingDescription : ToneMapping -> String
toneMappingDescription toneMapping =
    case toneMapping of
        NoToneMapping ->
            "No tone mapping"

        Reinhard ->
            "Reinhard"

        ReinhardPerChannel ->
            "Reinhard per channel"

        HableFilmic ->
            "Hable filmic"


toneMappingOptions : List ToneMapping
toneMappingOptions =
    [ NoToneMapping
    , Reinhard
    , ReinhardPerChannel
    , HableFilmic
    ]


init : () -> ( Model, Cmd Msg )
init () =
    ( { exposureValue = 14
      , toneMapping = NoToneMapping
      , azimuth = Angle.degrees 45
      , elevation = Angle.degrees 40
      , orbiting = False
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [ Html.Events.onMouseDown MouseDown ]
            [ Scene3d.toHtml
                { lights = Scene3d.twoLights lightBulb overheadLighting
                , camera = camera model
                , clipDepth = meters 0.1
                , dimensions = ( pixels 1024, pixels 768 )
                , antialiasing = Scene3d.multisampling
                , exposure = Scene3d.exposureValue model.exposureValue
                , toneMapping =
                    case model.toneMapping of
                        NoToneMapping ->
                            Scene3d.noToneMapping

                        Reinhard ->
                            Scene3d.reinhardToneMapping 4

                        ReinhardPerChannel ->
                            Scene3d.reinhardPerChannelToneMapping 4

                        HableFilmic ->
                            Scene3d.hableFilmicToneMapping
                , whiteBalance = Scene3d.fluorescentLighting
                , background = Scene3d.transparentBackground
                , entities =
                    [ goldSphere
                    , aluminumSphere
                    , blackPlasticSphere
                    , whitePlasticSphere
                    , floor
                    , Scene3d.quad (Scene3d.castsShadows True)
                        (Material.uniform Materials.aluminum)
                        (Point3d.meters 1 1 -0.5)
                        (Point3d.meters -1 1 0)
                        (Point3d.meters -1 -1 0.5)
                        (Point3d.meters 1 -1 0)
                    ]
                }
            ]
        , Html.div []
            [ Html.text "Exposure value:"
            , slider [] { min = 10, max = 18 } model.exposureValue
                |> Html.map SetExposureValue
            ]
        , Html.div []
            [ Html.text "Tone mapping:"
            , comboBox [] toneMappingDescription toneMappingOptions model.toneMapping
                |> Html.map SetToneMapping
            ]
        ]


decodeMouseMove : Decoder Msg
decodeMouseMove =
    Decode.map2 MouseMove
        (Decode.field "movementX" Decode.float)
        (Decode.field "movementY" Decode.float)


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.orbiting then
        Sub.batch
            [ Browser.Events.onMouseMove decodeMouseMove
            , Browser.Events.onMouseUp (Decode.succeed MouseUp)
            ]

    else
        Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
