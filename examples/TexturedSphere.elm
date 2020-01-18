module TexturedSphere exposing (main)

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Browser
import Browser.Events
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Direction3d
import Frame3d exposing (Frame3d)
import Html exposing (Html)
import Illuminance
import Json.Decode as Decode exposing (Decoder)
import Length exposing (Meters)
import Luminance
import LuminousFlux
import Palette.Tango as Tango
import Parameter1d
import Pixels
import Point3d
import Quantity
import Scene3d
import Scene3d.Chromaticity as Chromaticity
import Scene3d.Exposure as Exposure
import Scene3d.Material as Material exposing (Material)
import Sphere3d
import Task
import Temperature
import Vector3d exposing (Vector3d)
import Viewpoint3d exposing (Viewpoint3d)
import WebGL.Texture


type SphereCoordinates
    = SphereCoordinates


type WorldCoordinates
    = WorldCoordinates


type Model
    = Loading
        { colorTexture : Maybe (Material.Channel Color)
        , metallicTexture : Maybe (Material.Channel Float)
        , roughnessTexture : Maybe (Material.Channel Float)
        }
    | Loaded
        { colorTexture : Material.Channel Color
        , metallicTexture : Material.Channel Float
        , roughnessTexture : Material.Channel Float
        , sphereFrame : Frame3d Meters WorldCoordinates { defines : SphereCoordinates }
        , orbiting : Bool
        }
    | Errored String


type Msg
    = GotColorTexture (Result WebGL.Texture.Error (Material.Channel Color))
    | GotMetallicTexture (Result WebGL.Texture.Error (Material.Channel Float))
    | GotRoughnessTexture (Result WebGL.Texture.Error (Material.Channel Float))
    | MouseDown
    | MouseUp
    | MouseMove Float Float


init : ( Model, Cmd Msg )
init =
    ( Loading { colorTexture = Nothing, metallicTexture = Nothing, roughnessTexture = Nothing }
    , Cmd.batch
        [ Material.load "https://ianmackenzie.github.io/elm-3d-scene/examples/metal/Metal03_col.jpg"
            |> Task.attempt GotColorTexture
        , Material.load "https://ianmackenzie.github.io/elm-3d-scene/examples/metal/Metal03_rgh.jpg"
            |> Task.attempt GotRoughnessTexture
        , Material.load "https://ianmackenzie.github.io/elm-3d-scene/examples/metal/Metal03_met.jpg"
            |> Task.attempt GotMetallicTexture
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    let
        updatedModel =
            case model of
                Loading textures ->
                    case message of
                        GotColorTexture (Ok colorChannel) ->
                            checkIfLoaded { textures | colorTexture = Just colorChannel }

                        GotMetallicTexture (Ok metallicChannel) ->
                            checkIfLoaded { textures | metallicTexture = Just metallicChannel }

                        GotRoughnessTexture (Ok roughnessChannel) ->
                            checkIfLoaded { textures | roughnessTexture = Just roughnessChannel }

                        GotColorTexture (Err error) ->
                            Errored "Error loading texture"

                        GotMetallicTexture (Err error) ->
                            Errored "Error loading texture"

                        GotRoughnessTexture (Err error) ->
                            Errored "Error loading texture"

                        MouseDown ->
                            model

                        MouseUp ->
                            model

                        MouseMove _ _ ->
                            model

                Loaded loadedModel ->
                    case message of
                        GotColorTexture _ ->
                            model

                        GotMetallicTexture _ ->
                            model

                        GotRoughnessTexture _ ->
                            model

                        MouseDown ->
                            Loaded { loadedModel | orbiting = True }

                        MouseUp ->
                            Loaded { loadedModel | orbiting = False }

                        MouseMove dx dy ->
                            if loadedModel.orbiting then
                                let
                                    rotationVector =
                                        Vector3d.withLength (Angle.degrees dx)
                                            (Viewpoint3d.yDirection viewpoint)
                                            |> Vector3d.plus
                                                (Vector3d.withLength (Angle.degrees dy)
                                                    (Viewpoint3d.xDirection viewpoint)
                                                )
                                in
                                case Vector3d.direction rotationVector of
                                    Just direction ->
                                        let
                                            newFrame =
                                                loadedModel.sphereFrame
                                                    |> Frame3d.rotateAround
                                                        (Axis3d.through (Frame3d.originPoint loadedModel.sphereFrame) direction)
                                                        (Vector3d.length rotationVector)
                                        in
                                        Loaded { loadedModel | sphereFrame = newFrame }

                                    Nothing ->
                                        model

                            else
                                model

                Errored _ ->
                    model
    in
    ( updatedModel, Cmd.none )


checkIfLoaded :
    { colorTexture : Maybe (Material.Channel Color)
    , metallicTexture : Maybe (Material.Channel Float)
    , roughnessTexture : Maybe (Material.Channel Float)
    }
    -> Model
checkIfLoaded textures =
    case ( textures.colorTexture, textures.metallicTexture, textures.roughnessTexture ) of
        ( Just colorTexture, Just metallicTexture, Just roughnessTexture ) ->
            Loaded
                { colorTexture = colorTexture
                , metallicTexture = metallicTexture
                , roughnessTexture = roughnessTexture
                , sphereFrame = Frame3d.atOrigin
                , orbiting = False
                }

        _ ->
            Loading textures


viewpoint : Viewpoint3d Meters WorldCoordinates
viewpoint =
    Viewpoint3d.lookAt
        { focalPoint = Point3d.origin
        , eyePoint = Point3d.centimeters 20 10 10
        , upDirection = Direction3d.positiveZ
        }


sunlight =
    Scene3d.directionalLight
        Chromaticity.sunlight
        (Illuminance.lux 20000)
        (Direction3d.yz (Angle.degrees -120))


camera : Camera3d Meters WorldCoordinates
camera =
    Camera3d.perspective
        { viewpoint = viewpoint
        , verticalFieldOfView = Angle.degrees 30
        , clipDepth = Length.centimeters 0.5
        }


view : Model -> Html msg
view model =
    case model of
        Loaded { colorTexture, roughnessTexture, metallicTexture, sphereFrame } ->
            Scene3d.toHtml
                { camera = camera
                , width = Pixels.pixels 800
                , height = Pixels.pixels 600
                , environmentalLighting =
                    Scene3d.softLighting
                        { upDirection = Direction3d.positiveZ
                        , above = ( Luminance.nits 3000, Chromaticity.d65 )
                        , below = ( Quantity.zero, Chromaticity.d65 )
                        }
                , directLighting =
                    Scene3d.oneLightSource sunlight { castsShadows = False }
                , exposure = Exposure.fromEv100 12
                , whiteBalance = Scene3d.defaultWhiteBalance
                , backgroundColor = Scene3d.transparentBackground
                }
                [ Scene3d.sphere
                    (Sphere3d.withRadius (Length.centimeters 5) Point3d.origin)
                    (Material.texturedPbr
                        { baseColor = colorTexture
                        , roughness = roughnessTexture
                        , metallic = metallicTexture
                        }
                    )
                    { castsShadow = False }
                    |> Scene3d.placeIn sphereFrame
                ]

        Loading _ ->
            Html.text "Loading..."

        Errored message ->
            Html.text message


decodeMouseMove : Decoder Msg
decodeMouseMove =
    Decode.map2 MouseMove
        (Decode.field "movementX" Decode.float)
        (Decode.field "movementY" Decode.float)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Loading _ ->
            Sub.none

        Errored _ ->
            Sub.none

        Loaded { orbiting } ->
            if orbiting then
                Sub.batch
                    [ Browser.Events.onMouseMove decodeMouseMove
                    , Browser.Events.onMouseUp (Decode.succeed MouseUp)
                    ]

            else
                Browser.Events.onMouseDown (Decode.succeed MouseDown)


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
