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
        { colorTexture : Maybe (Material.Texture Color)
        , roughnessTexture : Maybe (Material.Texture Float)
        , normalMapTexture : Maybe (Material.Texture Material.NormalMap)
        }
    | Loaded
        { colorTexture : Material.Texture Color
        , roughnessTexture : Material.Texture Float
        , normalMapTexture : Material.Texture Material.NormalMap
        , sphereFrame : Frame3d Meters WorldCoordinates { defines : SphereCoordinates }
        , orbiting : Bool
        }
    | Errored String


type Msg
    = GotColorTexture (Result WebGL.Texture.Error (Material.Texture Color))
    | GotRoughnessTexture (Result WebGL.Texture.Error (Material.Texture Float))
    | GotNormalMapTexture (Result WebGL.Texture.Error (Material.Texture Material.NormalMap))
    | MouseDown
    | MouseUp
    | MouseMove Float Float


init : ( Model, Cmd Msg )
init =
    ( Loading
        { colorTexture = Nothing
        , roughnessTexture = Nothing
        , normalMapTexture = Nothing
        }
    , Cmd.batch
        [ Material.load "https://ianmackenzie.github.io/elm-3d-scene/examples/leather/Leather11_col.jpg"
            |> Task.attempt GotColorTexture
        , Material.load "https://ianmackenzie.github.io/elm-3d-scene/examples/leather/Leather11_rgh.jpg"
            |> Task.attempt GotRoughnessTexture
        , Material.load "https://ianmackenzie.github.io/elm-3d-scene/examples/leather/Leather11_nrm.jpg"
            |> Task.attempt GotNormalMapTexture
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    let
        updatedModel =
            case model of
                Loading textures ->
                    case message of
                        GotColorTexture (Ok colorTexture) ->
                            checkIfLoaded { textures | colorTexture = Just colorTexture }

                        GotRoughnessTexture (Ok roughnessTexture) ->
                            checkIfLoaded { textures | roughnessTexture = Just roughnessTexture }

                        GotNormalMapTexture (Ok normalMapTexture) ->
                            checkIfLoaded { textures | normalMapTexture = Just normalMapTexture }

                        GotColorTexture (Err error) ->
                            Errored "Error loading color texture"

                        GotRoughnessTexture (Err error) ->
                            Errored "Error loading roughness texture"

                        GotNormalMapTexture (Err error) ->
                            Errored "Error loading normal map texture"

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

                        GotRoughnessTexture _ ->
                            model

                        GotNormalMapTexture _ ->
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
    { colorTexture : Maybe (Material.Texture Color)
    , roughnessTexture : Maybe (Material.Texture Float)
    , normalMapTexture : Maybe (Material.Texture Material.NormalMap)
    }
    -> Model
checkIfLoaded textures =
    case ( textures.colorTexture, textures.roughnessTexture, textures.normalMapTexture ) of
        ( Just colorTexture, Just roughnessTexture, Just normalMapTexture ) ->
            Loaded
                { colorTexture = colorTexture
                , roughnessTexture = roughnessTexture
                , normalMapTexture = normalMapTexture
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
    Scene3d.directionalLight Scene3d.doesNotCastShadows
        { chromaticity = Chromaticity.sunlight
        , intensity = Illuminance.lux 20000
        , direction = Direction3d.yz (Angle.degrees -120)
        }


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
        Loaded { colorTexture, roughnessTexture, normalMapTexture, sphereFrame } ->
            let
                material =
                    Material.normalMappedPbr
                        { baseColor = colorTexture
                        , roughness = roughnessTexture
                        , metallic = Material.constant 0
                        , normalMap = normalMapTexture
                        }
            in
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
                    Scene3d.oneLightSource sunlight
                , exposure = Exposure.fromEv100 12
                , whiteBalance = Scene3d.defaultWhiteBalance
                , background = Scene3d.transparentBackground
                }
                [ Sphere3d.withRadius (Length.centimeters 5) Point3d.origin
                    |> Scene3d.sphere Scene3d.doesNotCastShadows material
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
