module Sphere exposing (main)

import Angle exposing (Angle)
import Browser
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Direction3d
import Html exposing (Html)
import Illuminance
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
import Viewpoint3d exposing (Viewpoint3d)
import WebGL.Texture


type World
    = World


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
        }
    | Errored String


type Msg
    = GotColorTexture (Result WebGL.Texture.Error (Material.Channel Color))
    | GotMetallicTexture (Result WebGL.Texture.Error (Material.Channel Float))
    | GotRoughnessTexture (Result WebGL.Texture.Error (Material.Channel Float))


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
    case model of
        Loading textures ->
            case message of
                GotColorTexture (Ok colorChannel) ->
                    ( checkIfLoaded { textures | colorTexture = Just colorChannel }
                    , Cmd.none
                    )

                GotMetallicTexture (Ok metallicChannel) ->
                    ( checkIfLoaded { textures | metallicTexture = Just metallicChannel }
                    , Cmd.none
                    )

                GotRoughnessTexture (Ok roughnessChannel) ->
                    ( checkIfLoaded { textures | roughnessTexture = Just roughnessChannel }
                    , Cmd.none
                    )

                GotColorTexture (Err error) ->
                    ( Errored (Debug.toString error), Cmd.none )

                GotMetallicTexture (Err error) ->
                    ( Errored (Debug.toString error), Cmd.none )

                GotRoughnessTexture (Err error) ->
                    ( Errored (Debug.toString error), Cmd.none )

        Loaded _ ->
            ( model, Cmd.none )

        Errored _ ->
            ( model, Cmd.none )


checkIfLoaded :
    { colorTexture : Maybe (Material.Channel Color)
    , metallicTexture : Maybe (Material.Channel Float)
    , roughnessTexture : Maybe (Material.Channel Float)
    }
    -> Model
checkIfLoaded textures =
    case ( textures.colorTexture, textures.metallicTexture, textures.roughness ) of
        ( Just colorTexture, Just metallicTexture, Just roughnessTexture ) ->
            Loaded { colorTexture = colorTexture, metallicTexture = metallicTexture, roughnessTexture = roughnessTexture }

        _ ->
            Loading textures


viewpoint : Viewpoint3d Meters World
viewpoint =
    Viewpoint3d.lookAt
        { focalPoint = Point3d.origin
        , eyePoint = Point3d.centimeters 20 10 20
        , upDirection = Direction3d.positiveZ
        }


sunlight =
    Scene3d.directionalLight
        Chromaticity.d65
        (Illuminance.lux 10000)
        (Direction3d.yz (Angle.degrees -120))


camera : Camera3d Meters World
camera =
    Camera3d.perspective
        { viewpoint = viewpoint
        , verticalFieldOfView = Angle.degrees 30
        , clipDepth = Length.centimeters 0.5
        }


material : Material.ForMeshWithNormalsAndUvs
material =
    Material.nonmetal
        { baseColor = Tango.skyBlue2
        , roughness = 0.4
        }


view : Model -> Html msg
view model =
    case model of
        Loaded { colorTexture, roughnessTexture, metallicTexture } ->
            Scene3d.toHtml
                { camera = camera
                , width = Pixels.pixels 800
                , height = Pixels.pixels 600
                , environmentalLighting =
                    Scene3d.softLighting
                        { upDirection = Direction3d.positiveZ
                        , above = ( Luminance.nits 5000, Chromaticity.d65 )
                        , below = ( Quantity.zero, Chromaticity.d65 )
                        }
                , directLighting =
                    Scene3d.oneLightSource sunlight { castsShadows = False }
                , exposure =
                    Exposure.fromMaxLuminance (Luminance.nits 5000)
                , whiteBalance = Scene3d.defaultWhiteBalance
                , backgroundColor = Scene3d.transparentBackground
                }
                [ Scene3d.sphere
                    (Sphere3d.withRadius (Length.centimeters 5) Point3d.origin)
                    material
                    { castsShadow = False }
                ]

        Loading _ ->
            Html.text "Loading..."

        Errored message ->
            Html.text message


main : Program Never Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
