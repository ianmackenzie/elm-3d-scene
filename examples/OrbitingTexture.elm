module OrbitingTexture exposing (main)

import Angle exposing (Angle)
import Array
import Browser
import Browser.Events
import Camera3d
import Color exposing (Color)
import Html exposing (Html)
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Length
import Palette.Tango as Tango
import Pixels
import Point3d
import Quantity
import Scene3d
import Scene3d.Material as Material
import Scene3d.Mesh as Mesh exposing (Mesh)
import SketchPlane3d
import Task
import Triangle3d
import TriangularMesh
import Viewpoint3d
import WebGL.Texture as Texture exposing (Texture)


type World
    = World


type alias Model =
    { azimuth : Angle
    , elevation : Angle
    , orbiting : Bool
    , texture : Maybe (Material.Texture Color)
    }


type Msg
    = MouseDown
    | MouseUp
    | MouseMove Float Float
    | GotTexture (Result Texture.Error (Material.Texture Color))


init : () -> ( Model, Cmd Msg )
init () =
    ( { azimuth = Angle.degrees 45
      , elevation = Angle.degrees 30
      , orbiting = False
      , texture = Nothing
      }
    , Task.attempt GotTexture
        (Material.loadWith Material.bilinearFiltering
            "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f3/Elm_logo.svg/512px-Elm_logo.svg.png"
        )
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        MouseDown ->
            ( { model | orbiting = True }, Cmd.none )

        MouseUp ->
            ( { model | orbiting = False }, Cmd.none )

        MouseMove dx dy ->
            if model.orbiting then
                let
                    newAzimuth =
                        model.azimuth |> Quantity.minus (Angle.degrees dx)

                    newElevation =
                        model.elevation
                            |> Quantity.plus (Angle.degrees dy)
                            |> Quantity.clamp
                                (Angle.degrees -90)
                                (Angle.degrees 90)
                in
                ( { model | azimuth = newAzimuth, elevation = newElevation }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        GotTexture (Ok texture) ->
            ( { model | texture = Just texture }, Cmd.none )

        GotTexture (Err error) ->
            let
                _ =
                    Debug.log "Error loading texture" error
            in
            ( model, Cmd.none )


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
        Browser.Events.onMouseDown (Decode.succeed MouseDown)


view : Model -> Browser.Document Msg
view model =
    let
        viewpoint =
            Viewpoint3d.orbit
                { focalPoint = Point3d.meters 0.5 0.5 0
                , groundPlane = SketchPlane3d.xy
                , azimuth = model.azimuth
                , elevation = model.elevation
                , distance = Length.meters 3
                }

        camera =
            Camera3d.perspective
                { viewpoint = viewpoint
                , verticalFieldOfView = Angle.degrees 30
                }
    in
    { title = "OrbitingTexture"
    , body =
        case model.texture of
            Just texture ->
                [ Scene3d.unlit
                    { camera = camera
                    , dimensions = ( Pixels.pixels 800, Pixels.pixels 600 )
                    , background = Scene3d.backgroundColor Tango.plum1
                    , clipDepth = Length.meters 0.1
                    , entities =
                        [ Scene3d.quad (Material.texturedColor texture)
                            (Point3d.meters 0 0 0)
                            (Point3d.meters 1 0 0)
                            (Point3d.meters 1 1 0)
                            (Point3d.meters 0 1 0)
                        ]
                    }
                ]

            Nothing ->
                [ Html.text "Loading..." ]
    }


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
