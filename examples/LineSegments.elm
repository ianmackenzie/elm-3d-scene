module LineSegments exposing (main)

import Angle exposing (Angle)
import Arc2d
import Arc3d
import Browser
import Browser.Events
import Camera3d
import Color
import Html exposing (Html)
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Length
import Parameter1d
import Pixels
import Point2d
import Point3d
import Polyline3d
import Quantity
import Scene3d
import Scene3d.Chromaticity as Chromaticity
import Scene3d.Drawable as Drawable
import Scene3d.Exposure as Exposure
import Scene3d.Mesh as Mesh exposing (LineSegments, Mesh)
import SketchPlane3d
import Triangle3d
import Viewpoint3d


type World
    = World


type alias Model =
    { azimuth : Angle
    , elevation : Angle
    , orbiting : Bool
    , mesh : Mesh World LineSegments
    }


type Msg
    = MouseDown
    | MouseUp
    | MouseMove Float Float


init : () -> ( Model, Cmd Msg )
init () =
    let
        polyline =
            Polyline3d.fromVertices
                [ Point3d.meters 1 0 0
                , Point3d.meters 0 0 0
                , Point3d.meters 0 0 1
                , Point3d.meters 0 1 1
                ]

        mesh =
            Mesh.polyline [] polyline
    in
    ( { azimuth = Angle.degrees 45
      , elevation = Angle.degrees 30
      , orbiting = False
      , mesh = mesh
      }
    , Cmd.none
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
                { focalPoint = Point3d.origin
                , groundPlane = SketchPlane3d.xy
                , azimuth = model.azimuth
                , elevation = model.elevation
                , distance = Length.meters 5
                }

        camera =
            Camera3d.perspective
                { viewpoint = viewpoint
                , verticalFieldOfView = Angle.degrees 30
                , clipDepth = Length.meters 0.1
                }
    in
    { title = "Points"
    , body =
        [ Scene3d.unlit []
            { camera = camera
            , width = Pixels.pixels 800
            , height = Pixels.pixels 600
            }
            [ Drawable.colored Color.blue model.mesh ]
        ]
    }


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
