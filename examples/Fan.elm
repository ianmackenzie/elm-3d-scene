module Fan exposing (main)

import Angle exposing (Angle)
import Axis3d
import Browser
import Browser.Events
import Camera3d
import Color
import Direction3d
import Html exposing (Html)
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Length
import Parameter1d
import Pixels
import Point3d
import Quantity
import Scene3d
import Scene3d.Chromaticity as Chromaticity
import Scene3d.Drawable as Drawable
import Scene3d.Exposure as Exposure
import Scene3d.Mesh as Mesh exposing (Mesh, NoNormals, NoTangents, NoUV, ShadowsDisabled, Triangles)
import SketchPlane3d
import Triangle3d
import Viewpoint3d


type World
    = World


type alias Model =
    { azimuth : Angle
    , elevation : Angle
    , orbiting : Bool
    , mesh1 : Mesh World (Triangles NoNormals NoUV NoTangents ShadowsDisabled)
    , mesh2 : Mesh World (Triangles NoNormals NoUV NoTangents ShadowsDisabled)
    }


type Msg
    = MouseDown
    | MouseUp
    | MouseMove Float Float


init : () -> ( Model, Cmd Msg )
init () =
    let
        triangle1 =
            Triangle3d.from
                (Point3d.meters 0 0 0)
                (Point3d.meters 1 0 0)
                (Point3d.meters 1 1 0)

        triangle2 =
            Triangle3d.from
                (Point3d.meters 0 0 0)
                (Point3d.meters 1 1 0)
                (Point3d.meters 0 1 0)

        mesh1 =
            Mesh.triangles [] [ triangle1 ]

        mesh2 =
            Mesh.triangles [] [ triangle2 ]
    in
    ( { azimuth = Angle.degrees 45
      , elevation = Angle.degrees 30
      , orbiting = False
      , mesh1 = mesh1
      , mesh2 = mesh2
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
                { focalPoint = Point3d.meters 0.5 2 0
                , groundPlane = SketchPlane3d.xy
                , azimuth = model.azimuth
                , elevation = model.elevation
                , distance = Length.meters 12
                }

        camera =
            Camera3d.perspective
                { viewpoint = viewpoint
                , verticalFieldOfView = Angle.degrees 30
                , clipDepth = Length.meters 0.1
                }

        square =
            Drawable.group
                [ Drawable.colored Color.orange model.mesh1
                , Drawable.colored Color.blue model.mesh2
                ]

        rotationAxis =
            Axis3d.through (Point3d.meters 0 2 0) Direction3d.x

        angles =
            Parameter1d.leading 16 <|
                Quantity.interpolateFrom
                    (Angle.degrees 0)
                    (Angle.degrees 360)

        rotatedSquare angle =
            square |> Drawable.rotateAround rotationAxis angle
    in
    { title = "Layers"
    , body =
        [ Scene3d.unlit []
            { camera = camera
            , width = Pixels.pixels 800
            , height = Pixels.pixels 600
            }
            (List.map rotatedSquare angles)
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
