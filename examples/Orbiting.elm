module Orbiting exposing (main)

import Angle exposing (Angle)
import Browser
import Browser.Events
import Camera3d
import Color
import Html exposing (Html)
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Length
import Palette.Tango as Tango
import Pixels
import Point3d
import Quantity
import Scene3d
import Scene3d.Chromaticity as Chromaticity
import Scene3d.Exposure as Exposure
import Scene3d.Material as Material
import Scene3d.Mesh as Mesh exposing (Mesh)
import SketchPlane3d
import Triangle3d
import Viewpoint3d


type World
    = World


type alias Model =
    { azimuth : Angle
    , elevation : Angle
    , orbiting : Bool
    , mesh1 : Mesh.Plain World
    , mesh2 : Mesh.Plain World
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
            Mesh.triangles [ triangle1 ]

        mesh2 =
            Mesh.triangles [ triangle2 ]
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
                , clipDepth = Length.meters 0.1
                }
    in
    { title = "Triangles"
    , body =
        [ Scene3d.toHtml []
            { camera = camera
            , dimensions = ( Pixels.pixels 800, Pixels.pixels 600 )
            , directLighting = Scene3d.noDirectLighting
            , environmentalLighting = Scene3d.noEnvironmentalLighting
            , background = Scene3d.transparentBackground
            , exposure = Scene3d.defaultExposure
            , whiteBalance = Scene3d.defaultWhiteBalance
            }
            [ Scene3d.mesh (Material.color Tango.orange2) model.mesh1
            , Scene3d.mesh (Material.color Tango.skyBlue2) model.mesh2
            ]
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
