module OrbitingCamera exposing (main)

{-| This example shows how you can allow orbiting of a scene by listening for
mouse events and moving the camera accordingly.
-}

import Angle exposing (Angle)
import Browser
import Browser.Events
import Camera3d
import Color
import Json.Decode as Decode exposing (Decoder)
import Length
import Pixels exposing (Pixels)
import Point3d
import Quantity exposing (Quantity)
import Scene3d
import Scene3d.Material as Material
import Scene3d.Mesh as Mesh exposing (Mesh)
import Triangle3d
import Viewpoint3d


type WorldCoordinates
    = WorldCoordinates


type alias Model =
    { azimuth : Angle -- Orbiting angle of the camera around the focal point
    , elevation : Angle -- Angle of the camera up from the XY plane
    , orbiting : Bool -- Whether the mouse button is currently down
    , mesh1 : Mesh.Plain WorldCoordinates -- Saved Mesh values for rendering
    , mesh2 : Mesh.Plain WorldCoordinates
    }


type Msg
    = MouseDown
    | MouseUp
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)


init : () -> ( Model, Cmd Msg )
init () =
    -- Create a couple of Mesh values containing a single triangle each and
    -- store them in the model
    let
        mesh1 =
            Mesh.triangles
                [ Triangle3d.from
                    (Point3d.meters 0 0 0)
                    (Point3d.meters 1 0 0)
                    (Point3d.meters 1 1 0)
                ]

        mesh2 =
            Mesh.triangles
                [ Triangle3d.from
                    (Point3d.meters 0 0 0)
                    (Point3d.meters 1 1 0)
                    (Point3d.meters 0 1 0)
                ]
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
        -- Start orbiting when a mouse button is pressed
        MouseDown ->
            ( { model | orbiting = True }, Cmd.none )

        -- Stop orbiting when a mouse button is released
        MouseUp ->
            ( { model | orbiting = False }, Cmd.none )

        -- Orbit camera on mouse move (if a mouse button is down)
        MouseMove dx dy ->
            if model.orbiting then
                let
                    -- How fast we want to orbit the camera (orbiting the
                    -- camera by 1 degree per pixel of drag is a decent default
                    -- to start with)
                    rotationRate =
                        Angle.degrees 1 |> Quantity.per Pixels.pixel

                    -- Adjust azimuth based on horizontal mouse motion (one
                    -- degree per pixel)
                    newAzimuth =
                        model.azimuth
                            |> Quantity.minus (dx |> Quantity.at rotationRate)

                    -- Adjust elevation based on vertical mouse motion (one
                    -- degree per pixel), and clamp to make sure camera cannot
                    -- go past vertical in either direction
                    newElevation =
                        model.elevation
                            |> Quantity.plus (dy |> Quantity.at rotationRate)
                            |> Quantity.clamp (Angle.degrees -90) (Angle.degrees 90)
                in
                ( { model | azimuth = newAzimuth, elevation = newElevation }
                , Cmd.none
                )

            else
                ( model, Cmd.none )


{-| Use movementX and movementY for simplicity (don't need to store initial
mouse position in the model) - not supported in Internet Explorer though
-}
decodeMouseMove : Decoder Msg
decodeMouseMove =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.float Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.float Decode.float))


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.orbiting then
        -- If we're currently orbiting, listen for mouse moves and mouse button
        -- up events (to stop orbiting); in a real app we'd probably also want
        -- to listen for page visibility changes to stop orbiting if the user
        -- switches to a different tab or something
        Sub.batch
            [ Browser.Events.onMouseMove decodeMouseMove
            , Browser.Events.onMouseUp (Decode.succeed MouseUp)
            ]

    else
        -- If we're not currently orbiting, just listen for mouse down events
        -- to start orbiting
        Browser.Events.onMouseDown (Decode.succeed MouseDown)


view : Model -> Browser.Document Msg
view model =
    let
        -- Create a viewpoint by orbiting around a Z axis through the given
        -- focal point, with azimuth measured from the positive X direction
        -- towards positive Y
        viewpoint =
            Viewpoint3d.orbitZ
                { focalPoint = Point3d.meters 0.5 0.5 0
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
    { title = "OrbitingCamera"
    , body =
        [ Scene3d.unlit
            { camera = camera
            , clipDepth = Length.meters 0.1
            , dimensions = ( Pixels.int 400, Pixels.int 300 )
            , background = Scene3d.transparentBackground
            , entities =
                -- Draw the two single-triangle meshes
                [ Scene3d.mesh (Material.color Color.orange) model.mesh1
                , Scene3d.mesh (Material.color Color.blue) model.mesh2
                ]
            }
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
