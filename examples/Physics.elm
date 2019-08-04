module Physics exposing (main)

import Angle
import Array exposing (Array)
import Browser
import Camera3d exposing (Camera3d)
import Color
import Common.Events as Events
import Common.Fps as Fps
import Common.Scene as Scene exposing (ModelCoordinates, WorldCoordinates)
import Common.Settings as Settings exposing (Settings, SettingsMsg, settings)
import Direction3d exposing (Direction3d)
import Html exposing (Html)
import Html.Events exposing (onClick)
import Length exposing (Meters, inMeters, meters)
import Materials
import Physics.Body as Body exposing (Body)
import Physics.World as World exposing (World)
import Pixels exposing (pixels)
import Point3d exposing (Point3d)
import Random
import Scene3d.Drawable as Drawable exposing (Drawable)
import Scene3d.Material as Material exposing (Material)
import Shapes
import Viewpoint3d exposing (Viewpoint3d)


type alias Model =
    { world : World (Drawable Meters ModelCoordinates)
    , fps : List Float
    , settings : Settings
    , screenWidth : Float
    , screenHeight : Float
    }


type Msg
    = ForSettings SettingsMsg
    | Tick Float
    | Resize Float Float
    | Restart


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { world = initialWorld
      , fps = []
      , settings = { settings | showFpsMeter = True }
      , screenWidth = 0
      , screenHeight = 0
      }
    , Events.measureSize Resize
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ForSettings settingsMsg ->
            ( { model
                | settings = Settings.update settingsMsg model.settings
              }
            , Cmd.none
            )

        Tick dt ->
            ( { model
                | fps = Fps.update dt model.fps
                , world = World.simulate (1000 / 60) model.world
              }
            , Cmd.none
            )

        Resize width height ->
            ( { model | screenWidth = width, screenHeight = height }
            , Cmd.none
            )

        Restart ->
            ( { model | world = initialWorld }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onResize Resize
        , Events.onAnimationFrameDelta Tick
        ]


view : Model -> Html Msg
view { settings, fps, world, screenWidth, screenHeight } =
    let
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { eyePoint = Point3d.meters 0 30 20
                        , focalPoint = Point3d.origin
                        , upDirection = Direction3d.positiveZ
                        }
                , clipDepth = meters 0.1
                , verticalFieldOfView = Angle.degrees 24
                }
    in
    Html.div []
        [ Scene.view
            { world = world
            , camera = camera
            , floorOffset = Just floorOffset
            , screenWidth = pixels screenWidth
            , screenHeight = pixels screenHeight
            }
        , Settings.view ForSettings
            settings
            [ Html.button [ onClick Restart ]
                [ Html.text "Restart the demo" ]
            ]
        , if settings.showFpsMeter then
            Fps.view fps (List.length (World.getBodies world))

          else
            Html.text ""
        ]


initialWorld : World (Drawable Meters ModelCoordinates)
initialWorld =
    World.empty
        |> World.setGravity { x = 0, y = 0, z = -10 }
        |> World.add floor
        |> addBoxes


materials : Array Material
materials =
    Array.fromList
        [ Materials.aluminum
        , Materials.whitePlastic
        , Materials.copper
        , Material.nonmetal { color = Color.lightBlue, roughness = 0.25 }
        , Materials.gold
        , Materials.whitePlastic
        , Material.nonmetal { color = Color.blue, roughness = 0.25 }
        ]


type alias Offsets =
    { x : Float
    , y : Float
    , z : Float
    }


offsetGenerator : Random.Generator Offsets
offsetGenerator =
    let
        magnitude =
            0.01
    in
    Random.map3 Offsets
        (Random.float -magnitude magnitude)
        (Random.float -magnitude magnitude)
        (Random.float -magnitude magnitude)


randomOffsets : Int -> Offsets
randomOffsets index =
    Random.step offsetGenerator (Random.initialSeed index)
        |> Tuple.first


addBoxes : World (Drawable Meters ModelCoordinates) -> World (Drawable Meters ModelCoordinates)
addBoxes world =
    let
        xySize =
            4

        zSize =
            5

        xyDimensions =
            List.map toFloat (List.range 0 (xySize - 1))

        zDimensions =
            List.map toFloat (List.range 0 (zSize - 1))

        distance =
            1
    in
    List.foldl
        (\x world1 ->
            List.foldl
                (\y world2 ->
                    List.foldl
                        (\z ->
                            let
                                index =
                                    round (z * xySize * xySize + y * xySize + x)

                                material =
                                    Array.get (index |> modBy (Array.length materials)) materials
                                        |> Maybe.withDefault Materials.aluminum

                                body =
                                    if (index |> modBy 3) == 0 then
                                        box material

                                    else
                                        sphere material

                                offsets =
                                    randomOffsets index
                            in
                            body
                                |> Body.moveBy
                                    { x = (x - (xySize - 1) / 2) * distance + offsets.x
                                    , y = (y - (xySize - 1) / 2) * distance + offsets.y
                                    , z = (z + (2 * zSize + 1) / 2) * distance + offsets.z
                                    }
                                |> World.add
                        )
                        world2
                        zDimensions
                )
                world1
                xyDimensions
        )
        world
        xyDimensions


{-| Shift the floor a little bit down
-}
floorOffset : { x : Float, y : Float, z : Float }
floorOffset =
    { x = 0, y = 0, z = -1 }


{-| Floor has an empty mesh, because it is not rendered
-}
floor : Body (Drawable Meters ModelCoordinates)
floor =
    Body.plane Drawable.empty
        |> Body.setMass 0
        |> Body.setPosition floorOffset


box : Material -> Body (Drawable Meters ModelCoordinates)
box material =
    let
        size =
            meters 0.9
    in
    Shapes.box material size size size
        |> Body.box { x = inMeters size, y = inMeters size, z = inMeters size }
        |> Body.setMass 5


sphere : Material -> Body (Drawable Meters ModelCoordinates)
sphere material =
    let
        radius =
            meters 0.45
    in
    Shapes.sphere material Point3d.origin radius
        |> Body.sphere (inMeters radius)
        |> Body.setMass 2.5
