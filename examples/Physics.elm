module Physics exposing (main)

import Angle
import Array exposing (Array)
import Browser
import Camera3d exposing (Camera3d)
import Color
import Direction3d exposing (Direction3d)
import Html exposing (Html)
import Html.Events exposing (onClick)
import Length exposing (Length, Meters, inMeters, meters)
import Materials
import Physics.Body as Body exposing (Body)
import Physics.Events as Events
import Physics.Fps as Fps
import Physics.Scene as Scene exposing (BodyCoordinates, WorldCoordinates)
import Physics.Settings as Settings exposing (Settings, SettingsMsg, settings)
import Physics.World as World exposing (World)
import Pixels exposing (pixels)
import Point3d exposing (Point3d)
import Random
import Scene3d.Drawable as Drawable exposing (Drawable, Material)
import Scene3d.Mesh as Mesh exposing (Mesh, NoTangents, NoUV, ShadowsDisabled, ShadowsEnabled, Triangles, WithNormals)
import Scene3d.Shape as Shape
import Viewpoint3d exposing (Viewpoint3d)


type alias Model =
    { world : World (Drawable BodyCoordinates)
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
                        { eyePoint = Point3d.meters 0 20 20
                        , focalPoint = Point3d.meters 0 0 0
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


earthGravity : { x : Float, y : Float, z : Float }
earthGravity =
    { x = 0, y = 0, z = -9.81 }


moonGravity : { x : Float, y : Float, z : Float }
moonGravity =
    { x = 0, y = 0, z = -1.62 }


marsGravity : { x : Float, y : Float, z : Float }
marsGravity =
    { x = 0, y = 0, z = -3.7 }


initialWorld : World (Drawable BodyCoordinates)
initialWorld =
    World.empty
        |> World.setGravity moonGravity
        |> World.add floor
        |> addBoxes


materials : Array Material
materials =
    Array.fromList
        [ Materials.aluminum
        , Materials.whitePlastic
        , Materials.copper
        , { baseColor = Color.lightBlue, roughness = 0.25, metallic = False }
        , Materials.gold
        , Materials.whitePlastic
        , { baseColor = Color.blue, roughness = 0.25, metallic = False }
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


addBoxes : World (Drawable BodyCoordinates) -> World (Drawable BodyCoordinates)
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


floorDimensions : ( Length, Length, Length )
floorDimensions =
    ( Length.meters 10
    , Length.meters 10
    , Length.meters 0.1
    )


floorRadius : Length
floorRadius =
    Length.meters 30


floorMesh : Mesh BodyCoordinates (Triangles WithNormals NoUV NoTangents ShadowsDisabled)
floorMesh =
    Shape.sphere { radius = floorRadius, subdivisions = 144 }


{-| Floor has an empty mesh, because it is not rendered
-}
floor : Body (Drawable BodyCoordinates)
floor =
    let
        ( x, y, z ) =
            floorDimensions
    in
    Drawable.physical Materials.aluminum floorMesh
        |> Body.sphere (Length.inMeters floorRadius)
        |> Body.setMass 0
        |> Body.moveBy { x = 0, y = 0, z = -(Length.inMeters floorRadius) }


boxSize : Length
boxSize =
    Length.meters 0.9


boxMesh : Mesh BodyCoordinates (Triangles WithNormals NoUV NoTangents ShadowsEnabled)
boxMesh =
    Shape.block boxSize boxSize boxSize
        |> Mesh.enableShadows


box : Material -> Body (Drawable BodyCoordinates)
box material =
    let
        bodySize =
            Length.inMeters boxSize
    in
    Drawable.physical material boxMesh
        |> Drawable.withShadow boxMesh
        |> Body.box { x = bodySize, y = bodySize, z = bodySize }
        |> Body.setMass 5


sphereRadius : Length
sphereRadius =
    Length.meters 0.45


sphereMesh : Mesh BodyCoordinates (Triangles WithNormals NoUV NoTangents ShadowsEnabled)
sphereMesh =
    Shape.sphere { radius = sphereRadius, subdivisions = 36 }
        |> Mesh.enableShadows


sphere : Material -> Body (Drawable BodyCoordinates)
sphere material =
    Drawable.physical material sphereMesh
        |> Drawable.withShadow sphereMesh
        |> Body.sphere (inMeters sphereRadius)
        |> Body.setMass 2.5
