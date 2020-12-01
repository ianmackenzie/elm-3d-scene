module DucklingGame exposing (main)

import Acceleration exposing (Acceleration)
import Angle exposing (Angle)
import Area
import Array exposing (Array)
import Axis3d exposing (Axis3d)
import Block3d exposing (Block3d)
import BoundingBox3d
import Browser
import Browser.Dom
import Browser.Events
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Density
import Direction3d exposing (Direction3d)
import Duration exposing (Duration)
import Force exposing (Force)
import Frame3d exposing (Frame3d)
import Html exposing (Html)
import Html.Events
import Http
import Json.Decode as Decode
import Keyboard exposing (Key)
import Keyboard.Arrows
import Length exposing (Length, Meters)
import LineSegment3d exposing (LineSegment3d)
import Mass exposing (Mass)
import Obj.Decode exposing (Decoder, ObjCoordinates)
import Physics.Body exposing (Body)
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Physics.Shape exposing (Shape)
import Physics.World exposing (World)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity, Unitless)
import Rectangle2d exposing (Rectangle2d)
import Rectangle3d exposing (Rectangle3d)
import Scene3d
import Scene3d.Material as Material
import Scene3d.Mesh as Mesh
import SketchPlane3d exposing (SketchPlane3d)
import Sphere3d exposing (Sphere3d)
import Task exposing (Task)
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)
import Viewpoint3d exposing (Viewpoint3d)
import WebGL.Texture


type alias TexturedVertex =
    { position : Point3d Meters BodyCoordinates
    , normal : Vector3d Unitless BodyCoordinates
    , uv : ( Float, Float )
    }


type alias Meshes =
    { convex : TriangularMesh (Point3d Meters BodyCoordinates)
    , textured : TriangularMesh TexturedVertex
    }


type alias LoadingModel =
    { meshes : Maybe Meshes
    , texture : Maybe (Material.Texture Color)
    , viewport : Maybe Browser.Dom.Viewport
    }


type BodyId
    = DucklingId
    | BallId
    | WallId


type alias RunningModel =
    { texturedMesh : Mesh.Textured BodyCoordinates
    , shadowMesh : Mesh.Shadow BodyCoordinates
    , texture : Material.Texture Color
    , world : World BodyId
    , screenDimensions : ( Quantity Int Pixels, Quantity Int Pixels )
    , pressedKeys : List Key
    , animationTime : Duration
    }


type Model
    = Loading LoadingModel
    | Running RunningModel
    | Failed String


type LoadingMsg
    = GotInitialViewport Browser.Dom.Viewport
    | MeshesResponse (Result Http.Error Meshes)
    | TextureResponse (Result WebGL.Texture.Error (Material.Texture Color))


type RunningMsg
    = Tick
    | Resize Int Int
    | KeyboardMsg Keyboard.Msg


type Msg
    = LoadingMsg LoadingMsg
    | RunningMsg RunningMsg


objFrame : Frame3d Meters BodyCoordinates { defines : ObjCoordinates }
objFrame =
    Frame3d.atPoint (Point3d.centimeters -1 0 -0.6)


decodeMeshes : Decoder Meshes
decodeMeshes =
    Obj.Decode.map2 Meshes
        (Obj.Decode.object "convex" (Obj.Decode.trianglesIn objFrame))
        (Obj.Decode.object "mesh" (Obj.Decode.texturedFacesIn objFrame))


init : () -> ( Model, Cmd Msg )
init () =
    ( Loading { meshes = Nothing, texture = Nothing, viewport = Nothing }
    , Cmd.map LoadingMsg <|
        Cmd.batch
            [ Task.attempt TextureResponse <|
                Material.loadWith Material.trilinearFiltering
                    "https://ianmackenzie.github.io/elm-3d-scene/examples/duckling.png"
            , Http.get
                { url = "https://ianmackenzie.github.io/elm-3d-scene/examples/duckling-with-hull.obj"
                , expect =
                    Obj.Decode.expectObj MeshesResponse (\value -> Length.centimeters (6 * value)) decodeMeshes
                }
            , Task.perform GotInitialViewport Browser.Dom.getViewport
            ]
    )


wall1 : Block3d Meters BodyCoordinates
wall1 =
    Block3d.from
        (Point3d.centimeters -31 -31 0)
        (Point3d.centimeters -30 31 1)


wall2 : Block3d Meters BodyCoordinates
wall2 =
    Block3d.rotateAround Axis3d.z (Angle.degrees 90) wall1


wall3 : Block3d Meters BodyCoordinates
wall3 =
    Block3d.rotateAround Axis3d.z (Angle.degrees 90) wall2


wall4 : Block3d Meters BodyCoordinates
wall4 =
    Block3d.rotateAround Axis3d.z (Angle.degrees 90) wall3


handleResponse : LoadingModel -> Model
handleResponse loadingModel =
    case ( loadingModel.meshes, loadingModel.texture, loadingModel.viewport ) of
        ( Just meshes, Just texture, Just viewport ) ->
            let
                ducklingShape =
                    Physics.Shape.unsafeConvex meshes.convex

                counterweight =
                    Physics.Shape.sphere <|
                        Sphere3d.atPoint (Point3d.centimeters 0 -0.25 -6)
                            (Length.centimeters 4)

                ducklingBehavior =
                    Physics.Body.dynamic (Mass.grams 40)

                ducklingBody =
                    Physics.Body.compound
                        [ ducklingShape
                        , counterweight
                        ]
                        DucklingId
                        |> Physics.Body.withBehavior ducklingBehavior
                        |> Physics.Body.withDamping
                            { linear = 0.99
                            , angular = 0.95
                            }

                wallBody1 =
                    Physics.Body.block wall1 WallId

                wallBody2 =
                    Physics.Body.block wall2 WallId

                wallBody3 =
                    Physics.Body.block wall3 WallId

                wallBody4 =
                    Physics.Body.block wall4 WallId

                texturedMesh =
                    Mesh.texturedFaces meshes.textured

                shadowMesh =
                    Mesh.shadow texturedMesh

                world =
                    Physics.World.empty
                        |> Physics.World.withGravity (Acceleration.gees 1) Direction3d.negativeZ
                        |> Physics.World.add ducklingBody
                        |> Physics.World.add wallBody1
                        |> Physics.World.add wallBody2
                        |> Physics.World.add wallBody3
                        |> Physics.World.add wallBody4

                boundingBox =
                    meshes.convex
                        |> TriangularMesh.vertices
                        |> Array.toList
                        |> BoundingBox3d.hullN
            in
            Running
                { texturedMesh = texturedMesh
                , shadowMesh = shadowMesh
                , texture = texture
                , world = world
                , screenDimensions =
                    ( Pixels.int (round viewport.viewport.width)
                    , Pixels.int (round viewport.viewport.height)
                    )
                , pressedKeys = []
                , animationTime = Duration.seconds 0
                }

        _ ->
            Loading loadingModel


updateLoading : LoadingMsg -> LoadingModel -> ( Model, Cmd Msg )
updateLoading msg model =
    case msg of
        GotInitialViewport viewport ->
            ( handleResponse { model | viewport = Just viewport }, Cmd.none )

        MeshesResponse (Ok meshes) ->
            ( handleResponse { model | meshes = Just meshes }, Cmd.none )

        TextureResponse (Ok texture) ->
            ( handleResponse { model | texture = Just texture }, Cmd.none )

        MeshesResponse (Err _) ->
            ( Failed "Error loading meshes", Cmd.none )

        TextureResponse (Err _) ->
            ( Failed "Error loading texture", Cmd.none )


simulate : Duration -> List Key -> Duration -> World BodyId -> World BodyId
simulate duration pressedKeys animationTime world =
    let
        controlDirection =
            Keyboard.Arrows.arrowsDirection pressedKeys
    in
    world
        |> Physics.World.update
            (\body ->
                let
                    bodyId =
                        Physics.Body.data body

                    bodyFrame =
                        Physics.Body.frame body

                    bodyOrigin =
                        Frame3d.originPoint bodyFrame
                in
                case bodyId of
                    DucklingId ->
                        let
                            centerDepth =
                                Quantity.max
                                    (Quantity.negate (Point3d.zCoordinate bodyOrigin))
                                    Quantity.zero

                            nominalCrossSection =
                                Area.squareCentimeters 40

                            submergedVolume =
                                nominalCrossSection
                                    |> Quantity.times centerDepth

                            displacedMass =
                                submergedVolume |> Quantity.at (Density.gramsPerCubicCentimeter 1)

                            buoyancy =
                                displacedMass |> Quantity.times (Acceleration.gees 1)

                            ( leftSign, rightSign ) =
                                case controlDirection of
                                    Keyboard.Arrows.North ->
                                        ( 1, 1 )

                                    Keyboard.Arrows.NorthEast ->
                                        ( 1, 0.5 )

                                    Keyboard.Arrows.NorthWest ->
                                        ( 0.5, 1 )

                                    Keyboard.Arrows.East ->
                                        ( 0.5, -0.5 )

                                    Keyboard.Arrows.West ->
                                        ( -0.5, 0.5 )

                                    Keyboard.Arrows.South ->
                                        ( -0.5, -0.5 )

                                    Keyboard.Arrows.SouthEast ->
                                        ( -0.5, -0.25 )

                                    Keyboard.Arrows.SouthWest ->
                                        ( -0.25, -0.5 )

                                    Keyboard.Arrows.NoDirection ->
                                        ( 0, 0 )

                            maxThrust =
                                Force.newtons 0.05

                            animationDuration =
                                Duration.seconds 0.5

                            animationParameter =
                                Quantity.ratio animationTime animationDuration

                            leftMagnitude =
                                maxThrust |> Quantity.multiplyBy (leftSign * 0.333 * (2 + sin (2 * pi * animationParameter)))

                            rightMagnitude =
                                maxThrust |> Quantity.multiplyBy (rightSign * 0.333 * (2 + cos (2 * pi * animationParameter)))

                            leftPoint =
                                Point3d.placeIn bodyFrame <|
                                    Point3d.centimeters -1 2.25 0

                            rightPoint =
                                Point3d.placeIn bodyFrame <|
                                    Point3d.centimeters -1 -1.75 0

                            thrustDirection =
                                Direction3d.xyZ (Angle.degrees 0) (Angle.degrees 45)
                                    |> Direction3d.placeIn bodyFrame
                        in
                        body
                            |> Physics.Body.applyForce buoyancy Direction3d.z bodyOrigin
                            |> Physics.Body.applyForce leftMagnitude thrustDirection leftPoint
                            |> Physics.Body.applyForce rightMagnitude thrustDirection rightPoint

                    _ ->
                        body
            )
        |> Physics.World.simulate duration


updateRunning : RunningMsg -> RunningModel -> ( Model, Cmd Msg )
updateRunning msg model =
    case msg of
        Tick ->
            let
                frameLength =
                    Duration.milliseconds 16
            in
            ( Running
                { model
                    | world =
                        simulate frameLength
                            model.pressedKeys
                            model.animationTime
                            model.world
                    , animationTime =
                        model.animationTime |> Quantity.plus frameLength
                }
            , Cmd.none
            )

        Resize width height ->
            ( Running { model | screenDimensions = ( Pixels.int width, Pixels.int height ) }
            , Cmd.none
            )

        KeyboardMsg keyboardMsg ->
            ( Running
                { model
                    | pressedKeys = Keyboard.update keyboardMsg model.pressedKeys
                    , animationTime = Duration.seconds 0
                }
            , Cmd.none
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LoadingMsg loadingMsg, Loading loadingModel ) ->
            updateLoading loadingMsg loadingModel

        ( LoadingMsg _, _ ) ->
            ( model, Cmd.none )

        ( RunningMsg runningMsg, Running runningModel ) ->
            updateRunning runningMsg runningModel

        ( RunningMsg _, _ ) ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Loading _ ->
            Sub.none

        Running _ ->
            Sub.map RunningMsg <|
                Sub.batch
                    [ Browser.Events.onResize Resize
                    , Browser.Events.onAnimationFrameDelta (always Tick)
                    , Sub.map KeyboardMsg Keyboard.subscriptions
                    ]

        Failed _ ->
            Sub.none


viewRunning : RunningModel -> Html Msg
viewRunning model =
    let
        ( width, height ) =
            model.screenDimensions

        testCamera =
            Camera3d.orthographic
                { viewpoint =
                    Viewpoint3d.orbitZ
                        { focalPoint = Point3d.origin
                        , distance = Length.meters 1
                        , azimuth = Angle.degrees 0
                        , elevation = Angle.degrees 0
                        }
                , viewportHeight = Length.centimeters 20
                }

        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.orbitZ
                        { focalPoint = Point3d.centimeters 10 10 0
                        , distance = Length.centimeters 75
                        , azimuth = Angle.degrees 30
                        , elevation = Angle.degrees 30
                        }
                , verticalFieldOfView = Angle.degrees 30
                }

        physicsEntities =
            model.world
                |> Physics.World.bodies
                |> List.map
                    (\body ->
                        let
                            bodyFrame =
                                Physics.Body.frame body

                            bodyId =
                                Physics.Body.data body

                            ducklingMaterial =
                                Material.texturedNonmetal
                                    { baseColor = model.texture
                                    , roughness = Material.constant 0.25
                                    }
                        in
                        case bodyId of
                            DucklingId ->
                                Scene3d.meshWithShadow ducklingMaterial
                                    model.texturedMesh
                                    model.shadowMesh
                                    |> Scene3d.placeIn bodyFrame

                            _ ->
                                Scene3d.nothing
                    )

        axes =
            Scene3d.group
                [ Scene3d.lineSegment (Material.color Color.red) (LineSegment3d.from Point3d.origin (Point3d.centimeters 20 0 0))
                , Scene3d.lineSegment (Material.color Color.green) (LineSegment3d.from Point3d.origin (Point3d.centimeters 0 20 0))
                , Scene3d.lineSegment (Material.color Color.blue) (LineSegment3d.from Point3d.origin (Point3d.centimeters 0 0 20))
                ]

        water =
            Scene3d.quad (Material.color Color.blue)
                (Point3d.centimeters -30 -30 0)
                (Point3d.centimeters 30 -30 0)
                (Point3d.centimeters 30 30 0)
                (Point3d.centimeters -30 30 0)

        walls =
            Scene3d.group
                (List.map (Scene3d.block (Material.matte Color.darkGreen)) [ wall1, wall2, wall3, wall4 ])
                |> Scene3d.placeIn Frame3d.atOrigin
    in
    Scene3d.cloudy
        { camera = camera
        , upDirection = Direction3d.z
        , dimensions = model.screenDimensions
        , background = Scene3d.transparentBackground
        , clipDepth = Length.meters 0.1
        , entities =
            [ Scene3d.group physicsEntities
            , walls
            , water
            ]
        }


view : Model -> Html Msg
view model =
    case model of
        Loading _ ->
            Html.text "Loading..."

        Running runningModel ->
            viewRunning runningModel

        Failed message ->
            Html.text message


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
