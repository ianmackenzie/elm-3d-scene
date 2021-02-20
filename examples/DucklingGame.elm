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
import Physics.Material
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
    , ducklingTexture : Maybe (Material.Texture Color)
    , ballColorTexture : Maybe (Material.Texture Color)
    , ballMetallicTexture : Maybe (Material.Texture Float)
    , ballRoughnessTexture : Maybe (Material.Texture Float)
    , viewport : Maybe Browser.Dom.Viewport
    }


type BodyId
    = DucklingId
    | BallId
    | WallId


type alias RunningModel =
    { texturedMesh : Mesh.Textured BodyCoordinates
    , shadowMesh : Mesh.Shadow BodyCoordinates
    , ducklingTexture : Material.Texture Color
    , ballColorTexture : Material.Texture Color
    , ballMetallicTexture : Material.Texture Float
    , ballRoughnessTexture : Material.Texture Float
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
    | DucklingTextureResponse (Result WebGL.Texture.Error (Material.Texture Color))
    | BallColorTextureResponse (Result WebGL.Texture.Error (Material.Texture Color))
    | BallMetallicTextureResponse (Result WebGL.Texture.Error (Material.Texture Float))
    | BallRoughnessTextureResponse (Result WebGL.Texture.Error (Material.Texture Float))


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
    ( Loading
        { meshes = Nothing
        , ducklingTexture = Nothing
        , ballColorTexture = Nothing
        , ballMetallicTexture = Nothing
        , ballRoughnessTexture = Nothing
        , viewport = Nothing
        }
    , Cmd.map LoadingMsg <|
        Cmd.batch
            [ Task.attempt DucklingTextureResponse <|
                Material.loadWith Material.trilinearFiltering
                    "https://ianmackenzie.github.io/elm-3d-scene/examples/duckling.png"
            , Task.attempt BallColorTextureResponse <|
                Material.loadWith Material.trilinearFiltering
                    "https://ianmackenzie.github.io/elm-3d-scene/examples/ornament/ChristmasTreeOrnament003_2K_Color.jpg"
            , Task.attempt BallRoughnessTextureResponse <|
                Material.loadWith Material.trilinearFiltering
                    "https://ianmackenzie.github.io/elm-3d-scene/examples/ornament/ChristmasTreeOrnament003_2K_Roughness.jpg"
            , Task.attempt BallMetallicTextureResponse <|
                Material.loadWith Material.trilinearFiltering
                    "https://ianmackenzie.github.io/elm-3d-scene/examples/ornament/ChristmasTreeOrnament003_2K_Metalness.jpg"
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


ballRadius : Length
ballRadius =
    Length.centimeters 6


type SixMaybes a b c d e f
    = SixMaybes (Maybe a) (Maybe b) (Maybe c) (Maybe d) (Maybe e) (Maybe f)


handleResponse : LoadingModel -> Model
handleResponse loadingModel =
    case SixMaybes loadingModel.meshes loadingModel.ducklingTexture loadingModel.viewport loadingModel.ballColorTexture loadingModel.ballRoughnessTexture loadingModel.ballMetallicTexture of
        SixMaybes (Just meshes) (Just ducklingTexture) (Just viewport) (Just ballColorTexture) (Just ballRoughnessTexture) (Just ballMetallicTexture) ->
            let
                ducklingShape =
                    Physics.Shape.unsafeConvex meshes.convex

                counterweight =
                    Physics.Shape.sphere <|
                        Sphere3d.atPoint (Point3d.centimeters 0 -0.25 -6)
                            (Length.centimeters 4)

                ducklingBehavior =
                    Physics.Body.dynamic (Mass.grams 40)

                ducklingMaterial =
                    Physics.Material.custom
                        { friction = 0.0005
                        , bounciness = 0.75
                        }

                ballBehavior =
                    Physics.Body.dynamic (Mass.grams 10)

                ballMaterial =
                    Physics.Material.custom
                        { friction = 0.05
                        , bounciness = 0.75
                        }

                wallMaterial =
                    Physics.Material.custom
                        { friction = 0.0005
                        , bounciness = 0
                        }

                ducklingBody =
                    Physics.Body.compound
                        [ ducklingShape
                        , counterweight
                        ]
                        DucklingId
                        |> Physics.Body.withBehavior ducklingBehavior
                        |> Physics.Body.withMaterial ducklingMaterial
                        |> Physics.Body.withDamping
                            { linear = 0.99
                            , angular = 0.95
                            }

                ballBody =
                    Physics.Body.sphere (Sphere3d.atOrigin ballRadius) BallId
                        |> Physics.Body.withBehavior ballBehavior
                        |> Physics.Body.withMaterial ballMaterial
                        |> Physics.Body.withDamping
                            { linear = 0.99
                            , angular = 0.8
                            }
                        |> Physics.Body.translateBy (Vector3d.centimeters 15 -5 5)

                wallBody1 =
                    Physics.Body.block wall1 WallId
                        |> Physics.Body.withMaterial wallMaterial

                wallBody2 =
                    Physics.Body.block wall2 WallId
                        |> Physics.Body.withMaterial wallMaterial

                wallBody3 =
                    Physics.Body.block wall3 WallId
                        |> Physics.Body.withMaterial wallMaterial

                wallBody4 =
                    Physics.Body.block wall4 WallId
                        |> Physics.Body.withMaterial wallMaterial

                texturedMesh =
                    Mesh.texturedFaces meshes.textured

                shadowMesh =
                    Mesh.shadow texturedMesh

                world =
                    Physics.World.empty
                        |> Physics.World.withGravity (Acceleration.gees 1) Direction3d.negativeZ
                        |> Physics.World.add ducklingBody
                        |> Physics.World.add ballBody
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
                , ducklingTexture = ducklingTexture
                , ballColorTexture = ballColorTexture
                , ballRoughnessTexture = ballRoughnessTexture
                , ballMetallicTexture = ballMetallicTexture
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

        DucklingTextureResponse (Ok texture) ->
            ( handleResponse { model | ducklingTexture = Just texture }, Cmd.none )

        BallColorTextureResponse (Ok texture) ->
            ( handleResponse { model | ballColorTexture = Just texture }, Cmd.none )

        BallRoughnessTextureResponse (Ok texture) ->
            ( handleResponse { model | ballRoughnessTexture = Just texture }, Cmd.none )

        BallMetallicTextureResponse (Ok texture) ->
            ( handleResponse { model | ballMetallicTexture = Just texture }, Cmd.none )

        MeshesResponse (Err _) ->
            ( Failed "Error loading meshes", Cmd.none )

        DucklingTextureResponse (Err _) ->
            ( Failed "Error loading duckling texture", Cmd.none )

        BallColorTextureResponse (Err _) ->
            ( Failed "Error loading ball color texture", Cmd.none )

        BallRoughnessTextureResponse (Err _) ->
            ( Failed "Error loading ball roughness texture", Cmd.none )

        BallMetallicTextureResponse (Err _) ->
            ( Failed "Error loading ball metallic texture", Cmd.none )


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
                                    Point3d.centimeters -1.5 2.25 0

                            rightPoint =
                                Point3d.placeIn bodyFrame <|
                                    Point3d.centimeters -1.5 -1.75 0

                            thrustDirection =
                                Direction3d.xyZ (Angle.degrees 0) (Angle.degrees 45)
                                    |> Direction3d.placeIn bodyFrame
                        in
                        body
                            |> Physics.Body.applyForce buoyancy Direction3d.z bodyOrigin
                            |> Physics.Body.applyForce leftMagnitude thrustDirection leftPoint
                            |> Physics.Body.applyForce rightMagnitude thrustDirection rightPoint

                    BallId ->
                        let
                            z0 =
                                Point3d.zCoordinate bodyOrigin
                        in
                        if z0 |> Quantity.greaterThan ballRadius then
                            body

                        else
                            let
                                h =
                                    ballRadius |> Quantity.minus z0

                                submergedVolume =
                                    Quantity.multiplyBy (pi / 3) (Quantity.squared h)
                                        |> Quantity.times
                                            (Quantity.multiplyBy 3 ballRadius |> Quantity.minus h)

                                displacedMass =
                                    submergedVolume |> Quantity.at (Density.gramsPerCubicCentimeter 1)

                                buoyancy =
                                    displacedMass |> Quantity.times (Acceleration.gees 1)
                            in
                            body
                                |> Physics.Body.applyForce buoyancy Direction3d.z bodyOrigin

                    WallId ->
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
                                    { baseColor = model.ducklingTexture
                                    , roughness = Material.constant 0.25
                                    }

                            ballMaterial =
                                Material.texturedPbr
                                    { baseColor = model.ballColorTexture
                                    , roughness = model.ballRoughnessTexture
                                    , metallic = model.ballMetallicTexture
                                    }
                        in
                        case bodyId of
                            DucklingId ->
                                Scene3d.meshWithShadow ducklingMaterial
                                    model.texturedMesh
                                    model.shadowMesh
                                    |> Scene3d.placeIn bodyFrame

                            WallId ->
                                Scene3d.nothing

                            BallId ->
                                Scene3d.sphere ballMaterial
                                    (Sphere3d.atOrigin ballRadius)
                                    |> Scene3d.placeIn bodyFrame
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
