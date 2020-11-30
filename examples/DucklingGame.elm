module DucklingGame exposing (main)

import Acceleration exposing (Acceleration)
import Angle exposing (Angle)
import Array exposing (Array)
import Axis3d exposing (Axis3d)
import BoundingBox3d
import Browser
import Browser.Dom
import Browser.Events
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Direction3d exposing (Direction3d)
import Duration exposing (Duration)
import Force exposing (Force)
import Frame3d exposing (Frame3d)
import Html exposing (Html)
import Http
import Length exposing (Length, Meters)
import Mass exposing (Mass)
import Obj.Decode exposing (Decoder, ObjCoordinates)
import Physics.Body exposing (Body)
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Physics.Shape exposing (Shape)
import Physics.World exposing (World)
import Pixels exposing (Pixels)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity, Unitless)
import Scene3d
import Scene3d.Material as Material
import Scene3d.Mesh as Mesh
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
    = Tick Duration
    | Resize Int Int


type Msg
    = LoadingMsg LoadingMsg
    | RunningMsg RunningMsg


objFrame : Frame3d Meters BodyCoordinates { defines : ObjCoordinates }
objFrame =
    Frame3d.atOrigin


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
                    Obj.Decode.expectObj MeshesResponse Length.meters decodeMeshes
                }
            , Task.perform GotInitialViewport Browser.Dom.getViewport
            ]
    )



-- bounds: Just (BoundingBox3d { maxX = Quantity 0.981508, maxY = Quantity 0.59949, maxZ = Quantity 1.635867, minX = Quantity -0.70286, minY = Quantity -0.541566, minZ = Quantity 0.097289 })


handleResponse : LoadingModel -> Model
handleResponse loadingModel =
    case ( loadingModel.meshes, loadingModel.texture, loadingModel.viewport ) of
        ( Just meshes, Just texture, Just viewport ) ->
            let
                ducklingShape =
                    Physics.Shape.unsafeConvex meshes.convex

                ducklingBehavior =
                    Physics.Body.dynamic (Mass.grams 100)

                ducklingBody =
                    Physics.Body.compound [ ducklingShape ] DucklingId
                        |> Physics.Body.withBehavior ducklingBehavior
                        |> Physics.Body.withDamping
                            { linear = 0.15
                            , angular = 0.15
                            }

                texturedMesh =
                    Mesh.texturedFaces meshes.textured

                shadowMesh =
                    Mesh.shadow texturedMesh

                world =
                    Physics.World.empty
                        |> Physics.World.withGravity (Acceleration.gees 1) Direction3d.negativeZ
                        |> Physics.World.add ducklingBody

                boundingBox =
                    meshes.convex
                        |> TriangularMesh.vertices
                        |> Array.toList
                        |> BoundingBox3d.hullN

                _ =
                    Debug.log "bounds" boundingBox
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


simulate : Duration -> World BodyId -> World BodyId
simulate duration world =
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
                            springConstant =
                                Force.newtons 1 |> Quantity.per (Length.centimeters 10)

                            forceMagnitude =
                                Point3d.zCoordinate bodyOrigin
                                    |> Quantity.negate
                                    |> Quantity.at springConstant
                        in
                        body |> Physics.Body.applyForce forceMagnitude Direction3d.z bodyOrigin

                    _ ->
                        body
            )
        |> Physics.World.simulate duration


updateRunning : RunningMsg -> RunningModel -> ( Model, Cmd Msg )
updateRunning msg model =
    case msg of
        Tick duration ->
            ( Running { model | world = simulate duration model.world }
            , Cmd.none
            )

        Resize width height ->
            ( Running { model | screenDimensions = ( Pixels.int width, Pixels.int height ) }
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
                    [ Browser.Events.onAnimationFrameDelta (Duration.milliseconds >> Tick)
                    , Browser.Events.onResize Resize
                    ]

        Failed _ ->
            Sub.none


viewRunning : RunningModel -> Html Msg
viewRunning model =
    let
        ( width, height ) =
            model.screenDimensions

        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.orbitZ
                        { focalPoint = Point3d.origin
                        , distance = Length.meters 7
                        , azimuth = Angle.degrees 30
                        , elevation = Angle.degrees 32
                        }
                , verticalFieldOfView = Angle.degrees 30
                }
    in
    Scene3d.sunny
        { upDirection = Direction3d.z
        , sunlightDirection = Direction3d.negativeZ
        , shadows = True
        , camera = camera
        , dimensions = model.screenDimensions
        , background = Scene3d.transparentBackground
        , clipDepth = Length.meters 0.1
        , entities =
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
