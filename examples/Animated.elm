module Animated exposing (..)

import AnimationFrame
import Html exposing (Html)
import Materials
import Math.Vector3 as Vector3 exposing (Vec3, vec3)
import OpenSolid.Axis3d as Axis3d
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Frame3d as Frame3d
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point3d as Point3d
import OpenSolid.Scene as Scene
import OpenSolid.Scene.Geometry as Geometry exposing (Geometry)
import OpenSolid.Scene.Light as Light exposing (Light)
import OpenSolid.Scene.Material as Material exposing (Material)
import OpenSolid.Scene.Node as Node exposing (Node)
import OpenSolid.Scene.SimpleGeometry as SimpleGeometry
import OpenSolid.WebGL.Camera as Camera
import OpenSolid.WebGL.Frame3d as Frame3d
import Shapes
import Task
import Time exposing (Time)
import WebGL.Texture


shape : Geometry -> Material -> ( Float, Float ) -> Node
shape geometry material ( x, y ) =
    Geometry.shaded material geometry
        |> Node.translateBy (Vector3d ( x, y, 0 ))


sphere : Material -> ( Float, Float ) -> Node
sphere =
    let
        geometry =
            Shapes.sphere Point3d.origin 1
    in
    shape geometry


box : Material -> ( Float, Float ) -> Node
box =
    let
        geometry =
            Shapes.box 1.5 1.5 1.5
    in
    shape geometry


cylinder : Material -> ( Float, Float ) -> Node
cylinder =
    let
        geometry =
            Shapes.cylinder (Point3d ( 0, 0, -0.75 )) (Point3d ( 0, 0, 0.75 )) 1
    in
    shape geometry


shapes : Node
shapes =
    Node.group
        [ sphere Materials.gold ( 3, -3 )
        , cylinder Materials.whitePlastic ( 3, 0 )
        , sphere Materials.copper ( 3, 3 )
        , box Materials.chromium ( 0, -3 )
        , sphere Materials.aluminum ( 0, 0 )
        , cylinder Materials.gold ( 0, 3 )
        , sphere Materials.copper ( -3, -3 )
        , box Materials.blackPlastic ( -3, 0 )
        , sphere Materials.whitePlastic ( -3, 3 )
        ]


camera =
    Camera.perspective
        { frame =
            Frame3d.lookAt
                { eyePoint = Point3d ( 10, 10, 10 )
                , focalPoint = Point3d.origin
                , upDirection = Direction3d.positiveZ
                }
        , screenWidth = 1024
        , screenHeight = 768
        , verticalFov = degrees 30
        , zNear = 0.1
        , zFar = 100
        }


type alias PointLight =
    { startPoint : Point3d
    , rotationAxis : Axis3d
    , color : Vec3
    , rotationSpeed : Float
    }


pointLight1 : PointLight
pointLight1 =
    { startPoint = Point3d ( 1.5, 1.5, 3 )
    , rotationAxis = Axis3d.z
    , color = vec3 0 2 10
    , rotationSpeed = degrees 125
    }


pointLight2 : PointLight
pointLight2 =
    { startPoint = Point3d ( 1.5, -1.5, 0 )
    , rotationAxis = Axis3d.x |> Axis3d.rotateAround Axis3d.z (degrees 45)
    , color = vec3 3 0 0
    , rotationSpeed = degrees 67
    }


pointLightRadius : Float
pointLightRadius =
    0.05


pointLightColorScale : Float
pointLightColorScale =
    1.0 / (pointLightRadius * pointLightRadius)


pointLightGeometry : Geometry
pointLightGeometry =
    Shapes.sphere Point3d.origin pointLightRadius


view : Model -> Html Msg
view model =
    case model.loadedTexture of
        Nothing ->
            Html.text "Loading texture..."

        Just (Err _) ->
            Html.text "Error loading texture"

        Just (Ok lookupTexture) ->
            let
                seconds =
                    Time.inSeconds model.time

                lightDirection1 =
                    Direction3d.negativeX
                        |> Direction3d.rotateAround Axis3d.y (degrees -15)
                        |> Direction3d.rotateAround Axis3d.z (seconds * degrees 111)

                lightDirection2 =
                    Direction3d.negativeY
                        |> Direction3d.rotateAround Axis3d.x (degrees 45)
                        |> Direction3d.rotateAround Axis3d.z (seconds * degrees 47)

                lightPoint1 =
                    pointLight1.startPoint
                        |> Point3d.rotateAround pointLight1.rotationAxis
                            (seconds * pointLight1.rotationSpeed)

                lightPoint2 =
                    pointLight2.startPoint
                        |> Point3d.rotateAround pointLight2.rotationAxis
                            (seconds * pointLight2.rotationSpeed)

                lightNode point color =
                    let
                        scaledColor =
                            Vector3.scale pointLightColorScale color
                    in
                    pointLightGeometry
                        |> Geometry.shaded (Material.emissive scaledColor)
                        |> Node.placeIn (Frame3d.at point)

                lightNode1 =
                    lightNode lightPoint1 pointLight1.color

                lightNode2 =
                    lightNode lightPoint2 pointLight2.color

                lights =
                    [ Light.directional lightDirection1 (vec3 0 0.1 0.02)
                    , Light.directional lightDirection2 (vec3 0.3 0.3 0.3)
                    , Light.point lightPoint1 pointLight1.color
                    , Light.point lightPoint2 pointLight2.color
                    , Light.ambient lookupTexture (vec3 0.01 0.01 0.01)
                    , Light.point (Point3d ( 8, 8, 5 )) (vec3 5 5 5)
                    , Light.point (Point3d ( 8, -8, 5 )) (vec3 5 5 5)
                    , Light.point (Point3d ( -8, 8, 5 )) (vec3 5 5 5)
                    , Light.point (Point3d ( -8, -8, 5 )) (vec3 5 5 5)
                    ]

                scene =
                    Node.group [ shapes, lightNode1, lightNode2 ]

                renderOptions =
                    [ Scene.devicePixelRatio 2
                    , Scene.gammaCorrection 0.45
                    ]
            in
            Scene.renderWith renderOptions lights camera scene


type alias Model =
    { loadedTexture : Maybe (Result WebGL.Texture.Error Light.AmbientLookupTexture)
    , time : Time
    }


type Msg
    = LoadComplete (Result WebGL.Texture.Error Light.AmbientLookupTexture)
    | Tick Float


init : ( Model, Cmd Msg )
init =
    ( { loadedTexture = Nothing, time = 0 }
    , Task.attempt LoadComplete (Light.loadAmbientLookupTextureFrom "lookup.png")
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        LoadComplete loadedTexture ->
            ( { model | loadedTexture = Just loadedTexture }, Cmd.none )

        Tick time ->
            ( { model | time = time }, Cmd.none )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , subscriptions = always (AnimationFrame.times Tick)
        , update = update
        , view = view
        }
