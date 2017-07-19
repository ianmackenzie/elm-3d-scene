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
import OpenSolid.WebGL.Camera as Camera exposing (Camera)
import OpenSolid.WebGL.Frame3d as Frame3d
import PointLight exposing (PointLight(..))
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


view : Model -> Html Msg
view =
    let
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

        pointLight1StartPoint =
            Point3d ( 1.5, 1.5, 3 )

        pointLight1RotationAxis =
            Axis3d.z

        pointLight1Color =
            vec3 0 2 10

        pointLight2StartPoint =
            Point3d ( 1.5, -1.5, 0 )

        pointLight2RotationAxis =
            Axis3d.x |> Axis3d.rotateAround Axis3d.z (degrees 45)

        pointLight2Color =
            vec3 3 0 0

        pointLightRadius =
            0.05

        directionalLight1StartDirection =
            Direction3d.negativeX
                |> Direction3d.rotateAround Axis3d.y (degrees -15)

        directionalLight1Color =
            vec3 0 0.1 0.02

        directionalLight2StartDirection =
            Direction3d.negativeY
                |> Direction3d.rotateAround Axis3d.x (degrees 45)

        directionalLight2Color =
            vec3 0.3 0.3 0.3

        ambientLightColor =
            vec3 0.01 0.01 0.01

        overheadLight1Point =
            Point3d ( 8, 8, 5 )

        overheadLight2Point =
            Point3d ( 8, -8, 5 )

        overheadLight3Point =
            Point3d ( -8, 8, 5 )

        overheadLight4Point =
            Point3d ( -8, -8, 5 )

        overheadLightColor =
            vec3 5 5 5
    in
    \model ->
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
                        directionalLight1StartDirection
                            |> Direction3d.rotateAround Axis3d.z
                                (seconds * degrees 111)

                    lightDirection2 =
                        directionalLight2StartDirection
                            |> Direction3d.rotateAround Axis3d.z
                                (seconds * degrees 47)

                    lightPoint1 =
                        pointLight1StartPoint
                            |> Point3d.rotateAround pointLight1RotationAxis
                                (seconds * degrees 67)

                    lightPoint2 =
                        pointLight2StartPoint
                            |> Point3d.rotateAround pointLight2RotationAxis
                                (seconds * degrees 67)

                    pointLight1 =
                        PointLight
                            { position = lightPoint1
                            , radius = pointLightRadius
                            , color = pointLight1Color
                            }

                    pointLight2 =
                        PointLight
                            { position = lightPoint2
                            , radius = pointLightRadius
                            , color = pointLight2Color
                            }

                    overheadLight1 =
                        PointLight
                            { position = overheadLight1Point
                            , radius = pointLightRadius
                            , color = overheadLightColor
                            }

                    overheadLight2 =
                        PointLight
                            { position = overheadLight2Point
                            , radius = pointLightRadius
                            , color = overheadLightColor
                            }

                    overheadLight3 =
                        PointLight
                            { position = overheadLight3Point
                            , radius = pointLightRadius
                            , color = overheadLightColor
                            }

                    overheadLight4 =
                        PointLight
                            { position = overheadLight4Point
                            , radius = pointLightRadius
                            , color = overheadLightColor
                            }

                    lights =
                        [ Light.ambient lookupTexture ambientLightColor
                        , Light.directional lightDirection1
                            directionalLight1Color
                        , Light.directional lightDirection2
                            directionalLight2Color
                        , PointLight.light pointLight1
                        , PointLight.light pointLight2
                        , PointLight.light overheadLight1
                        , PointLight.light overheadLight2
                        , PointLight.light overheadLight3
                        , PointLight.light overheadLight4
                        ]

                    scene =
                        Node.group
                            [ shapes
                            , PointLight.node pointLight1
                            , PointLight.node pointLight2
                            ]

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
