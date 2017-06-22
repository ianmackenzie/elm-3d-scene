module Animated exposing (..)

import AnimationFrame
import Html exposing (Html)
import Materials
import Math.Vector3 as Vector3 exposing (Vec3, vec3)
import OpenSolid.Axis3d as Axis3d
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point3d as Point3d
import OpenSolid.Scene as Scene
import OpenSolid.Scene.Geometry as Geometry exposing (Geometry)
import OpenSolid.Scene.Light as Light
import OpenSolid.Scene.Material as Material exposing (Material)
import OpenSolid.Scene.Node as Node exposing (Node)
import OpenSolid.Scene.SimpleGeometry as SimpleGeometry
import OpenSolid.WebGL.Camera as Camera
import OpenSolid.WebGL.Frame3d as Frame3d
import Shapes
import Time


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
            Shapes.box 2 2 2
    in
    shape geometry


cylinder : Material -> ( Float, Float ) -> Node
cylinder =
    let
        geometry =
            Shapes.cylinder (Point3d ( 0, 0, -1 )) (Point3d ( 0, 0, 1 )) 1
    in
    shape geometry


shapes : Node
shapes =
    Node.group
        [ sphere Materials.gold ( 4, -4 )
        , cylinder Materials.whitePlastic ( 4, 0 )
        , box Materials.copper ( 4, 4 )
        , box Materials.chromium ( 0, -4 )
        , sphere Materials.aluminum ( 0, 0 )
        , cylinder Materials.gold ( 0, 4 )
        , cylinder Materials.copper ( -4, -4 )
        , box Materials.blackPlastic ( -4, 0 )
        , sphere Materials.whitePlastic ( -4, 4 )
        ]


camera =
    Camera.perspective
        { frame =
            Frame3d.lookAt
                { eyePoint = Point3d ( 15, 15, 15 )
                , focalPoint = Point3d.origin
                , upDirection = Direction3d.positiveZ
                }
        , screenWidth = 1024
        , screenHeight = 768
        , verticalFov = degrees 30
        , zNear = 0.1
        , zFar = 100
        }


gammaCorrected : Vec3 -> Vec3
gammaCorrected color =
    let
        ( r, g, b ) =
            Vector3.toTuple color
    in
    vec3 (r ^ 0.45) (g ^ 0.45) (b ^ 0.45)


type alias PointLight =
    { startPoint : Point3d
    , rotationAxis : Axis3d
    , color : Vec3
    , rotationSpeed : Float
    }


pointLight1 : PointLight
pointLight1 =
    { startPoint = Point3d ( 2, 2, 0 )
    , rotationAxis = Axis3d.x |> Axis3d.rotateAround Axis3d.z (degrees -45)
    , color = vec3 0 0 2
    , rotationSpeed = degrees 188
    }


pointLight2 : PointLight
pointLight2 =
    { startPoint = Point3d ( 2, -2, 0 )
    , rotationAxis = Axis3d.x |> Axis3d.rotateAround Axis3d.z (degrees 45)
    , color = vec3 2 0 0
    , rotationSpeed = degrees 67
    }


pointLight1StartNode : Node
pointLight1StartNode =
    SimpleGeometry.points [ pointLight1.startPoint ]
        |> SimpleGeometry.colored (gammaCorrected pointLight1.color)


pointLight2StartNode : Node
pointLight2StartNode =
    SimpleGeometry.points [ pointLight2.startPoint ]
        |> SimpleGeometry.colored (gammaCorrected pointLight2.color)


view : Float -> Html Float
view time =
    let
        seconds =
            Time.inSeconds time

        lightDirection1 =
            Direction3d.negativeX
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

        lightNode1 =
            pointLight1StartNode
                |> Node.rotateAround pointLight1.rotationAxis
                    (seconds * pointLight1.rotationSpeed)

        lightNode2 =
            pointLight2StartNode
                |> Node.rotateAround pointLight2.rotationAxis
                    (seconds * pointLight2.rotationSpeed)

        lights =
            [ Light.directional lightDirection1 (vec3 0 0.2 0.2)
            , Light.directional lightDirection2 (vec3 0.2 0.2 0.2)
            , Light.point lightPoint1 pointLight1.color
            , Light.point lightPoint2 pointLight2.color
            ]

        scene =
            Node.group [ shapes, lightNode1, lightNode2 ]
    in
    Scene.renderWith { devicePixelRatio = 2 } lights camera scene


update : Float -> Float -> ( Float, Cmd Float )
update time _ =
    ( time, Cmd.none )


main : Program Never Float Float
main =
    Html.program
        { init = ( 0.0, Cmd.none )
        , subscriptions = always (AnimationFrame.times identity)
        , update = update
        , view = view
        }
