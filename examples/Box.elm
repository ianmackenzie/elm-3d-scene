module Box exposing (..)

import OpenSolid.SceneGraph as SceneGraph
import OpenSolid.SceneGraph.Material as Material
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Direction3d as Direction3d
import OpenSolid.WebGL.Frame3d as Frame3d
import WebGL
import OpenSolid.Axis3d as Axis3d
import OpenSolid.Point3d as Point3d
import Html exposing (Html)
import Html.Attributes as Attributes
import Color exposing (Color)
import AnimationFrame
import Time


box : SceneGraph.Node
box =
    let
        p0 =
            Point3d ( -5, -5, -5 )

        p1 =
            Point3d ( 5, -5, -5 )

        p2 =
            Point3d ( 5, 5, -5 )

        p3 =
            Point3d ( -5, 5, -5 )

        p4 =
            Point3d ( -5, -5, 5 )

        p5 =
            Point3d ( 5, -5, 5 )

        p6 =
            Point3d ( 5, 5, 5 )

        p7 =
            Point3d ( -5, 5, 5 )

        -- Build a square face like this:
        --
        --   d      c
        --   o------o
        --   |     /|
        --   |   /  |
        --   | /    |
        --   o------o
        --   a      b
        --
        square : Color -> Point3d -> Point3d -> Point3d -> Point3d -> SceneGraph.Node
        square color a b c d =
            SceneGraph.triangles
                [ Triangle3d ( a, b, c ), Triangle3d ( a, c, d ) ]
                |> SceneGraph.meshWith { material = Material.solid color }

        bottomFace =
            square Color.blue p0 p3 p2 p1

        topFace =
            square Color.blue p4 p5 p6 p7

        backFace =
            square Color.red p3 p0 p4 p7

        frontFace =
            square Color.red p1 p2 p6 p5

        leftFace =
            square Color.green p2 p3 p7 p6

        rightFace =
            square Color.green p0 p1 p5 p4
    in
        SceneGraph.group
            [ frontFace
            , backFace
            , leftFace
            , rightFace
            , topFace
            , bottomFace
            ]


view : Float -> Html msg
view elapsedTime =
    let
        width =
            800

        height =
            600

        eyeFrame =
            Frame3d.lookAt
                { focalPoint = Point3d.origin
                , eyePoint = Point3d ( 40, 0, 20 )
                , upDirection = Direction3d.z
                }

        projection =
            SceneGraph.perspectiveProjection
                { verticalFov = degrees 30
                , aspectRatio = width / height
                , zNear = 0.1
                , zFar = 100
                }

        angle =
            degrees 90 * Time.inSeconds elapsedTime

        scene =
            box |> SceneGraph.rotateAround Axis3d.z angle

        entities =
            SceneGraph.toEntities eyeFrame projection scene

        attributes =
            [ Attributes.width width, Attributes.height height ]
    in
        WebGL.toHtml attributes entities


main : Program Never Float Float
main =
    Html.program
        { init = ( 0.0, Cmd.none )
        , view = view
        , update = \delta elapsed -> ( elapsed + delta, Cmd.none )
        , subscriptions = always (AnimationFrame.diffs identity)
        }
