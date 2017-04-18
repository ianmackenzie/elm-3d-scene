module Logo exposing (node)

import OpenSolid.SceneGraph as SceneGraph
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point3d as Point3d
import OpenSolid.Frame3d as Frame3d
import Color


node : SceneGraph.Node
node =
    let
        height =
            0.9

        xOffset =
            0.6

        yOffset =
            0.6

        zOffset =
            0.6

        p0 =
            Point3d.origin

        p1 =
            Point3d ( 1, 0, 0 )

        p2 =
            Point3d ( 1, 1, 0 )

        p3 =
            Point3d ( 0, 1, 0 )

        p4 =
            Point3d ( 0, 1, height )

        p5 =
            Point3d ( 0, 0, height )

        p6 =
            Point3d ( 1, 0, height )

        p7 =
            Point3d ( 1, 1 - yOffset, height )

        p8 =
            Point3d ( 1, 1, height - zOffset )

        p9 =
            Point3d ( 1 - xOffset, 1, height )

        orange =
            Color.rgb 240 173 0

        green =
            Color.rgb 127 209 59

        lightBlue =
            Color.rgb 96 181 204

        darkBlue =
            Color.rgb 90 99 120

        leftFace =
            SceneGraph.triangleFan [ p1, p2, p8, p7, p6 ]
                |> SceneGraph.colored orange

        rightFace =
            SceneGraph.triangleFan [ p2, p3, p4, p9, p8 ]
                |> SceneGraph.colored lightBlue

        topFace =
            SceneGraph.triangleFan [ p6, p7, p9, p4, p5 ]
                |> SceneGraph.colored green

        triangleFace =
            SceneGraph.triangleFan [ p7, p8, p9 ]
                |> SceneGraph.colored darkBlue

        bottomFace =
            SceneGraph.triangleFan [ p0, p3, p2, p1 ]
                |> SceneGraph.colored green

        backLeftFace =
            SceneGraph.triangleFan [ p6, p5, p0, p1 ]
                |> SceneGraph.colored lightBlue

        backRightFace =
            SceneGraph.triangleFan [ p3, p0, p5, p4 ]
                |> SceneGraph.colored orange

        centerFrame =
            Frame3d.at (Point3d ( 0.5, 0.5, height / 2 ))
    in
        SceneGraph.group
            [ leftFace
            , rightFace
            , topFace
            , triangleFace
            , backLeftFace
            , backRightFace
            , bottomFace
            ]
            |> SceneGraph.relativeTo centerFrame
