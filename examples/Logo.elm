module Logo
    exposing
        ( edges
        , node
        , vertices
        )

import Color
import OpenSolid.Frame3d as Frame3d
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.LineSegment3d as LineSegment3d
import OpenSolid.Point3d as Point3d
import OpenSolid.Scene as Scene


height : Float
height =
    0.9


xOffset : Float
xOffset =
    0.6


yOffset : Float
yOffset =
    0.6


zOffset : Float
zOffset =
    0.6


p0 : Point3d
p0 =
    Point3d.origin


p1 : Point3d
p1 =
    Point3d ( 1, 0, 0 )


p2 : Point3d
p2 =
    Point3d ( 1, 1, 0 )


p3 : Point3d
p3 =
    Point3d ( 0, 1, 0 )


p4 : Point3d
p4 =
    Point3d ( 0, 1, height )


p5 : Point3d
p5 =
    Point3d ( 0, 0, height )


p6 : Point3d
p6 =
    Point3d ( 1, 0, height )


p7 : Point3d
p7 =
    Point3d ( 1, 1 - yOffset, height )


p8 : Point3d
p8 =
    Point3d ( 1, 1, height - zOffset )


p9 : Point3d
p9 =
    Point3d ( 1 - xOffset, 1, height )


centerFrame : Frame3d
centerFrame =
    Frame3d.at (Point3d ( 0.5, 0.5, height / 2 ))


node : Scene.Node
node =
    let
        orange =
            Color.rgb 240 173 0

        green =
            Color.rgb 127 209 59

        lightBlue =
            Color.rgb 96 181 204

        darkBlue =
            Color.rgb 90 99 120

        leftFace =
            Scene.triangleFan [ p1, p2, p8, p7, p6 ]
                |> Scene.colored orange

        rightFace =
            Scene.triangleFan [ p2, p3, p4, p9, p8 ]
                |> Scene.colored lightBlue

        topFace =
            Scene.triangleFan [ p6, p7, p9, p4, p5 ]
                |> Scene.colored green

        triangleFace =
            Scene.triangleFan [ p7, p8, p9 ]
                |> Scene.colored darkBlue

        bottomFace =
            Scene.triangleFan [ p0, p3, p2, p1 ]
                |> Scene.colored green

        backLeftFace =
            Scene.triangleFan [ p6, p5, p0, p1 ]
                |> Scene.colored lightBlue

        backRightFace =
            Scene.triangleFan [ p3, p0, p5, p4 ]
                |> Scene.colored orange
    in
    Scene.group
        [ leftFace
        , rightFace
        , topFace
        , triangleFace
        , backLeftFace
        , backRightFace
        , bottomFace
        ]
        |> Scene.relativeTo centerFrame


vertices : List Point3d
vertices =
    [ p0, p1, p2, p3, p4, p5, p6, p7, p8, p9 ]
        |> List.map (Point3d.relativeTo centerFrame)


edges : List LineSegment3d
edges =
    [ LineSegment3d ( p0, p1 )
    , LineSegment3d ( p1, p2 )
    , LineSegment3d ( p2, p3 )
    , LineSegment3d ( p3, p0 )
    , LineSegment3d ( p0, p5 )
    , LineSegment3d ( p1, p6 )
    , LineSegment3d ( p2, p8 )
    , LineSegment3d ( p3, p4 )
    , LineSegment3d ( p5, p6 )
    , LineSegment3d ( p6, p7 )
    , LineSegment3d ( p7, p8 )
    , LineSegment3d ( p8, p9 )
    , LineSegment3d ( p7, p9 )
    , LineSegment3d ( p9, p4 )
    , LineSegment3d ( p4, p5 )
    ]
        |> List.map (LineSegment3d.relativeTo centerFrame)
