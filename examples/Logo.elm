module Logo exposing
    ( CenterCoordinates
    , edges
    , entity
    , vertices
    )

import Array exposing (Array)
import Color
import Frame3d exposing (Frame3d)
import Length exposing (Length, Meters)
import LineSegment3d exposing (LineSegment3d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity, Unitless)
import Scene3d
import Scene3d.Material as Material
import Scene3d.Mesh as Mesh
import TriangularMesh


type CenterCoordinates
    = CenterCoordinates


type CornerCoordinates
    = CornerCoordinates


height : Length
height =
    Length.meters 0.9


xOffset : Length
xOffset =
    Length.meters 0.6


yOffset : Length
yOffset =
    Length.meters 0.6


zOffset : Length
zOffset =
    Length.meters 0.6


p0 : Point3d Meters CornerCoordinates
p0 =
    Point3d.origin


p1 : Point3d Meters CornerCoordinates
p1 =
    Point3d.meters 1 0 0


p2 : Point3d Meters CornerCoordinates
p2 =
    Point3d.meters 1 1 0


p3 : Point3d Meters CornerCoordinates
p3 =
    Point3d.meters 0 1 0


p4 : Point3d Meters CornerCoordinates
p4 =
    Point3d.xyz Quantity.zero Length.meter height


p5 : Point3d Meters CornerCoordinates
p5 =
    Point3d.xyz Quantity.zero Quantity.zero height


p6 : Point3d Meters CornerCoordinates
p6 =
    Point3d.xyz Length.meter Quantity.zero height


p7 : Point3d Meters CornerCoordinates
p7 =
    Point3d.xyz
        Length.meter
        (Length.meter |> Quantity.minus yOffset)
        height


p8 : Point3d Meters CornerCoordinates
p8 =
    Point3d.xyz
        Length.meter
        Length.meter
        (height |> Quantity.minus zOffset)


p9 : Point3d Meters CornerCoordinates
p9 =
    Point3d.xyz
        (Length.meter |> Quantity.minus xOffset)
        Length.meter
        height


centerFrame : Frame3d Meters CornerCoordinates { defines : CenterCoordinates }
centerFrame =
    Frame3d.atPoint <|
        Point3d.xyz
            (Length.meters 0.5)
            (Length.meters 0.5)
            (Quantity.half height)


triangleFan : List (Point3d Meters coordinates) -> Mesh.Plain coordinates
triangleFan points =
    let
        faceIndices =
            List.range 1 (List.length points - 2)
                |> List.map
                    (\index ->
                        ( 0, index, index + 1 )
                    )
    in
    Mesh.indexedTriangles <|
        TriangularMesh.indexed (Array.fromList points) faceIndices


entity : Scene3d.Entity CenterCoordinates
entity =
    let
        orange =
            Color.rgb255 240 173 0

        green =
            Color.rgb255 127 209 59

        lightBlue =
            Color.rgb255 96 181 204

        darkBlue =
            Color.rgb255 90 99 86

        leftFace =
            Scene3d.mesh (Material.color orange) <|
                triangleFan [ p1, p2, p8, p7, p6 ]

        rightFace =
            Scene3d.mesh (Material.color lightBlue) <|
                triangleFan [ p2, p3, p4, p9, p8 ]

        topFace =
            Scene3d.mesh (Material.color green) <|
                triangleFan [ p6, p7, p9, p4, p5 ]

        triangleFace =
            Scene3d.mesh (Material.color darkBlue) <|
                triangleFan [ p7, p8, p9 ]

        bottomFace =
            Scene3d.mesh (Material.color green) <|
                triangleFan [ p0, p3, p2, p1 ]

        backLeftFace =
            Scene3d.mesh (Material.color lightBlue) <|
                triangleFan [ p6, p5, p0, p1 ]

        backRightFace =
            Scene3d.mesh (Material.color orange) <|
                triangleFan [ p3, p0, p5, p4 ]
    in
    Scene3d.group
        [ leftFace
        , rightFace
        , topFace
        , triangleFace
        , backLeftFace
        , backRightFace
        , bottomFace
        ]
        |> Scene3d.relativeTo centerFrame


vertices : List (Point3d Meters CenterCoordinates)
vertices =
    [ p0, p1, p2, p3, p4, p5, p6, p7, p8, p9 ]
        |> List.map (Point3d.relativeTo centerFrame)


edges : List (LineSegment3d Meters CenterCoordinates)
edges =
    [ LineSegment3d.from p0 p1
    , LineSegment3d.from p1 p2
    , LineSegment3d.from p2 p3
    , LineSegment3d.from p3 p0
    , LineSegment3d.from p0 p5
    , LineSegment3d.from p1 p6
    , LineSegment3d.from p2 p8
    , LineSegment3d.from p3 p4
    , LineSegment3d.from p5 p6
    , LineSegment3d.from p6 p7
    , LineSegment3d.from p7 p8
    , LineSegment3d.from p8 p9
    , LineSegment3d.from p7 p9
    , LineSegment3d.from p9 p4
    , LineSegment3d.from p4 p5
    ]
        |> List.map (LineSegment3d.relativeTo centerFrame)
