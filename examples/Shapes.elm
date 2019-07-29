module Shapes exposing
    ( box
    , cylinder
    , sphere
    )

import Angle
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Parameter1d
import Point3d exposing (Point3d)
import Quantity exposing (Quantity, zero)
import Scene3d.Drawable as Drawable exposing (Drawable)
import Scene3d.Material as Material exposing (Material)
import SketchPlane3d
import Triangle3d exposing (Triangle3d)
import Vector3d exposing (Vector3d)


sphere : Material -> Point3d units coordinates -> Quantity Float units -> Drawable units coordinates
sphere material centerPoint radius =
    let
        x0 =
            Point3d.xCoordinate centerPoint

        y0 =
            Point3d.yCoordinate centerPoint

        z0 =
            Point3d.zCoordinate centerPoint

        subdivisions =
            72

        angleValues =
            Parameter1d.steps subdivisions
                (Quantity.interpolateFrom zero (Angle.turns 1))

        pointsAndNormals =
            angleValues
                |> List.map
                    (\theta ->
                        angleValues
                            |> List.map
                                (\phi ->
                                    ( Point3d.xyz
                                        (x0 |> Quantity.plus (radius |> Quantity.multiplyBy (Angle.sin phi * Angle.cos theta)))
                                        (y0 |> Quantity.plus (radius |> Quantity.multiplyBy (Angle.sin phi * Angle.sin theta)))
                                        (z0 |> Quantity.plus (radius |> Quantity.multiplyBy (Angle.cos phi)))
                                    , Direction3d.fromAzimuthInAndElevationFrom
                                        SketchPlane3d.xy
                                        theta
                                        (Angle.degrees 90 |> Quantity.minus phi)
                                        |> Direction3d.toVector
                                    )
                                )
                    )
                |> List.concat

        startIndices =
            List.range 0 (subdivisions - 1)

        linearIndex thetaIndex phiIndex =
            (thetaIndex |> modBy subdivisions) * subdivisions + (phiIndex |> modBy subdivisions)

        faces =
            startIndices
                |> List.map
                    (\leftThetaIndex ->
                        startIndices
                            |> List.map
                                (\topPhiIndex ->
                                    let
                                        rightThetaIndex =
                                            leftThetaIndex + 1

                                        bottomPhiIndex =
                                            topPhiIndex + 1

                                        bottomLeftIndex =
                                            linearIndex leftThetaIndex bottomPhiIndex

                                        bottomRightIndex =
                                            linearIndex rightThetaIndex bottomPhiIndex

                                        topLeftIndex =
                                            linearIndex leftThetaIndex topPhiIndex

                                        topRightIndex =
                                            linearIndex rightThetaIndex topPhiIndex
                                    in
                                    [ ( bottomLeftIndex
                                      , bottomRightIndex
                                      , topRightIndex
                                      )
                                    , ( bottomLeftIndex
                                      , topRightIndex
                                      , topLeftIndex
                                      )
                                    ]
                                )
                            |> List.concat
                    )
                |> List.concat
    in
    Drawable.indexedFaces material pointsAndNormals faces


cylinder : Material -> Point3d units coordinates -> Point3d units coordinates -> Quantity Float units -> Drawable units coordinates
cylinder material startPoint endPoint radius =
    case Direction3d.from startPoint endPoint of
        Just zDirection ->
            let
                zVector =
                    Direction3d.toVector zDirection

                negativeZVector =
                    Vector3d.reverse zVector

                length =
                    Point3d.distanceFrom startPoint endPoint

                localFrame =
                    Frame3d.withZDirection zDirection startPoint

                subdivisions =
                    72

                wedgeAngle =
                    Angle.turns 1 |> Quantity.divideBy subdivisions

                wedge index =
                    let
                        startAngle =
                            wedgeAngle |> Quantity.multiplyBy (toFloat index)

                        endAngle =
                            startAngle |> Quantity.plus wedgeAngle

                        startX =
                            radius |> Quantity.multiplyBy (Angle.cos startAngle)

                        endX =
                            radius |> Quantity.multiplyBy (Angle.cos endAngle)

                        startY =
                            radius |> Quantity.multiplyBy (Angle.sin startAngle)

                        endY =
                            radius |> Quantity.multiplyBy (Angle.sin endAngle)

                        p0 =
                            Point3d.xyzIn localFrame startX startY zero

                        p1 =
                            Point3d.xyzIn localFrame endX endY zero

                        p2 =
                            Point3d.xyzIn localFrame startX startY length

                        p3 =
                            Point3d.xyzIn localFrame endX endY length

                        startNormal =
                            Direction3d.fromAzimuthInAndElevationFrom
                                (Frame3d.xySketchPlane localFrame)
                                startAngle
                                zero
                                |> Direction3d.toVector

                        endNormal =
                            Direction3d.fromAzimuthInAndElevationFrom
                                (Frame3d.xySketchPlane localFrame)
                                endAngle
                                zero
                                |> Direction3d.toVector
                    in
                    [ ( ( startPoint, negativeZVector )
                      , ( p0, negativeZVector )
                      , ( p1, negativeZVector )
                      )
                    , ( ( p0, startNormal )
                      , ( p1, endNormal )
                      , ( p3, endNormal )
                      )
                    , ( ( p0, startNormal )
                      , ( p3, endNormal )
                      , ( p2, startNormal )
                      )
                    , ( ( endPoint, zVector )
                      , ( p2, zVector )
                      , ( p3, zVector )
                      )
                    ]

                wedges =
                    List.range 0 (subdivisions - 1) |> List.map wedge
            in
            Drawable.faces material (List.concat wedges)

        Nothing ->
            Drawable.empty


box : Material -> Quantity Float units -> Quantity Float units -> Quantity Float units -> Drawable units coordinates
box material x y z =
    let
        minX =
            Quantity.multiplyBy -0.5 x

        maxX =
            Quantity.multiplyBy 0.5 x

        minY =
            Quantity.multiplyBy -0.5 y

        maxY =
            Quantity.multiplyBy 0.5 y

        minZ =
            Quantity.multiplyBy -0.5 z

        maxZ =
            Quantity.multiplyBy 0.5 z

        p0 =
            Point3d.xyz minX minY minZ

        p1 =
            Point3d.xyz maxX minY minZ

        p2 =
            Point3d.xyz maxX maxY minZ

        p3 =
            Point3d.xyz minX maxY minZ

        p4 =
            Point3d.xyz minX minY maxZ

        p5 =
            Point3d.xyz maxX minY maxZ

        p6 =
            Point3d.xyz maxX maxY maxZ

        p7 =
            Point3d.xyz minX maxY maxZ
    in
    Drawable.triangles material
        [ Triangle3d.fromVertices p0 p2 p1
        , Triangle3d.fromVertices p0 p3 p2
        , Triangle3d.fromVertices p4 p5 p6
        , Triangle3d.fromVertices p4 p6 p7
        , Triangle3d.fromVertices p1 p2 p6
        , Triangle3d.fromVertices p1 p6 p5
        , Triangle3d.fromVertices p0 p7 p3
        , Triangle3d.fromVertices p0 p4 p7
        , Triangle3d.fromVertices p0 p1 p5
        , Triangle3d.fromVertices p0 p5 p4
        , Triangle3d.fromVertices p3 p6 p2
        , Triangle3d.fromVertices p3 p7 p6
        ]
