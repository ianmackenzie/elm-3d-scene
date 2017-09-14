module Shapes
    exposing
        ( box
        , cylinder
        , sphere
        )

import OpenSolid.Direction3d as Direction3d exposing (Direction3d)
import OpenSolid.Frame3d as Frame3d exposing (Frame3d)
import OpenSolid.Point3d as Point3d exposing (Point3d)
import OpenSolid.Scene.Geometry as Geometry exposing (Geometry)
import OpenSolid.Triangle3d as Triangle3d exposing (Triangle3d)
import OpenSolid.Vector3d as Vector3d exposing (Vector3d)


sphere : Point3d -> Float -> Geometry
sphere centerPoint radius =
    let
        ( x0, y0, z0 ) =
            Point3d.coordinates centerPoint

        subdivisions =
            72

        angle =
            2 * pi / subdivisions

        angleValues =
            List.range 0 subdivisions
                |> List.map (\i -> toFloat i * angle)

        pointsAndNormals =
            angleValues
                |> List.map
                    (\theta ->
                        angleValues
                            |> List.map
                                (\phi ->
                                    ( Point3d.fromCoordinates
                                        ( x0 + radius * sin phi * cos theta
                                        , y0 + radius * sin phi * sin theta
                                        , z0 + radius * cos phi
                                        )
                                    , Direction3d.with
                                        { azimuth = theta
                                        , elevation = pi / 2 - phi
                                        }
                                        |> Direction3d.toVector
                                    )
                                )
                    )
                |> List.concat

        startIndices =
            List.range 0 (subdivisions - 1)

        linearIndex thetaIndex phiIndex =
            (thetaIndex % subdivisions) * subdivisions + (phiIndex % subdivisions)

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
    Geometry.indexedFaces pointsAndNormals faces


cylinder : Point3d -> Point3d -> Float -> Geometry
cylinder startPoint endPoint radius =
    case Direction3d.from startPoint endPoint of
        Just zDirection ->
            let
                zVector =
                    Direction3d.toVector zDirection

                negativeZVector =
                    Vector3d.flip zVector

                length =
                    Point3d.distanceFrom startPoint endPoint

                frame =
                    Frame3d.with
                        { originPoint = startPoint
                        , zDirection = zDirection
                        }

                subdivisions =
                    72

                wedgeAngle =
                    2 * pi / subdivisions

                wedge index =
                    let
                        startAngle =
                            wedgeAngle * toFloat index

                        endAngle =
                            startAngle + wedgeAngle

                        startX =
                            radius * cos startAngle

                        endX =
                            radius * cos endAngle

                        startY =
                            radius * sin startAngle

                        endY =
                            radius * sin endAngle

                        p0 =
                            Point3d.in_ frame ( startX, startY, 0.0 )

                        p1 =
                            Point3d.in_ frame ( endX, endY, 0.0 )

                        p2 =
                            Point3d.in_ frame ( startX, startY, length )

                        p3 =
                            Point3d.in_ frame ( endX, endY, length )

                        startNormal =
                            Direction3d.with
                                { azimuth = startAngle
                                , elevation = 0
                                }
                                |> Direction3d.placeIn frame
                                |> Direction3d.toVector

                        endNormal =
                            Direction3d.with
                                { azimuth = endAngle
                                , elevation = 0
                                }
                                |> Direction3d.placeIn frame
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
            Geometry.faces (List.concat wedges)

        Nothing ->
            Geometry.faces []


box : Float -> Float -> Float -> Geometry
box x y z =
    let
        p0 =
            Point3d.fromCoordinates ( -x / 2, -y / 2, -z / 2 )

        p1 =
            Point3d.fromCoordinates ( x / 2, -y / 2, -z / 2 )

        p2 =
            Point3d.fromCoordinates ( x / 2, y / 2, -z / 2 )

        p3 =
            Point3d.fromCoordinates ( -x / 2, y / 2, -z / 2 )

        p4 =
            Point3d.fromCoordinates ( -x / 2, -y / 2, z / 2 )

        p5 =
            Point3d.fromCoordinates ( x / 2, -y / 2, z / 2 )

        p6 =
            Point3d.fromCoordinates ( x / 2, y / 2, z / 2 )

        p7 =
            Point3d.fromCoordinates ( -x / 2, y / 2, z / 2 )
    in
    Geometry.facets
        [ Triangle3d.fromVertices ( p0, p2, p1 )
        , Triangle3d.fromVertices ( p0, p3, p2 )
        , Triangle3d.fromVertices ( p4, p5, p6 )
        , Triangle3d.fromVertices ( p4, p6, p7 )
        , Triangle3d.fromVertices ( p1, p2, p6 )
        , Triangle3d.fromVertices ( p1, p6, p5 )
        , Triangle3d.fromVertices ( p0, p7, p3 )
        , Triangle3d.fromVertices ( p0, p4, p7 )
        , Triangle3d.fromVertices ( p0, p1, p5 )
        , Triangle3d.fromVertices ( p0, p5, p4 )
        , Triangle3d.fromVertices ( p3, p6, p2 )
        , Triangle3d.fromVertices ( p3, p7, p6 )
        ]
