module Shapes
    exposing
        ( box
        , cylinder
        , sphere
        )

import Math.Vector3 exposing (Vec3)
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point3d as Point3d
import OpenSolid.Scene.Geometry as Geometry exposing (Geometry)
import OpenSolid.Scene.Material as Material
import OpenSolid.Scene.Node as Node exposing (Node)


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
                                    ( Point3d
                                        ( x0 + radius * sin phi * cos theta
                                        , y0 + radius * sin phi * sin theta
                                        , z0 + radius * cos phi
                                        )
                                    , Direction3d
                                        ( sin phi * cos theta
                                        , sin phi * sin theta
                                        , cos phi
                                        )
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
    case Point3d.directionFrom startPoint endPoint of
        Just zDirection ->
            let
                ( xDirection, yDirection ) =
                    Direction3d.perpendicularBasis zDirection

                negativeZDirection =
                    Direction3d.flip zDirection

                length =
                    Point3d.distanceFrom startPoint endPoint

                frame =
                    Frame3d
                        { originPoint = startPoint
                        , xDirection = xDirection
                        , yDirection = yDirection
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
                            Direction3d ( cos startAngle, sin startAngle, 0.0 )
                                |> Direction3d.placeIn frame

                        endNormal =
                            Direction3d ( cos endAngle, sin endAngle, 0.0 )
                                |> Direction3d.placeIn frame
                    in
                    [ ( ( startPoint, negativeZDirection )
                      , ( p0, negativeZDirection )
                      , ( p1, negativeZDirection )
                      )
                    , ( ( p0, startNormal )
                      , ( p1, endNormal )
                      , ( p3, endNormal )
                      )
                    , ( ( p0, startNormal )
                      , ( p3, endNormal )
                      , ( p2, startNormal )
                      )
                    , ( ( endPoint, zDirection )
                      , ( p2, zDirection )
                      , ( p3, zDirection )
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
            Point3d ( -x / 2, -y / 2, -z / 2 )

        p1 =
            Point3d ( x / 2, -y / 2, -z / 2 )

        p2 =
            Point3d ( x / 2, y / 2, -z / 2 )

        p3 =
            Point3d ( -x / 2, y / 2, -z / 2 )

        p4 =
            Point3d ( -x / 2, -y / 2, z / 2 )

        p5 =
            Point3d ( x / 2, -y / 2, z / 2 )

        p6 =
            Point3d ( x / 2, y / 2, z / 2 )

        p7 =
            Point3d ( -x / 2, y / 2, z / 2 )
    in
    Geometry.facets
        [ Triangle3d ( p0, p2, p1 )
        , Triangle3d ( p0, p3, p2 )
        , Triangle3d ( p4, p5, p6 )
        , Triangle3d ( p4, p6, p7 )
        , Triangle3d ( p1, p2, p6 )
        , Triangle3d ( p1, p6, p5 )
        , Triangle3d ( p0, p7, p3 )
        , Triangle3d ( p0, p4, p7 )
        , Triangle3d ( p0, p1, p5 )
        , Triangle3d ( p0, p5, p4 )
        , Triangle3d ( p3, p6, p2 )
        , Triangle3d ( p3, p7, p6 )
        ]
