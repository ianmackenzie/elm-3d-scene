module Sphere exposing (sphere)

import Math.Vector3 exposing (Vec3)
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point3d as Point3d
import OpenSolid.Scene.Geometry as Geometry exposing (Geometry)
import OpenSolid.Scene.Lighting as Lighting
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
    Geometry.indexedTriangles pointsAndNormals faces
