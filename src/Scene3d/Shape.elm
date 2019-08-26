module Scene3d.Shape exposing
    ( block
    , cylinder
    , sphere
    )

import Angle
import Array
import Direction2d
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Length exposing (Length, Meters)
import Parameter1d
import Point3d exposing (Point3d)
import Quantity exposing (Quantity, zero)
import Scene3d.Drawable as Drawable exposing (Drawable, Material)
import Scene3d.Mesh as Mesh exposing (HasNormals, Mesh, NoTangents, NoUV)
import SketchPlane3d
import Triangle3d exposing (Triangle3d)
import TriangularMesh
import Vector3d exposing (Vector3d)


subdivisionsFor : { radius : Length, maxError : Length } -> Int
subdivisionsFor { radius, maxError } =
    if maxError |> Quantity.lessThanOrEqualTo Quantity.zero then
        0

    else if
        maxError
            |> Quantity.greaterThanOrEqualTo
                (Quantity.twice (Quantity.abs radius))
    then
        0

    else
        let
            maxSegmentAngle =
                Quantity.twice <|
                    Angle.acos <|
                        (1 - Quantity.ratio maxError (Quantity.abs radius))
        in
        ceiling (Quantity.ratio (Angle.turns 1) maxSegmentAngle)


sphere : { radius : Length, maxError : Length } -> Mesh coordinates HasNormals NoUV NoTangents
sphere { radius, maxError } =
    let
        n =
            subdivisionsFor { radius = radius, maxError = maxError }

        m =
            ceiling (toFloat n / 2)

        thetaValues =
            Parameter1d.leading n
                (Quantity.interpolateFrom zero (Angle.turns 1))

        phiValues =
            Parameter1d.steps m
                (Quantity.interpolateFrom
                    (Angle.degrees 90)
                    (Angle.degrees -90)
                )

        pointsAndNormals =
            thetaValues
                |> List.map
                    (\theta ->
                        phiValues
                            |> List.map
                                (\phi ->
                                    { position =
                                        Point3d.xyz
                                            (radius |> Quantity.multiplyBy (Angle.cos phi * Angle.cos theta))
                                            (radius |> Quantity.multiplyBy (Angle.cos phi * Angle.sin theta))
                                            (radius |> Quantity.multiplyBy (Angle.sin phi))
                                    , normal =
                                        Direction3d.fromAzimuthInAndElevationFrom
                                            SketchPlane3d.xy
                                            theta
                                            phi
                                            |> Direction3d.toVector
                                    }
                                )
                    )
                |> List.concat
                |> Array.fromList

        thetaStartIndices =
            List.range 0 (n - 1)

        phiStartIndices =
            List.range 0 (m - 1)

        linearIndex i j =
            (i |> modBy n) * (m + 1) + j

        faces =
            thetaStartIndices
                |> List.map
                    (\i ->
                        phiStartIndices
                            |> List.map
                                (\j ->
                                    let
                                        bottomLeftIndex =
                                            linearIndex i (j + 1)

                                        bottomRightIndex =
                                            linearIndex (i + 1) (j + 1)

                                        topLeftIndex =
                                            linearIndex i j

                                        topRightIndex =
                                            linearIndex (i + 1) j
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
    Mesh.smooth Mesh.cullBackFaces <|
        TriangularMesh.indexed pointsAndNormals faces


cylinder : { radius : Length, height : Length, maxError : Length } -> Mesh coordinates HasNormals NoUV NoTangents
cylinder { radius, height, maxError } =
    let
        subdivisions =
            subdivisionsFor { radius = radius, maxError = maxError }

        wedgeAngle =
            Angle.turns 1 |> Quantity.divideBy (toFloat subdivisions)

        negativeZVector =
            Direction3d.negativeZ |> Direction3d.toVector

        positiveZVector =
            Direction3d.positiveZ |> Direction3d.toVector

        topCenter =
            Point3d.xyz zero zero height

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
                    Point3d.xyz startX startY zero

                p1 =
                    Point3d.xyz endX endY zero

                p2 =
                    Point3d.xyz startX startY height

                p3 =
                    Point3d.xyz endX endY height

                startNormal =
                    Direction3d.on SketchPlane3d.xy
                        (Direction2d.fromAngle startAngle)
                        |> Direction3d.toVector

                endNormal =
                    Direction3d.on SketchPlane3d.xy
                        (Direction2d.fromAngle endAngle)
                        |> Direction3d.toVector
            in
            [ ( { position = Point3d.origin, normal = negativeZVector }
              , { position = p0, normal = negativeZVector }
              , { position = p1, normal = negativeZVector }
              )
            , ( { position = p0, normal = startNormal }
              , { position = p1, normal = endNormal }
              , { position = p3, normal = endNormal }
              )
            , ( { position = p0, normal = startNormal }
              , { position = p3, normal = endNormal }
              , { position = p2, normal = startNormal }
              )
            , ( { position = topCenter, normal = positiveZVector }
              , { position = p2, normal = positiveZVector }
              , { position = p3, normal = positiveZVector }
              )
            ]

        wedges =
            List.range 0 (subdivisions - 1)
                |> List.map wedge

        triangularMesh =
            TriangularMesh.triangles (List.concat wedges)
    in
    Mesh.smooth Mesh.cullBackFaces triangularMesh


block : Length -> Length -> Length -> Mesh coordinates HasNormals NoUV NoTangents
block x y z =
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
    Mesh.facets Mesh.cullBackFaces
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
