module Scene3d.Primitives exposing
    ( block
    , blockShadow
    , cone
    , coneShadow
    , cylinder
    , cylinderShadow
    , sphere
    )

import Angle
import Array
import Direction2d
import Direction3d exposing (Direction3d)
import Length exposing (Length, Meters)
import Parameter1d
import Point3d exposing (Point3d)
import Quantity exposing (Quantity, zero)
import Scene3d.Mesh as Mesh exposing (Mesh)
import SketchPlane3d
import Triangle3d exposing (Triangle3d)
import TriangularMesh
import Vector3d exposing (Vector3d)


sphere : Mesh.Textured coordinates
sphere =
    let
        n =
            72

        radius =
            Length.meters 1

        m =
            ceiling (toFloat n / 2)

        thetaValues =
            Parameter1d.steps n
                (Quantity.interpolateFrom zero (Angle.turns 1))

        phiValues =
            Parameter1d.steps m
                (Quantity.interpolateFrom
                    (Angle.degrees 90)
                    (Angle.degrees -90)
                )

        vertices =
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
                                        Direction3d.xyZ theta phi |> Direction3d.toVector
                                    , uv =
                                        ( Quantity.ratio theta (Angle.turns 1)
                                        , Quantity.ratio
                                            (phi |> Quantity.plus (Angle.degrees 90))
                                            (Angle.degrees 180)
                                        )

                                    -- , tangent =
                                    --     Direction3d.xy (theta |> Quantity.plus (Angle.degrees 90))
                                    --         |> Direction3d.toVector
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
            i * (m + 1) + j

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
    Mesh.texturedFaces (TriangularMesh.indexed vertices faces)
        |> Mesh.cullBackFaces


cylinder : Mesh.Uniform coordinates
cylinder =
    let
        radius =
            Length.meters 1

        subdivisions =
            72

        height =
            Length.meters 1

        wedgeAngle =
            Angle.turns 1 |> Quantity.divideBy (toFloat subdivisions)

        negativeZVector =
            Direction3d.negativeZ |> Direction3d.toVector

        positiveZVector =
            Direction3d.positiveZ |> Direction3d.toVector

        bottomZ =
            Quantity.multiplyBy -0.5 height

        topZ =
            Quantity.multiplyBy 0.5 height

        bottomCenter =
            Point3d.xyz zero zero bottomZ

        topCenter =
            Point3d.xyz zero zero topZ

        wedge startIndex =
            let
                startAngle =
                    wedgeAngle |> Quantity.multiplyBy (toFloat startIndex)

                endIndex =
                    startIndex + 1 |> modBy subdivisions

                endAngle =
                    wedgeAngle |> Quantity.multiplyBy (toFloat endIndex)

                startX =
                    radius |> Quantity.multiplyBy (Angle.cos startAngle)

                endX =
                    radius |> Quantity.multiplyBy (Angle.cos endAngle)

                startY =
                    radius |> Quantity.multiplyBy (Angle.sin startAngle)

                endY =
                    radius |> Quantity.multiplyBy (Angle.sin endAngle)

                p0 =
                    Point3d.xyz startX startY bottomZ

                p1 =
                    Point3d.xyz endX endY bottomZ

                p2 =
                    Point3d.xyz startX startY topZ

                p3 =
                    Point3d.xyz endX endY topZ

                startNormal =
                    Direction3d.on SketchPlane3d.xy
                        (Direction2d.fromAngle startAngle)
                        |> Direction3d.toVector

                endNormal =
                    Direction3d.on SketchPlane3d.xy
                        (Direction2d.fromAngle endAngle)
                        |> Direction3d.toVector
            in
            [ ( { position = bottomCenter, normal = negativeZVector }
              , { position = p1, normal = negativeZVector }
              , { position = p0, normal = negativeZVector }
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
    Mesh.indexedFaces triangularMesh |> Mesh.cullBackFaces


cone : Mesh.Uniform coordinates
cone =
    let
        radius =
            Length.meters 1

        subdivisions =
            72

        elevationAngle =
            Angle.degrees 45

        wedgeAngle =
            Angle.turns 1 |> Quantity.divideBy (toFloat subdivisions)

        negativeZVector =
            Direction3d.negativeZ |> Direction3d.toVector

        bottomZ =
            Quantity.zero

        topZ =
            Length.meters 1

        basePoint =
            Point3d.xyz zero zero bottomZ

        tipPoint =
            Point3d.xyz zero zero topZ

        wedge startIndex =
            let
                startAngle =
                    wedgeAngle |> Quantity.multiplyBy (toFloat startIndex)

                endIndex =
                    startIndex + 1 |> modBy subdivisions

                endAngle =
                    wedgeAngle |> Quantity.multiplyBy (toFloat endIndex)

                startX =
                    radius |> Quantity.multiplyBy (Angle.cos startAngle)

                endX =
                    radius |> Quantity.multiplyBy (Angle.cos endAngle)

                startY =
                    radius |> Quantity.multiplyBy (Angle.sin startAngle)

                endY =
                    radius |> Quantity.multiplyBy (Angle.sin endAngle)

                p0 =
                    Point3d.xyz startX startY bottomZ

                p1 =
                    Point3d.xyz endX endY bottomZ

                startNormal =
                    Direction3d.xyZ startAngle elevationAngle
                        |> Direction3d.toVector

                endNormal =
                    Direction3d.xyZ endAngle elevationAngle
                        |> Direction3d.toVector

                tipNormal =
                    Vector3d.zero
            in
            [ ( { position = basePoint, normal = negativeZVector }
              , { position = p1, normal = negativeZVector }
              , { position = p0, normal = negativeZVector }
              )
            , ( { position = p0, normal = startNormal }
              , { position = p1, normal = endNormal }
              , { position = tipPoint, normal = tipNormal }
              )
            ]

        wedges =
            List.range 0 (subdivisions - 1)
                |> List.map wedge

        triangularMesh =
            TriangularMesh.triangles (List.concat wedges)
    in
    Mesh.indexedFaces triangularMesh |> Mesh.cullBackFaces


block : Mesh.Uniform coordinates
block =
    let
        x =
            Length.meters 1

        y =
            Length.meters 1

        z =
            Length.meters 1

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
    Mesh.facets
        [ Triangle3d.from p0 p2 p1
        , Triangle3d.from p0 p3 p2
        , Triangle3d.from p4 p5 p6
        , Triangle3d.from p4 p6 p7
        , Triangle3d.from p1 p2 p6
        , Triangle3d.from p1 p6 p5
        , Triangle3d.from p0 p7 p3
        , Triangle3d.from p0 p4 p7
        , Triangle3d.from p0 p1 p5
        , Triangle3d.from p0 p5 p4
        , Triangle3d.from p3 p6 p2
        , Triangle3d.from p3 p7 p6
        ]
        |> Mesh.cullBackFaces


blockShadow : Mesh.Shadow coordinates
blockShadow =
    Mesh.shadow block


cylinderShadow : Mesh.Shadow coordinates
cylinderShadow =
    Mesh.shadow cylinder


coneShadow : Mesh.Shadow coordinates
coneShadow =
    Mesh.shadow cone
