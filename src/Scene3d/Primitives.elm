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


sphere : Mesh.Bumpy coordinates
sphere =
    Mesh.cullBackFaces <|
        Mesh.bumpyFaces <|
            TriangularMesh.grid 72 72 <|
                \u v ->
                    let
                        theta =
                            2 * pi * u

                        phi =
                            -pi / 2 + pi * v

                        sinTheta =
                            sin theta

                        cosTheta =
                            cos theta

                        sinPhi =
                            sin phi

                        cosPhi =
                            cos phi

                        xyz =
                            { x = cosPhi * cosTheta
                            , y = cosPhi * sinTheta
                            , z = sinPhi
                            }
                    in
                    { position = Point3d.unsafe xyz
                    , normal = Vector3d.unsafe xyz
                    , uv = ( u, v )
                    , tangent = Vector3d.unsafe { x = -sinTheta, y = cosTheta, z = 0 }
                    , tangentBasisIsRightHanded = True
                    }


cylinder : Mesh.Uniform coordinates
cylinder =
    let
        negativeZVector =
            Vector3d.unsafe { x = 0, y = 0, z = -1 }

        positiveZVector =
            Vector3d.unsafe { x = 0, y = 0, z = 1 }

        bottomCenter =
            { position = Point3d.unsafe { x = 0, y = 0, z = -0.5 }
            , normal = negativeZVector
            }

        topCenter =
            { position = Point3d.unsafe { x = 0, y = 0, z = 0.5 }
            , normal = positiveZVector
            }

        bottom =
            TriangularMesh.radial bottomCenter <|
                Parameter1d.leading 72 <|
                    \u ->
                        let
                            theta =
                                2 * pi * u
                        in
                        { position = Point3d.unsafe { x = cos theta, y = -(sin theta), z = -0.5 }
                        , normal = negativeZVector
                        }

        top =
            TriangularMesh.radial topCenter <|
                Parameter1d.leading 72 <|
                    \u ->
                        let
                            theta =
                                2 * pi * u
                        in
                        { position = Point3d.unsafe { x = cos theta, y = sin theta, z = 0.5 }
                        , normal = positiveZVector
                        }

        sides =
            TriangularMesh.tube 1 72 <|
                \u v ->
                    let
                        theta =
                            2 * pi * v

                        sinTheta =
                            sin theta

                        cosTheta =
                            cos theta
                    in
                    { position = Point3d.unsafe { x = cosTheta, y = -sinTheta, z = u - 0.5 }
                    , normal = Vector3d.unsafe { x = cosTheta, y = -sinTheta, z = 0 }
                    }
    in
    Mesh.cullBackFaces <|
        Mesh.indexedFaces <|
            TriangularMesh.combine [ top, bottom, sides ]


cone : Mesh.Uniform coordinates
cone =
    let
        negativeZVector =
            Vector3d.unsafe { x = 0, y = 0, z = -1 }

        bottomCenter =
            { position = Point3d.origin
            , normal = negativeZVector
            }

        tip =
            { position = Point3d.unsafe { x = 0, y = 0, z = 1 }
            , normal = Vector3d.zero
            }

        halfRootTwo =
            0.5 * sqrt 2

        bottom =
            TriangularMesh.radial bottomCenter <|
                Parameter1d.leading 72 <|
                    \u ->
                        let
                            theta =
                                2 * pi * u

                            sinTheta =
                                sin theta

                            cosTheta =
                                cos theta
                        in
                        { position = Point3d.unsafe { x = cosTheta, y = -sinTheta, z = 0 }
                        , normal = negativeZVector
                        }

        sides =
            TriangularMesh.radial tip <|
                Parameter1d.leading 72 <|
                    \u ->
                        let
                            theta =
                                2 * pi * u

                            sinTheta =
                                sin theta

                            cosTheta =
                                cos theta
                        in
                        { position = Point3d.unsafe { x = cosTheta, y = sinTheta, z = 0 }
                        , normal =
                            Vector3d.unsafe
                                { x = cosTheta * halfRootTwo
                                , y = sinTheta * halfRootTwo
                                , z = halfRootTwo
                                }
                        }
    in
    Mesh.cullBackFaces <|
        Mesh.indexedFaces <|
            TriangularMesh.combine [ bottom, sides ]


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
