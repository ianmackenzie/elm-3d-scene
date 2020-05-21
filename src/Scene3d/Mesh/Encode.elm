module Scene3d.Mesh.Encode exposing (mesh, shadow)

{-| These functions let you encode [`Mesh`](Scene3d-Mesh#Mesh) and [`Shadow`](Scene3d-Mesh#Shadow)
values as JSON which can later be decoded using the functions in the
[`Mesh.Decode`](Scene3d-Mesh-Decode) module. This is useful mostly as an offline
pre-processing step to store meshes on a server in a form that can be
efficiently loaded and parsed at runtime.

See the [`Mesh.Decode`](Scene3d-Mesh-Decode) module for documentation on the
JSON format used by different mesh types.

@docs mesh, shadow

-}

import Json.Encode as Encode exposing (Value)
import Length exposing (Meters)
import LineSegment3d exposing (LineSegment3d)
import Math.Vector3
import Point3d exposing (Point3d)
import Polyline3d exposing (Polyline3d)
import Quantity exposing (Unitless)
import Scene3d.Mesh as Mesh exposing (Mesh)
import Scene3d.Types as Types
import Triangle3d exposing (Triangle3d)
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)


encodePoint : Point3d Meters coordinates -> Value
encodePoint point =
    let
        { x, y, z } =
            Point3d.toMeters point
    in
    Encode.object
        [ ( "x", Encode.float x )
        , ( "y", Encode.float y )
        , ( "z", Encode.float z )
        ]


encodeLineSegment : LineSegment3d Meters coordinates -> Value
encodeLineSegment lineSegment =
    let
        ( p1, p2 ) =
            LineSegment3d.endpoints lineSegment
    in
    Encode.list encodePoint [ p1, p2 ]


encodeTriangle : Triangle3d Meters coordinates -> Value
encodeTriangle triangle =
    let
        ( p1, p2, p3 ) =
            Triangle3d.vertices triangle
    in
    Encode.list encodePoint [ p1, p2, p3 ]


encodePolyline : Polyline3d Meters coordinates -> Value
encodePolyline polyline =
    Encode.list encodePoint (Polyline3d.vertices polyline)


encodeMeshVertices : (vertex -> Value) -> TriangularMesh vertex -> Value
encodeMeshVertices encodeVertex triangularMesh =
    Encode.array encodeVertex (TriangularMesh.vertices triangularMesh)


encodeMeshFace : ( Int, Int, Int ) -> Value
encodeMeshFace ( i, j, k ) =
    Encode.list Encode.int [ i, j, k ]


encodeMeshFaces : TriangularMesh vertex -> Value
encodeMeshFaces triangularMesh =
    Encode.list encodeMeshFace (TriangularMesh.faceIndices triangularMesh)


encodeUniformVertex :
    { position : Point3d Meters coordinates
    , normal : Vector3d Unitless coordinates
    }
    -> Value
encodeUniformVertex { position, normal } =
    let
        p =
            Point3d.toMeters position

        n =
            Vector3d.toUnitless normal
    in
    Encode.object
        [ ( "x", Encode.float p.x )
        , ( "y", Encode.float p.y )
        , ( "z", Encode.float p.z )
        , ( "nx", Encode.float n.x )
        , ( "ny", Encode.float n.y )
        , ( "nz", Encode.float n.z )
        ]


encodeUnlitVertex :
    { position : Point3d Meters coordinates
    , uv : ( Float, Float )
    }
    -> Value
encodeUnlitVertex { position, uv } =
    let
        p =
            Point3d.toMeters position

        ( u, v ) =
            uv
    in
    Encode.object
        [ ( "x", Encode.float p.x )
        , ( "y", Encode.float p.y )
        , ( "z", Encode.float p.z )
        , ( "u", Encode.float u )
        , ( "v", Encode.float v )
        ]


encodeTexturedVertex :
    { position : Point3d Meters coordinates
    , normal : Vector3d Unitless coordinates
    , uv : ( Float, Float )
    }
    -> Value
encodeTexturedVertex { position, normal, uv } =
    let
        p =
            Point3d.toMeters position

        n =
            Vector3d.toUnitless normal

        ( u, v ) =
            uv
    in
    Encode.object
        [ ( "x", Encode.float p.x )
        , ( "y", Encode.float p.y )
        , ( "z", Encode.float p.z )
        , ( "nx", Encode.float n.x )
        , ( "ny", Encode.float n.y )
        , ( "nz", Encode.float n.z )
        , ( "u", Encode.float u )
        , ( "v", Encode.float v )
        ]


encodeNormalMappedVertex :
    { position : Point3d Meters coordinates
    , normal : Vector3d Unitless coordinates
    , tangent : Vector3d Unitless coordinates
    , uv : ( Float, Float )
    }
    -> Value
encodeNormalMappedVertex { position, normal, tangent, uv } =
    let
        p =
            Point3d.toMeters position

        n =
            Vector3d.toUnitless normal

        t =
            Vector3d.toUnitless tangent

        ( u, v ) =
            uv
    in
    Encode.object
        [ ( "x", Encode.float p.x )
        , ( "y", Encode.float p.y )
        , ( "z", Encode.float p.z )
        , ( "nx", Encode.float n.x )
        , ( "ny", Encode.float n.y )
        , ( "nz", Encode.float n.z )
        , ( "tx", Encode.float t.x )
        , ( "ty", Encode.float t.y )
        , ( "tz", Encode.float t.z )
        , ( "u", Encode.float u )
        , ( "v", Encode.float v )
        ]


encodeTriangularMesh : (vertex -> Value) -> TriangularMesh vertex -> Value
encodeTriangularMesh encodeVertex triangularMesh =
    Encode.object
        [ ( "vertices", encodeMeshVertices encodeVertex triangularMesh )
        , ( "faces", encodeMeshFaces triangularMesh )
        ]


{-| Encode a `Mesh` value as JSON.
-}
mesh : Mesh coordinates attributes -> Value
mesh givenMesh =
    case givenMesh of
        Types.EmptyMesh ->
            Encode.null

        Types.Triangles _ triangles _ _ ->
            Encode.list encodeTriangle triangles

        Types.Facets _ triangles _ _ ->
            Encode.list encodeTriangle triangles

        Types.Indexed _ triangularMesh _ _ ->
            encodeTriangularMesh encodePoint triangularMesh

        Types.MeshWithNormals _ triangularMesh _ _ ->
            encodeTriangularMesh encodeUniformVertex triangularMesh

        Types.MeshWithUvs _ triangularMesh _ _ ->
            encodeTriangularMesh encodeUnlitVertex triangularMesh

        Types.MeshWithNormalsAndUvs _ triangularMesh _ _ ->
            encodeTriangularMesh encodeTexturedVertex triangularMesh

        Types.MeshWithTangents _ triangularMesh _ _ ->
            encodeTriangularMesh encodeNormalMappedVertex triangularMesh

        Types.LineSegments _ lineSegments _ ->
            Encode.list encodeLineSegment lineSegments

        Types.Polyline _ polyline _ ->
            encodePolyline polyline

        Types.Points _ radius points _ ->
            Encode.list encodePoint points


encodeShadowVertex : Types.VertexWithNormal -> Value
encodeShadowVertex vertex =
    let
        p =
            Math.Vector3.toRecord vertex.position

        n =
            Math.Vector3.toRecord vertex.normal
    in
    Encode.object
        [ ( "x", Encode.float p.x )
        , ( "y", Encode.float p.y )
        , ( "z", Encode.float p.z )
        , ( "nx", Encode.float n.x )
        , ( "ny", Encode.float n.y )
        , ( "nz", Encode.float n.z )
        ]


{-| Encode a `Shadow` value as JSON.
-}
shadow : Mesh.Shadow coordinates -> Value
shadow givenShadow =
    case givenShadow of
        Types.EmptyShadow ->
            Encode.null

        Types.Shadow _ triangularMesh _ ->
            encodeTriangularMesh encodeShadowVertex triangularMesh
