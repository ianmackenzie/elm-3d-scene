module OpenSolid.Scene.Geometry
    exposing
        ( Geometry
        , body
        , empty
        , faces
        , facets
        , indexedFaces
        , mesh
        , shaded
        , surface
        )

import Array.Hamt as Array
import Math.Vector3 exposing (Vec3, vec3)
import OpenSolid.Body3d as Body3d exposing (Body3d)
import OpenSolid.BoundingBox3d as BoundingBox3d exposing (BoundingBox3d)
import OpenSolid.Interop.LinearAlgebra.Direction3d as Direction3d
import OpenSolid.Interop.LinearAlgebra.Point3d as Point3d
import OpenSolid.Interop.LinearAlgebra.Vector3d as Vector3d
import OpenSolid.Mesh as Mesh exposing (Mesh)
import OpenSolid.Point3d as Point3d exposing (Point3d)
import OpenSolid.Scene.Material exposing (Material)
import OpenSolid.Scene.Node exposing (Node)
import OpenSolid.Scene.Types as Types
import OpenSolid.Surface3d as Surface3d exposing (Surface3d)
import OpenSolid.Triangle3d as Triangle3d exposing (Triangle3d)
import OpenSolid.Vector3d as Vector3d exposing (Vector3d)
import WebGL


type alias Geometry =
    Types.Geometry


shaded : Material -> Geometry -> Node
shaded material geometry =
    case geometry of
        Types.Geometry boundingBox mesh ->
            Types.LeafNode (Types.ShadedGeometry material boundingBox mesh)

        Types.EmptyGeometry ->
            Types.EmptyNode


empty : Geometry
empty =
    Types.EmptyGeometry


facets : List Triangle3d -> Geometry
facets triangles =
    case BoundingBox3d.hullOf (List.map Triangle3d.boundingBox triangles) of
        Just boundingBox ->
            let
                vertexAttributes point normal =
                    { position = Point3d.toVec3 point
                    , normal = normal
                    }

                toAttributes triangle =
                    let
                        ( p1, p2, p3 ) =
                            Triangle3d.vertices triangle

                        normalVector =
                            Triangle3d.normalDirection triangle
                                |> Maybe.map Direction3d.toVec3
                                |> Maybe.withDefault (vec3 0 0 0)
                    in
                    ( vertexAttributes p1 normalVector
                    , vertexAttributes p2 normalVector
                    , vertexAttributes p3 normalVector
                    )

                attributes =
                    List.map toAttributes triangles

                mesh =
                    WebGL.triangles attributes
            in
            Types.Geometry boundingBox mesh

        Nothing ->
            Types.EmptyGeometry


type alias Vertex =
    ( Point3d, Vector3d )


type alias FaceVertices =
    ( Vertex, Vertex, Vertex )


faceBoundingBox : FaceVertices -> BoundingBox3d
faceBoundingBox ( ( p1, _ ), ( p2, _ ), ( p3, _ ) ) =
    Triangle3d.boundingBox (Triangle3d.withVertices ( p1, p2, p3 ))


faceAttributes : FaceVertices -> ( Types.VertexAttributes, Types.VertexAttributes, Types.VertexAttributes )
faceAttributes ( ( p1, n1 ), ( p2, n2 ), ( p3, n3 ) ) =
    ( { position = Point3d.toVec3 p1, normal = Vector3d.toVec3 n1 }
    , { position = Point3d.toVec3 p2, normal = Vector3d.toVec3 n2 }
    , { position = Point3d.toVec3 p3, normal = Vector3d.toVec3 n3 }
    )


faces : List ( ( Point3d, Vector3d ), ( Point3d, Vector3d ), ( Point3d, Vector3d ) ) -> Geometry
faces vertexTriples =
    case BoundingBox3d.hullOf (List.map faceBoundingBox vertexTriples) of
        Just boundingBox ->
            let
                mesh =
                    WebGL.triangles (List.map faceAttributes vertexTriples)
            in
            Types.Geometry boundingBox mesh

        Nothing ->
            Types.EmptyGeometry


vertexAttributes : Vertex -> Types.VertexAttributes
vertexAttributes ( point, normalVector ) =
    { position = Point3d.toVec3 point
    , normal = Vector3d.toVec3 normalVector
    }


indexedFaces : List ( Point3d, Vector3d ) -> List ( Int, Int, Int ) -> Geometry
indexedFaces vertices faces =
    let
        vertexPoints =
            List.map Tuple.first vertices
    in
    case Point3d.hullOf vertexPoints of
        Just boundingBox ->
            let
                attributes =
                    List.map vertexAttributes vertices

                mesh =
                    WebGL.indexedTriangles attributes faces
            in
            Types.Geometry boundingBox mesh

        Nothing ->
            Types.EmptyGeometry


mesh : Mesh ( Point3d, Vector3d ) -> Geometry
mesh mesh_ =
    indexedFaces (Array.toList (Mesh.vertices mesh_)) (Mesh.faceIndices mesh_)


surface : Float -> Surface3d -> Geometry
surface tolerance surface_ =
    mesh (Surface3d.toMesh tolerance surface_)


body : Float -> Body3d -> Geometry
body tolerance body_ =
    Body3d.surfaces body_
        |> List.map (Surface3d.toMesh tolerance)
        |> Mesh.combine
        |> mesh
