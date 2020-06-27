module Scene3d.Mesh exposing
    ( Mesh
    , Plain, Uniform, Unlit, Textured
    , points, lineSegments, polyline, triangles, facets
    , indexedTriangles, indexedFacets, indexedFaces
    , texturedTriangles, texturedFacets, texturedFaces
    , Shadow, shadow
    , cullBackFaces
    )

{-| This module lets you create object shapes which you can later render with
[`Scene3d.mesh`](Scene3d#mesh) or [`Scene3d.meshWithShadow`](Scene3d#meshWithShadow)
by applying a desired [material](Scene3d-Material).

**IMPORTANT**: `Mesh` (and `Shadow`) values should _not_ be created dynamically
in your `view` function, since this is a relatively expensive operation that can
lead to low frame rates or even out-of-memory errors if done repeatedly.
Instead, they should be created only once (in your `init` or `update` function,
or even as a top-level constant), then stored in your model and reused from
frame to frame.

Note that this does _not_ mean that objects can't move around or change color -
you can freely change the material applied to a mesh from frame to frame, and
you can use the various provided [transformation functions](Scene3d#transformations)
to transform meshes around without having to recreate them.

@docs Mesh


## Type aliases

These type aliases make it easier to write down type annotations for meshes you
store in your model or return from a function.

@docs Plain, Uniform, Unlit, Textured


# Constructors

@docs points, lineSegments, polyline, triangles, facets


## Indexed meshes

These functions all use the `TriangularMesh` type from the
[`ianmackenzie/elm-triangular-mesh`](https://package.elm-lang.org/packages/ianmackenzie/elm-triangular-mesh/latest/)
package to allow the creation of _indexed_ meshes, where vertices can be shared
between adjacent faces to save on space.

@docs indexedTriangles, indexedFacets, indexedFaces


## Textured meshes

These functions behave just like their corresponding `indexed` versions but
additionally require each vertex to include [UV](https://learnopengl.com/Getting-started/Textures)
(texture) coordinates to allow [textured materials](Scene3d-Material#textured-materials)
to be used.

@docs texturedTriangles, texturedFacets, texturedFaces


# Shadows

In `elm-3d-scene`, to render an object with its shadow you will first need to
construct (and save somewhere) a `Shadow` value for that object. You can then
render the object with its shadow using [`Scene3d.meshWithShadow`](Scene3d#meshWithShadow).

@docs Shadow, shadow


# Optimizations

@docs cullBackFaces

-}

import Array
import BoundingBox3d exposing (BoundingBox3d)
import Dict exposing (Dict)
import Geometry.Interop.LinearAlgebra.Point3d as Point3d
import Geometry.Interop.LinearAlgebra.Vector3d as Vector3d
import Length exposing (Meters)
import LineSegment3d exposing (LineSegment3d)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Pixels exposing (Pixels)
import Point3d exposing (Point3d)
import Polyline3d exposing (Polyline3d)
import Quantity exposing (Quantity(..), Unitless)
import Scene3d.Types as Types
    exposing
        ( BackFaceSetting(..)
        , Bounds
        , PlainVertex
        , Shadow
        , VertexWithNormal
        , VertexWithNormalAndUv
        , VertexWithTangent
        , VertexWithUv
        )
import Triangle3d exposing (Triangle3d)
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)
import WebGL


{-| A `Mesh` defines the shape of an object. It consists of a number of
_vertices_ (points that may have additional associated data like normal vectors
and texture coordinates) joined together into triangles or line segments (or
sometimes just displayed as dots).

The two type parameters of the `Mesh` type define what coordinate system a mesh
is defined in, and what attributes (in addition to position) are present on each
vertex. For example, a

    Mesh WorldCoordinates { normals : () }

refers to a mesh defined in `WorldCoordinates` (a type you would typically
define yourself) that has position and normal vector defined at each vertex.
(The `()` type isn't significant; you can think of it as meaning 'present'.)

-}
type alias Mesh coordinates attributes =
    Types.Mesh coordinates attributes


{-| A mesh containing vertex positions only.
-}
type alias Plain coordinates =
    Mesh coordinates {}


{-| A mesh with normal vectors at each vertex but no UV (texture) coordinates,
meaning that surface appearance will be uniform across the mesh.
-}
type alias Uniform coordinates =
    Mesh coordinates { normals : () }


{-| A mesh with UV coordinates at each vertex but no normal vectors (normal
vectors are required for any kind of lighting calculation).
-}
type alias Unlit coordinates =
    Mesh coordinates { uvs : () }


{-| A mesh with both normal vectors and UV coordinates at each vertex, allowing
for general-purpose texturing of lit objects.
-}
type alias Textured coordinates =
    Mesh coordinates { normals : (), uvs : () }


{-| A mesh with normal vectors, UV coordinates and tangent vectors at each
vertex, allowing for full texturing including normal maps.
-}
type alias NormalMapped coordinates =
    Mesh coordinates { normals : (), uvs : (), tangents : () }


{-| A mesh with normal and tangent vectors but no UV coordinates, allowing for
some specialized material models such as brushed metal but no texturing.
-}
type alias Anisotropic coordinates =
    Mesh coordinates { normals : (), tangents : () }


{-| Represents the shadow of a particular object.
-}
type alias Shadow coordinates =
    Types.Shadow coordinates


empty : Mesh coordinates attributes
empty =
    Types.EmptyMesh


plainVertex : Point3d Meters coordinates -> PlainVertex
plainVertex point =
    { position = Point3d.toVec3 point }


triangleAttributes :
    Triangle3d Meters coordinates
    -> ( PlainVertex, PlainVertex, PlainVertex )
triangleAttributes triangle =
    let
        ( p1, p2, p3 ) =
            Triangle3d.vertices triangle
    in
    ( plainVertex p1, plainVertex p2, plainVertex p3 )


triangleNormal :
    Point3d Meters coordinates
    -> Point3d Meters coordinates
    -> Point3d Meters coordinates
    -> Vector3d Unitless coordinates
triangleNormal p1 p2 p3 =
    let
        v1 =
            Vector3d.from p1 p2

        v2 =
            Vector3d.from p2 p3
    in
    Vector3d.normalize (v1 |> Vector3d.cross v2)


facetAttributes :
    Triangle3d Meters coordinates
    -> ( VertexWithNormal, VertexWithNormal, VertexWithNormal )
facetAttributes triangle =
    let
        ( p1, p2, p3 ) =
            Triangle3d.vertices triangle

        normal =
            Vector3d.toVec3 (triangleNormal p1 p2 p3)
    in
    ( { position = Point3d.toVec3 p1, normal = normal }
    , { position = Point3d.toVec3 p2, normal = normal }
    , { position = Point3d.toVec3 p3, normal = normal }
    )


{-| Construct a plain mesh from a list of triangles.

![Plain triangles mesh](https://ianmackenzie.github.io/elm-3d-scene/images/1.0.0/triangles.png)

-}
triangles : List (Triangle3d Meters coordinates) -> Plain coordinates
triangles givenTriangles =
    case givenTriangles of
        [] ->
            Types.EmptyMesh

        first :: rest ->
            let
                bounds =
                    BoundingBox3d.aggregateOf Triangle3d.boundingBox first rest

                webGLMesh =
                    WebGL.triangles (List.map triangleAttributes givenTriangles)
            in
            Types.Triangles bounds givenTriangles webGLMesh KeepBackFaces


{-| Construct a mesh from a list of triangles, but generate a normal vector for
each triangle so that [`matte`](Material#matte) and [`pbr`](Material#pbr)
(materials that react to light) can be used. Note that normal vectors will _not_
be smoothed, so the resulting mesh will appear to have flat [facets](https://en.wikipedia.org/wiki/Facet),
hence the name.
-}
facets : List (Triangle3d Meters coordinates) -> Uniform coordinates
facets givenTriangles =
    case givenTriangles of
        [] ->
            Types.EmptyMesh

        first :: rest ->
            let
                bounds =
                    BoundingBox3d.aggregateOf Triangle3d.boundingBox first rest

                webGLMesh =
                    WebGL.triangles (List.map facetAttributes givenTriangles)
            in
            Types.Facets bounds givenTriangles webGLMesh KeepBackFaces


collectPlain : Point3d Meters coordinates -> List PlainVertex -> List PlainVertex
collectPlain point accumulated =
    { position = Point3d.toVec3 point } :: accumulated


plainBoundsHelp :
    Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> List PlainVertex
    -> BoundingBox3d Meters coordinates
plainBoundsHelp minX maxX minY maxY minZ maxZ remaining =
    case remaining of
        next :: rest ->
            let
                x =
                    Math.Vector3.getX next.position

                y =
                    Math.Vector3.getY next.position

                z =
                    Math.Vector3.getZ next.position
            in
            plainBoundsHelp
                (min minX x)
                (max maxX x)
                (min minY y)
                (max maxY y)
                (min minZ z)
                (max maxZ z)
                rest

        [] ->
            BoundingBox3d.fromExtrema
                { minX = Quantity minX
                , maxX = Quantity maxX
                , minY = Quantity minY
                , maxY = Quantity maxY
                , minZ = Quantity minZ
                , maxZ = Quantity maxZ
                }


plainBounds : PlainVertex -> List PlainVertex -> BoundingBox3d Meters coordinates
plainBounds first rest =
    let
        x =
            Math.Vector3.getX first.position

        y =
            Math.Vector3.getY first.position

        z =
            Math.Vector3.getZ first.position
    in
    plainBoundsHelp x x y y z z rest


{-| Construct a mesh from a `TriangularMesh` of plain positions.
-}
indexedTriangles : TriangularMesh (Point3d Meters coordinates) -> Plain coordinates
indexedTriangles givenMesh =
    let
        collectedVertices =
            Array.foldr collectPlain [] (TriangularMesh.vertices givenMesh)
    in
    case collectedVertices of
        [] ->
            Types.EmptyMesh

        first :: rest ->
            let
                bounds =
                    plainBounds first rest

                webGLMesh =
                    WebGL.indexedTriangles
                        collectedVertices
                        (TriangularMesh.faceIndices givenMesh)
            in
            Types.Indexed bounds givenMesh webGLMesh KeepBackFaces


{-| Construct a mesh from a `TriangularMesh` of plain positions, but also
compute a normal vector for each triangle just like with [`facets`](#facets).
-}
indexedFacets : TriangularMesh (Point3d Meters coordinates) -> Uniform coordinates
indexedFacets givenMesh =
    facets (List.map Triangle3d.fromVertices (TriangularMesh.faceVertices givenMesh))


collectSmooth :
    { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates }
    -> List VertexWithNormal
    -> List VertexWithNormal
collectSmooth { position, normal } accumulated =
    { position = Point3d.toVec3 position, normal = Vector3d.toVec3 normal }
        :: accumulated


vertexBoundsHelp :
    Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> List { a | position : Vec3 }
    -> BoundingBox3d Meters coordinates
vertexBoundsHelp minX maxX minY maxY minZ maxZ remaining =
    case remaining of
        next :: rest ->
            let
                x =
                    Math.Vector3.getX next.position

                y =
                    Math.Vector3.getY next.position

                z =
                    Math.Vector3.getZ next.position
            in
            vertexBoundsHelp
                (min minX x)
                (max maxX x)
                (min minY y)
                (max maxY y)
                (min minZ z)
                (max maxZ z)
                rest

        [] ->
            BoundingBox3d.fromExtrema
                { minX = Quantity minX
                , maxX = Quantity maxX
                , minY = Quantity minY
                , maxY = Quantity maxY
                , minZ = Quantity minZ
                , maxZ = Quantity maxZ
                }


vertexBounds : { a | position : Vec3 } -> List { a | position : Vec3 } -> BoundingBox3d Meters coordinates
vertexBounds first rest =
    let
        x =
            Math.Vector3.getX first.position

        y =
            Math.Vector3.getY first.position

        z =
            Math.Vector3.getZ first.position
    in
    vertexBoundsHelp x x y y z z rest


{-| Construct a mesh from a `TriangularMesh` of vertices with positions and
normal vectors.
-}
indexedFaces :
    TriangularMesh { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates }
    -> Uniform coordinates
indexedFaces givenMesh =
    let
        collectedVertices =
            Array.foldr collectSmooth [] (TriangularMesh.vertices givenMesh)
    in
    case collectedVertices of
        [] ->
            Types.EmptyMesh

        first :: rest ->
            let
                bounds =
                    vertexBounds first rest

                webGLMesh =
                    WebGL.indexedTriangles
                        collectedVertices
                        (TriangularMesh.faceIndices givenMesh)
            in
            Types.MeshWithNormals bounds givenMesh webGLMesh KeepBackFaces


collectTextured :
    { position : Point3d Meters coordinates, uv : ( Float, Float ) }
    -> List VertexWithUv
    -> List VertexWithUv
collectTextured { position, uv } accumulated =
    let
        ( u, v ) =
            uv
    in
    { position = Point3d.toVec3 position, uv = Math.Vector2.vec2 u v }
        :: accumulated


{-| Construct a mesh from a `TriangularMesh` of vertices with positions and
texture coordinates.
-}
texturedTriangles :
    TriangularMesh { position : Point3d Meters coordinates, uv : ( Float, Float ) }
    -> Unlit coordinates
texturedTriangles givenMesh =
    let
        collectedVertices =
            Array.foldr collectTextured [] (TriangularMesh.vertices givenMesh)
    in
    case collectedVertices of
        [] ->
            Types.EmptyMesh

        first :: rest ->
            let
                bounds =
                    vertexBounds first rest

                webGLMesh =
                    WebGL.indexedTriangles
                        collectedVertices
                        (TriangularMesh.faceIndices givenMesh)
            in
            Types.MeshWithUvs bounds givenMesh webGLMesh KeepBackFaces


type alias TexturedTriangleVertex coordinates =
    { position : Point3d Meters coordinates
    , uv : ( Float, Float )
    }


type alias TexturedFacetVertex coordinates =
    { position : Point3d Meters coordinates
    , uv : ( Float, Float )
    , normal : Vector3d Unitless coordinates
    }


collectTexturedFacetVertices :
    ( TexturedTriangleVertex coordinates, TexturedTriangleVertex coordinates, TexturedTriangleVertex coordinates )
    -> List (TexturedFacetVertex coordinates)
    -> List (TexturedFacetVertex coordinates)
collectTexturedFacetVertices ( tv1, tv2, tv3 ) accumulated =
    let
        normal =
            triangleNormal tv1.position tv2.position tv3.position

        fv1 =
            { position = tv1.position
            , uv = tv1.uv
            , normal = normal
            }

        fv2 =
            { position = tv2.position
            , uv = tv2.uv
            , normal = normal
            }

        fv3 =
            { position = tv3.position
            , uv = tv3.uv
            , normal = normal
            }
    in
    fv1 :: fv2 :: fv3 :: accumulated


texturedFacetFaceIndices : Int -> List ( Int, Int, Int ) -> List ( Int, Int, Int )
texturedFacetFaceIndices count accumulated =
    if count <= 0 then
        accumulated

    else
        texturedFacetFaceIndices (count - 3) (( count - 3, count - 2, count - 1 ) :: accumulated)


{-| Construct a mesh from a `TriangularMesh` of vertices with positions and
texture coordinates, but additionally compute a normal vector for each triangle.
-}
texturedFacets :
    TriangularMesh { position : Point3d Meters coordinates, uv : ( Float, Float ) }
    -> Textured coordinates
texturedFacets givenMesh =
    let
        collectedVertices =
            Array.fromList <|
                List.foldr collectTexturedFacetVertices [] <|
                    TriangularMesh.faceVertices givenMesh

        faceIndices =
            texturedFacetFaceIndices (Array.length collectedVertices) []
    in
    texturedFaces (TriangularMesh.indexed collectedVertices faceIndices)


collectSmoothTextured :
    { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates, uv : ( Float, Float ) }
    -> List VertexWithNormalAndUv
    -> List VertexWithNormalAndUv
collectSmoothTextured { position, normal, uv } accumulated =
    let
        ( u, v ) =
            uv
    in
    { position = Point3d.toVec3 position, normal = Vector3d.toVec3 normal, uv = Math.Vector2.vec2 u v }
        :: accumulated


{-| Construct a mesh from a `TriangularMesh` of vertices with positions, normal
vectors and texture coordinates.
-}
texturedFaces :
    TriangularMesh
        { position : Point3d Meters coordinates
        , normal : Vector3d Unitless coordinates
        , uv : ( Float, Float )
        }
    -> Textured coordinates
texturedFaces givenMesh =
    let
        collectedVertices =
            Array.foldr collectSmoothTextured [] (TriangularMesh.vertices givenMesh)
    in
    case collectedVertices of
        [] ->
            Types.EmptyMesh

        first :: rest ->
            let
                bounds =
                    vertexBounds first rest

                webGLMesh =
                    WebGL.indexedTriangles
                        collectedVertices
                        (TriangularMesh.faceIndices givenMesh)
            in
            Types.MeshWithNormalsAndUvs bounds givenMesh webGLMesh KeepBackFaces


collectNormalMapped :
    { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates, uv : ( Float, Float ), tangent : Vector3d Unitless coordinates }
    -> List VertexWithTangent
    -> List VertexWithTangent
collectNormalMapped { position, normal, uv, tangent } accumulated =
    let
        ( u, v ) =
            uv
    in
    { position = Point3d.toVec3 position, normal = Vector3d.toVec3 normal, uv = Math.Vector2.vec2 u v, tangent = Vector3d.toVec3 tangent }
        :: accumulated


normalMapped :
    TriangularMesh
        { position : Point3d Meters coordinates
        , normal : Vector3d Unitless coordinates
        , uv : ( Float, Float )
        , tangent : Vector3d Unitless coordinates
        }
    -> NormalMapped coordinates
normalMapped givenMesh =
    let
        collectedVertices =
            Array.foldr collectNormalMapped [] (TriangularMesh.vertices givenMesh)
    in
    case collectedVertices of
        [] ->
            Types.EmptyMesh

        first :: rest ->
            let
                bounds =
                    vertexBounds first rest

                webGLMesh =
                    WebGL.indexedTriangles
                        collectedVertices
                        (TriangularMesh.faceIndices givenMesh)
            in
            Types.MeshWithTangents bounds givenMesh webGLMesh KeepBackFaces


lineSegmentAttributes : LineSegment3d Meters coordinates -> ( PlainVertex, PlainVertex )
lineSegmentAttributes givenSegment =
    let
        ( p1, p2 ) =
            LineSegment3d.endpoints givenSegment
    in
    ( plainVertex p1, plainVertex p2 )


{-| Construct a mesh from a list of line segments.

![Line segments mesh](https://ianmackenzie.github.io/elm-3d-scene/images/1.0.0/line-segments.png)

(Note that this particular case, where all the line segments meet end-to-end,
could be rendered more efficiently as a [polyline](#polyline)).

-}
lineSegments : List (LineSegment3d Meters coordinates) -> Plain coordinates
lineSegments givenSegments =
    case givenSegments of
        [] ->
            Types.EmptyMesh

        first :: rest ->
            let
                bounds =
                    BoundingBox3d.aggregateOf LineSegment3d.boundingBox first rest

                webGLMesh =
                    WebGL.lines (List.map lineSegmentAttributes givenSegments)
            in
            Types.LineSegments bounds givenSegments webGLMesh


{-| Construct a mesh from a single polyline.
-}
polyline : Polyline3d Meters coordinates -> Plain coordinates
polyline givenPolyline =
    let
        vertices =
            Polyline3d.vertices givenPolyline
    in
    case vertices of
        [] ->
            Types.EmptyMesh

        first :: rest ->
            let
                bounds =
                    BoundingBox3d.hull first rest

                webGLMesh =
                    WebGL.lineStrip (List.map plainVertex vertices)
            in
            Types.Polyline bounds givenPolyline webGLMesh


{-| Construct a mesh from a list of points that that will be displayed as
separate circular dots, given a particular dot radius in pixels.

![Points mesh](https://ianmackenzie.github.io/elm-3d-scene/images/1.0.0/points.png)

-}
points :
    { radius : Quantity Float Pixels }
    -> List (Point3d Meters coordinates)
    -> Plain coordinates
points { radius } givenPoints =
    case givenPoints of
        [] ->
            Types.EmptyMesh

        first :: rest ->
            let
                bounds =
                    BoundingBox3d.hull first rest

                webGLMesh =
                    WebGL.points (List.map plainVertex givenPoints)
            in
            Types.Points bounds (Pixels.toFloat radius) givenPoints webGLMesh


{-| Construct a `Shadow` value from a given mesh. This is an expensive
operation, so `Shadow` values (like `Mesh` values) should be stored in your
model (or as top-level constants) instead of constructed dynamically in your
`view` function.
-}
shadow : Mesh coordinates attributes -> Shadow coordinates
shadow mesh =
    case mesh of
        Types.EmptyMesh ->
            Types.EmptyShadow

        Types.Triangles boundingBox meshTriangles _ _ ->
            let
                vertexTriples =
                    List.map Triangle3d.vertices meshTriangles
            in
            shadowImpl boundingBox identity (TriangularMesh.triangles vertexTriples)

        Types.Facets boundingBox meshTriangles _ _ ->
            let
                vertexTriples =
                    List.map Triangle3d.vertices meshTriangles
            in
            shadowImpl boundingBox identity (TriangularMesh.triangles vertexTriples)

        Types.Indexed boundingBox triangularMesh _ _ ->
            shadowImpl boundingBox identity triangularMesh

        Types.MeshWithNormals boundingBox triangularMesh _ _ ->
            shadowImpl boundingBox .position triangularMesh

        Types.MeshWithUvs boundingBox triangularMesh _ _ ->
            shadowImpl boundingBox .position triangularMesh

        Types.MeshWithNormalsAndUvs boundingBox triangularMesh _ _ ->
            shadowImpl boundingBox .position triangularMesh

        Types.MeshWithTangents boundingBox triangularMesh _ _ ->
            shadowImpl boundingBox .position triangularMesh

        Types.LineSegments _ _ _ ->
            Types.EmptyShadow

        Types.Polyline _ _ _ ->
            Types.EmptyShadow

        Types.Points _ _ _ _ ->
            Types.EmptyShadow


shadowImpl : BoundingBox3d Meters coordinates -> (a -> Point3d Meters coordinates) -> TriangularMesh a -> Shadow coordinates
shadowImpl meshBounds getPosition triangularMesh =
    let
        meshFaceVertices =
            TriangularMesh.faceVertices triangularMesh

        initialShadowVertices =
            List.foldr (collectShadowVertices getPosition) [] meshFaceVertices

        ( initialShadowFaceIndices, neighborDict, nextShadowVertexIndex ) =
            visitFaces getPosition meshFaceVertices 0 [] Dict.empty

        ( allShadowFaceIndices, extraShadowVertices ) =
            ( initialShadowFaceIndices, [], nextShadowVertexIndex )
                |> joinEdges getPosition neighborDict meshFaceVertices 0

        allShadowVertices =
            if List.isEmpty extraShadowVertices then
                initialShadowVertices

            else
                initialShadowVertices ++ extraShadowVertices
    in
    Types.Shadow meshBounds
        (TriangularMesh.indexed (Array.fromList allShadowVertices) allShadowFaceIndices)
        (WebGL.indexedTriangles allShadowVertices allShadowFaceIndices)


collectShadowVertices :
    (meshVertex -> Point3d Meters coordinates)
    -> ( meshVertex, meshVertex, meshVertex )
    -> List VertexWithNormal
    -> List VertexWithNormal
collectShadowVertices getPosition ( mv1, mv2, mv3 ) accumulated =
    let
        p1 =
            getPosition mv1

        p2 =
            getPosition mv2

        p3 =
            getPosition mv3

        faceNormal =
            Vector3d.toVec3 (triangleNormal p1 p2 p3)

        sv1 =
            { position = Point3d.toVec3 p1, normal = faceNormal }

        sv2 =
            { position = Point3d.toVec3 p2, normal = faceNormal }

        sv3 =
            { position = Point3d.toVec3 p3, normal = faceNormal }
    in
    sv1 :: sv2 :: sv3 :: accumulated


type alias EdgeKey =
    ( ( Float, Float, Float ), ( Float, Float, Float ) )


edgeKey : Point3d Meters coordinates -> Point3d Meters coordinates -> EdgeKey
edgeKey firstPoint secondPoint =
    let
        p1 =
            Point3d.toMeters firstPoint

        p2 =
            Point3d.toMeters secondPoint
    in
    ( ( p1.x, p1.y, p1.z ), ( p2.x, p2.y, p2.z ) )


visitFaces :
    (meshVertex -> Point3d Meters coordinates)
    -> List ( meshVertex, meshVertex, meshVertex )
    -> Int
    -> List ( Int, Int, Int )
    -> Dict EdgeKey Int
    -> ( List ( Int, Int, Int ), Dict EdgeKey Int, Int )
visitFaces getPosition meshFaceVertices nextShadowVertexIndex shadowFaceIndices neighborDict =
    case meshFaceVertices of
        ( mv1, mv2, mv3 ) :: remainingMeshFaceVertices ->
            let
                p1 =
                    getPosition mv1

                p2 =
                    getPosition mv2

                p3 =
                    getPosition mv3

                a =
                    nextShadowVertexIndex

                b =
                    nextShadowVertexIndex + 1

                c =
                    nextShadowVertexIndex + 2

                updatedShadowFaceIndices =
                    ( a, b, c ) :: shadowFaceIndices

                updatedNeighborDict =
                    neighborDict
                        |> Dict.insert (edgeKey p2 p1) a
                        |> Dict.insert (edgeKey p3 p2) b
                        |> Dict.insert (edgeKey p1 p3) c
            in
            visitFaces
                getPosition
                remainingMeshFaceVertices
                (nextShadowVertexIndex + 3)
                updatedShadowFaceIndices
                updatedNeighborDict

        [] ->
            ( shadowFaceIndices, neighborDict, nextShadowVertexIndex )


joinEdges :
    (meshVertex -> Point3d Meters coordinates)
    -> Dict EdgeKey Int
    -> List ( meshVertex, meshVertex, meshVertex )
    -> Int
    -> ( List ( Int, Int, Int ), List VertexWithNormal, Int )
    -> ( List ( Int, Int, Int ), List VertexWithNormal )
joinEdges getPosition neighborDict meshFaceVertices nextShadowVertexIndex state =
    case meshFaceVertices of
        ( mv1, mv2, mv3 ) :: remainingMeshFaceVertices ->
            let
                p1 =
                    getPosition mv1

                p2 =
                    getPosition mv2

                p3 =
                    getPosition mv3

                a =
                    nextShadowVertexIndex

                b =
                    nextShadowVertexIndex + 1

                c =
                    nextShadowVertexIndex + 2
            in
            joinEdges
                getPosition
                neighborDict
                remainingMeshFaceVertices
                (nextShadowVertexIndex + 3)
                (state
                    |> joinEdge p1 p2 a b neighborDict
                    |> joinEdge p2 p3 b c neighborDict
                    |> joinEdge p3 p1 c a neighborDict
                )

        [] ->
            let
                ( shadowFaceIndices, extraShadowVertices, _ ) =
                    state
            in
            ( shadowFaceIndices, List.reverse extraShadowVertices )


zeroVec3 : Vec3
zeroVec3 =
    Math.Vector3.vec3 0 0 0


joinEdge :
    Point3d Meters coordinates
    -> Point3d Meters coordinates
    -> Int
    -> Int
    -> Dict EdgeKey Int
    -> ( List ( Int, Int, Int ), List VertexWithNormal, Int )
    -> ( List ( Int, Int, Int ), List VertexWithNormal, Int )
joinEdge p1 p2 start end neighborDict ( shadowFaceIndices, extraShadowVertices, nextShadowVertexIndex ) =
    case Dict.get (edgeKey p1 p2) neighborDict of
        Just opposite ->
            ( ( start, opposite, end ) :: shadowFaceIndices
            , extraShadowVertices
            , nextShadowVertexIndex
            )

        Nothing ->
            let
                v1 =
                    { position = Point3d.toVec3 p1, normal = zeroVec3 }

                v2 =
                    { position = Point3d.toVec3 p2, normal = zeroVec3 }

                a =
                    nextShadowVertexIndex

                b =
                    nextShadowVertexIndex + 1
            in
            ( ( start, a, b ) :: ( start, b, end ) :: shadowFaceIndices
            , v2 :: v1 :: extraShadowVertices
            , nextShadowVertexIndex + 2
            )


{-| [Back face culling](https://en.wikipedia.org/wiki/Back-face_culling) is a
rendering optimization that cuts down on the number of triangles drawn by not
drawing any triangles that are facing away from the viewer (camera). However,
this only really works if the mesh in question is a closed volume with all faces
having the correct (counterclockwise) [winding order](https://cmichel.io/understanding-front-faces-winding-order-and-normals).
For example, if render a simple curved surface which might get viewed from
either side, then you wouldn't want to enable back face culling because then the
surface would be invisible from one side! As a result, in `elm-3d-scene` back
face culling is disabled by default but you can enable it per-mesh as an
optimization where it is valid.

Note that `cullBackFaces` doesn't actually strip out any faces from the mesh,
since which faces are culled depends on the (dynamic) viewing direction - it
simply sets a flag on the mesh that indicates that back face culling should be
performed during rendering.

-}
cullBackFaces : Mesh coordinates attributes -> Mesh coordinates attributes
cullBackFaces mesh =
    case mesh of
        Types.EmptyMesh ->
            mesh

        Types.Triangles boundingBox meshTriangles webGLMesh _ ->
            Types.Triangles boundingBox meshTriangles webGLMesh CullBackFaces

        Types.Facets boundingBox meshTriangles webGLMesh _ ->
            Types.Facets boundingBox meshTriangles webGLMesh CullBackFaces

        Types.Indexed boundingBox triangularMesh webGLMesh _ ->
            Types.Indexed boundingBox triangularMesh webGLMesh CullBackFaces

        Types.MeshWithNormals boundingBox triangularMesh webGLMesh _ ->
            Types.MeshWithNormals boundingBox triangularMesh webGLMesh CullBackFaces

        Types.MeshWithUvs boundingBox triangularMesh webGLMesh _ ->
            Types.MeshWithUvs boundingBox triangularMesh webGLMesh CullBackFaces

        Types.MeshWithNormalsAndUvs boundingBox triangularMesh webGLMesh _ ->
            Types.MeshWithNormalsAndUvs boundingBox triangularMesh webGLMesh CullBackFaces

        Types.MeshWithTangents boundingBox triangularMesh webGLMesh _ ->
            Types.MeshWithTangents boundingBox triangularMesh webGLMesh CullBackFaces

        Types.LineSegments _ _ _ ->
            mesh

        Types.Polyline _ _ _ ->
            mesh

        Types.Points _ _ _ _ ->
            mesh
