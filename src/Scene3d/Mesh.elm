module Scene3d.Mesh exposing
    ( Mesh
    , HasNormals, NoNormals, HasTangents, NoTangents, HasUV, NoUV
    , BackFaceSetting, cullBackFaces, renderBackFaces
    , empty
    , triangles, facets
    , indexed, smooth
    , lineSegments, polyline
    , points
    , withShadow
    )

{-|

@docs Mesh, Shadow

@docs HasNormals, NoNormals, HasTangents, NoTangents, HasUV, NoUV


## Back face settings

@docs BackFaceSetting, cullBackFaces, renderBackFaces


## Empty

@docs empty


## Triangles

@docs triangles, facets


## Indexed triangles

@docs indexed, smooth


## Lines

@docs lineSegments, polyline


## Points

@docs points


## Shadows

@docs withShadow

-}

import Array
import BoundingBox3d exposing (BoundingBox3d)
import Dict exposing (Dict)
import Geometry.Interop.LinearAlgebra.Point3d as Point3d
import Geometry.Interop.LinearAlgebra.Vector3d as Vector3d
import LineSegment3d exposing (LineSegment3d)
import Math.Vector3 exposing (Vec3)
import Point3d exposing (Point3d)
import Polyline3d exposing (Polyline3d)
import Quantity exposing (Quantity(..), Unitless)
import Scene3d.Types as Types exposing (Bounds, MeshData, PlainVertex, Shadow, ShadowEdge, SmoothVertex)
import Triangle3d exposing (Triangle3d)
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)
import WebGL


type alias Mesh units coordinates normals uv tangents =
    Types.Mesh units coordinates normals uv tangents


type HasNormals
    = HasNormals


type NoNormals
    = NoNormals


type HasUV
    = HasUV


type NoUV
    = NoUV


type HasTangents
    = HasTangents


type NoTangents
    = NoTangents


type BackFaceSetting
    = CullBackFaces Bool


cullBackFaces : BackFaceSetting
cullBackFaces =
    CullBackFaces True


renderBackFaces : BackFaceSetting
renderBackFaces =
    CullBackFaces False


plainVertex : Point3d units coordinates -> PlainVertex
plainVertex point =
    { position = Point3d.toVec3 point }


triangleAttributes : Triangle3d units coordinates -> ( PlainVertex, PlainVertex, PlainVertex )
triangleAttributes triangle =
    let
        ( p1, p2, p3 ) =
            Triangle3d.vertices triangle
    in
    ( plainVertex p1, plainVertex p2, plainVertex p3 )


facetAttributes : Triangle3d units coordinates -> ( SmoothVertex, SmoothVertex, SmoothVertex )
facetAttributes triangle =
    let
        ( p1, p2, p3 ) =
            Triangle3d.vertices triangle

        e1 =
            Vector3d.from p1 p2

        e2 =
            Vector3d.from p2 p3

        normal =
            Vector3d.toVec3 (e1 |> Vector3d.cross e2)
    in
    ( { position = Point3d.toVec3 p1, normal = normal }
    , { position = Point3d.toVec3 p2, normal = normal }
    , { position = Point3d.toVec3 p3, normal = normal }
    )


empty : Mesh units coordinates normals uv tangents
empty =
    Types.EmptyMesh


withoutShadow : MeshData units coordinates normals uv tangents -> Mesh units coordinates normals uv tangents
withoutShadow meshData =
    Types.Mesh meshData Nothing


triangles : BackFaceSetting -> List (Triangle3d units coordinates) -> Mesh units coordinates NoNormals NoUV NoTangents
triangles (CullBackFaces cullBack) givenTriangles =
    case givenTriangles of
        [] ->
            Types.EmptyMesh

        first :: rest ->
            let
                bounds =
                    BoundingBox3d.hullOf Triangle3d.boundingBox first rest

                webGLMesh =
                    WebGL.triangles (List.map triangleAttributes givenTriangles)
            in
            withoutShadow <|
                Types.Triangles bounds givenTriangles webGLMesh cullBack


facets : BackFaceSetting -> List (Triangle3d units coordinates) -> Mesh units coordinates HasNormals NoUV NoTangents
facets (CullBackFaces cullBack) givenTriangles =
    case givenTriangles of
        [] ->
            Types.EmptyMesh

        first :: rest ->
            let
                bounds =
                    BoundingBox3d.hullOf Triangle3d.boundingBox first rest

                webGLMesh =
                    WebGL.triangles (List.map facetAttributes givenTriangles)
            in
            withoutShadow <|
                Types.Facets bounds givenTriangles webGLMesh cullBack


collectPlain : Point3d units coordinates -> List PlainVertex -> List PlainVertex
collectPlain point accumulated =
    { position = Point3d.toVec3 point } :: accumulated


plainBoundsHelp : Float -> Float -> Float -> Float -> Float -> Float -> List PlainVertex -> BoundingBox3d units coordinates
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


plainBounds : PlainVertex -> List PlainVertex -> BoundingBox3d units coordinates
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


indexed : BackFaceSetting -> TriangularMesh (Point3d units coordinates) -> Mesh units coordinates NoNormals NoUV NoTangents
indexed (CullBackFaces cullBack) givenMesh =
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
            withoutShadow <|
                Types.Indexed bounds givenMesh webGLMesh cullBack


collectSmooth : { position : Point3d units coordinates, normal : Vector3d Unitless coordinates } -> List SmoothVertex -> List SmoothVertex
collectSmooth { position, normal } accumulated =
    { position = Point3d.toVec3 position, normal = Vector3d.toVec3 normal }
        :: accumulated


smoothBoundsHelp : Float -> Float -> Float -> Float -> Float -> Float -> List SmoothVertex -> BoundingBox3d units coordinates
smoothBoundsHelp minX maxX minY maxY minZ maxZ remaining =
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
            smoothBoundsHelp
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


smoothBounds : SmoothVertex -> List SmoothVertex -> BoundingBox3d units coordinates
smoothBounds first rest =
    let
        x =
            Math.Vector3.getX first.position

        y =
            Math.Vector3.getY first.position

        z =
            Math.Vector3.getZ first.position
    in
    smoothBoundsHelp x x y y z z rest


smooth : BackFaceSetting -> TriangularMesh { position : Point3d units coordinates, normal : Vector3d Unitless coordinates } -> Mesh units coordinates HasNormals NoUV NoTangents
smooth (CullBackFaces cullBack) givenMesh =
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
                    smoothBounds first rest

                webGLMesh =
                    WebGL.indexedTriangles
                        collectedVertices
                        (TriangularMesh.faceIndices givenMesh)
            in
            withoutShadow <|
                Types.Smooth bounds givenMesh webGLMesh cullBack


lineSegmentAttributes : LineSegment3d units coordinates -> ( PlainVertex, PlainVertex )
lineSegmentAttributes givenSegment =
    let
        ( p1, p2 ) =
            LineSegment3d.endpoints givenSegment
    in
    ( plainVertex p1, plainVertex p2 )


lineSegments : List (LineSegment3d units coordinates) -> Mesh units coordinates NoNormals NoUV NoTangents
lineSegments givenSegments =
    case givenSegments of
        [] ->
            Types.EmptyMesh

        first :: rest ->
            let
                bounds =
                    BoundingBox3d.hullOf LineSegment3d.boundingBox first rest

                webGLMesh =
                    WebGL.lines (List.map lineSegmentAttributes givenSegments)
            in
            withoutShadow <|
                Types.LineSegments bounds givenSegments webGLMesh


polyline : Polyline3d units coordinates -> Mesh units coordinates NoNormals NoUV NoTangents
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
                    Point3d.hull first rest

                webGLMesh =
                    WebGL.lineStrip (List.map plainVertex vertices)
            in
            withoutShadow <|
                Types.Polyline bounds givenPolyline webGLMesh


points : List (Point3d units coordinates) -> Mesh units coordinates NoNormals NoUV NoTangents
points givenPoints =
    case givenPoints of
        [] ->
            Types.EmptyMesh

        first :: rest ->
            let
                bounds =
                    Point3d.hull first rest

                webGLMesh =
                    WebGL.points (List.map plainVertex givenPoints)
            in
            withoutShadow <|
                Types.Points bounds givenPoints webGLMesh


withShadow : Mesh units coordinates normals uv tangents -> Mesh units coordinates normals uv tangents
withShadow mesh =
    case mesh of
        Types.Mesh meshData Nothing ->
            Types.Mesh meshData (Just (createShadow meshData))

        Types.Mesh meshData (Just shadow) ->
            mesh

        Types.EmptyMesh ->
            mesh


createShadow : MeshData units coordinates normals uv tangents -> Shadow units coordinates
createShadow meshData =
    case meshData of
        Types.Triangles boundingBox meshTriangles _ _ ->
            let
                vertexTriples =
                    List.map Triangle3d.vertices meshTriangles
            in
            shadowImpl boundingBox (TriangularMesh.triangles vertexTriples)

        Types.Facets boundingBox meshTriangles _ _ ->
            let
                vertexTriples =
                    List.map Triangle3d.vertices meshTriangles
            in
            shadowImpl boundingBox (TriangularMesh.triangles vertexTriples)

        Types.Indexed boundingBox triangularMesh _ _ ->
            shadowImpl boundingBox triangularMesh

        Types.Smooth boundingBox triangularMesh _ _ ->
            shadowImpl boundingBox
                (TriangularMesh.mapVertices .position triangularMesh)

        Types.LineSegments _ _ _ ->
            Types.EmptyShadow

        Types.Polyline _ _ _ ->
            Types.EmptyShadow

        Types.Points _ _ _ ->
            Types.EmptyShadow


shadowImpl : BoundingBox3d units coordinates -> TriangularMesh (Point3d units coordinates) -> Shadow units coordinates
shadowImpl boundingBox triangularMesh =
    let
        numVertices =
            Array.length (TriangularMesh.vertices triangularMesh)

        faceIndices =
            TriangularMesh.faceIndices triangularMesh

        faceVertices =
            TriangularMesh.faceVertices triangularMesh

        shadowEdges =
            buildShadowEdges numVertices faceIndices faceVertices Dict.empty

        shadowVolumeFaces =
            List.foldl collectShadowFaces [] shadowEdges
    in
    Types.Shadow shadowEdges (WebGL.triangles shadowVolumeFaces)


buildShadowEdges : Int -> List ( Int, Int, Int ) -> List ( Point3d units coordinates, Point3d units coordinates, Point3d units coordinates ) -> Dict Int (ShadowEdge units coordinates) -> List (ShadowEdge units coordinates)
buildShadowEdges numVertices faceIndices faceVertices edgeDictionary =
    case faceIndices of
        ( i, j, k ) :: remainingFaceIndices ->
            case faceVertices of
                ( p1, p2, p3 ) :: remainingFaceVertices ->
                    let
                        normal =
                            Vector3d.from p1 p2
                                |> Vector3d.cross (Vector3d.from p1 p3)
                                |> Vector3d.normalize

                        updatedEdgeDictionary =
                            if normal == Vector3d.zero then
                                -- Skip degenerate faces
                                edgeDictionary

                            else
                                edgeDictionary
                                    |> Dict.update (edgeKey numVertices i j)
                                        (updateShadowEdge i j p1 p2 normal)
                                    |> Dict.update (edgeKey numVertices j k)
                                        (updateShadowEdge j k p2 p3 normal)
                                    |> Dict.update (edgeKey numVertices k i)
                                        (updateShadowEdge k i p3 p1 normal)
                    in
                    buildShadowEdges numVertices
                        remainingFaceIndices
                        remainingFaceVertices
                        updatedEdgeDictionary

                [] ->
                    -- Should never happen, faceIndices and faceVertices should
                    -- always be the same length
                    []

        [] ->
            Dict.values edgeDictionary


collectShadowFaces : ShadowEdge units coordinates -> List ( SmoothVertex, SmoothVertex, SmoothVertex ) -> List ( SmoothVertex, SmoothVertex, SmoothVertex )
collectShadowFaces { startPoint, endPoint, leftNormal, rightNormal } accumulated =
    let
        firstFace =
            ( { position = Point3d.toVec3 startPoint, normal = Vector3d.toVec3 rightNormal }
            , { position = Point3d.toVec3 endPoint, normal = Vector3d.toVec3 rightNormal }
            , { position = Point3d.toVec3 endPoint, normal = Vector3d.toVec3 leftNormal }
            )

        secondFace =
            ( { position = Point3d.toVec3 endPoint, normal = Vector3d.toVec3 leftNormal }
            , { position = Point3d.toVec3 startPoint, normal = Vector3d.toVec3 leftNormal }
            , { position = Point3d.toVec3 startPoint, normal = Vector3d.toVec3 rightNormal }
            )
    in
    firstFace :: secondFace :: accumulated


edgeKey : Int -> Int -> Int -> Int
edgeKey numVertices i j =
    if i < j then
        i * numVertices + j

    else
        j * numVertices + i


updateShadowEdge : Int -> Int -> Point3d units coordinates -> Point3d units coordinates -> Vector3d Unitless coordinates -> Maybe (ShadowEdge units coordinates) -> Maybe (ShadowEdge units coordinates)
updateShadowEdge i j pi pj normalVector currentEntry =
    case currentEntry of
        Nothing ->
            if i < j then
                Just
                    { startPoint = pi
                    , endPoint = pj
                    , leftNormal = normalVector
                    , rightNormal = Vector3d.zero
                    }

            else
                Just
                    { startPoint = pj
                    , endPoint = pi
                    , leftNormal = Vector3d.zero
                    , rightNormal = normalVector
                    }

        Just currentEdge ->
            if i < j then
                if currentEdge.leftNormal == Vector3d.zero then
                    if currentEdge.rightNormal == Vector3d.zero then
                        -- Degenerate edge, leave as is
                        currentEntry

                    else
                        -- Add left normal to edge
                        Just
                            { startPoint = currentEdge.startPoint
                            , endPoint = currentEdge.endPoint
                            , leftNormal = normalVector
                            , rightNormal = currentEdge.rightNormal
                            }

                else
                    -- Encountered a degenerate edge, , mark it as degenerate
                    Just
                        { startPoint = currentEdge.startPoint
                        , endPoint = currentEdge.endPoint
                        , leftNormal = Vector3d.zero
                        , rightNormal = Vector3d.zero
                        }

            else if currentEdge.rightNormal == Vector3d.zero then
                if currentEdge.leftNormal == Vector3d.zero then
                    -- Degenerate edge, leave as is
                    currentEntry

                else
                    -- Add right normal to edge
                    Just
                        { startPoint = currentEdge.startPoint
                        , endPoint = currentEdge.endPoint
                        , leftNormal = currentEdge.leftNormal
                        , rightNormal = normalVector
                        }

            else
                -- Found a degenerate edge, mark it as degenerate
                Just
                    { startPoint = currentEdge.startPoint
                    , endPoint = currentEdge.endPoint
                    , leftNormal = Vector3d.zero
                    , rightNormal = Vector3d.zero
                    }
