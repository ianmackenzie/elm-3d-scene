module Scene3d.Drawable exposing
    ( Drawable
    , empty
    , faces
    , group
    , indexedFaces
    , lineSegments
    , mesh
    , mirrorAcross
    , placeIn
    , points
    , polyline
    , relativeTo
    , rotateAround
    , scaleAbout
    , translateBy
    , triangleFan
    , triangles
    )

import Array
import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import Color exposing (Color)
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import LineSegment3d exposing (LineSegment3d)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Polyline3d exposing (Polyline3d)
import Scene3d.Material as Material exposing (Material)
import Scene3d.Placement as Placement exposing (Placement)
import Scene3d.Types as Types
import Triangle3d exposing (Triangle3d)
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)
import WebGL


type alias Drawable =
    Types.Drawable


empty : Drawable
empty =
    Types.EmptyDrawable


lineSegments : Color -> List LineSegment3d -> Drawable
lineSegments color lineSegments_ =
    let
        segmentBoundingBoxes =
            List.map LineSegment3d.boundingBox lineSegments_
    in
    case BoundingBox3d.aggregate segmentBoundingBoxes of
        Just overallBoundingBox ->
            let
                vertexAttributes =
                    simpleAttributes color

                toAttributes lineSegment =
                    let
                        ( p1, p2 ) =
                            LineSegment3d.endpoints lineSegment
                    in
                    ( vertexAttributes p1, vertexAttributes p2 )

                lineMesh =
                    WebGL.lines (List.map toAttributes lineSegments_)
            in
            Types.MeshDrawable <|
                Types.SimpleMesh Types.FlatColor overallBoundingBox lineMesh

        Nothing ->
            Types.EmptyDrawable


polyline : Color -> Polyline3d -> Drawable
polyline color polyline_ =
    case Polyline3d.boundingBox polyline_ of
        Just boundingBox ->
            let
                toAttributes =
                    simpleAttributes color

                vertexAttributes =
                    Polyline3d.vertices polyline_
                        |> List.map toAttributes

                webGLMesh =
                    WebGL.lineStrip vertexAttributes
            in
            Types.MeshDrawable <|
                Types.SimpleMesh Types.FlatColor boundingBox webGLMesh

        Nothing ->
            Types.EmptyDrawable


points : Color -> List Point3d -> Drawable
points color points_ =
    case BoundingBox3d.containingPoints points_ of
        Just boundingBox ->
            let
                toAttributes =
                    simpleAttributes color

                pointsMesh =
                    WebGL.points <| List.map toAttributes points_
            in
            Types.MeshDrawable <|
                Types.SimpleMesh Types.FlatColor boundingBox pointsMesh

        Nothing ->
            Types.EmptyDrawable


physicalAttributes : { r : Float, g : Float, b : Float, rg : Float, mt : Float } -> Point3d -> Vector3d -> Types.PhysicalAttributes
physicalAttributes { r, g, b, rg, mt } point normal =
    let
        ( x, y, z ) =
            Point3d.coordinates point

        ( nx, ny, nz ) =
            Vector3d.components normal
    in
    { x = x
    , y = y
    , z = z
    , nx = nx
    , ny = ny
    , nz = nz
    , r = r
    , g = g
    , b = b
    , rg = rg
    , mt = mt
    }


simpleAttributes : Color -> Point3d -> Types.SimpleAttributes
simpleAttributes color =
    let
        { red, green, blue } =
            Color.toRgba color
    in
    \point ->
        let
            ( x, y, z ) =
                Point3d.coordinates point
        in
        { x = x
        , y = y
        , z = z
        , r = red
        , g = green
        , b = blue
        }


triangleFan : Color -> List Point3d -> Drawable
triangleFan color givenPoints =
    case BoundingBox3d.containingPoints givenPoints of
        Just boundingBox ->
            let
                fanMesh =
                    WebGL.triangleFan <|
                        List.map (simpleAttributes color) givenPoints
            in
            Types.MeshDrawable <|
                Types.SimpleMesh Types.FlatColor boundingBox fanMesh

        Nothing ->
            Types.EmptyDrawable


triangles : Material -> List Triangle3d -> Drawable
triangles material givenTriangles =
    case BoundingBox3d.aggregate (List.map Triangle3d.boundingBox givenTriangles) of
        Just overallBoundingBox ->
            case material of
                Types.SimpleMaterial colorType color ->
                    let
                        vertexAttributes =
                            simpleAttributes color

                        toAttributes triangle =
                            let
                                ( p1, p2, p3 ) =
                                    Triangle3d.vertices triangle
                            in
                            ( vertexAttributes p1
                            , vertexAttributes p2
                            , vertexAttributes p3
                            )

                        webGLMesh =
                            WebGL.triangles (List.map toAttributes givenTriangles)
                    in
                    Types.MeshDrawable <|
                        Types.SimpleMesh colorType overallBoundingBox webGLMesh

                Types.PhysicalMaterial properties ->
                    let
                        toAttributes triangle =
                            let
                                ( p1, p2, p3 ) =
                                    Triangle3d.vertices triangle

                                normal =
                                    case Triangle3d.normalDirection triangle of
                                        Just direction ->
                                            Direction3d.toVector direction

                                        Nothing ->
                                            Vector3d.zero
                            in
                            ( physicalAttributes properties p1 normal
                            , physicalAttributes properties p2 normal
                            , physicalAttributes properties p3 normal
                            )

                        webGLMesh =
                            WebGL.triangles (List.map toAttributes givenTriangles)
                    in
                    Types.MeshDrawable <|
                        Types.PhysicalMesh overallBoundingBox webGLMesh

        Nothing ->
            Types.EmptyDrawable


type alias Vertex =
    ( Point3d, Vector3d )


type alias FaceVertices =
    ( Vertex, Vertex, Vertex )


faceBoundingBox : FaceVertices -> BoundingBox3d
faceBoundingBox ( ( p1, _ ), ( p2, _ ), ( p3, _ ) ) =
    Triangle3d.boundingBox (Triangle3d.fromVertices ( p1, p2, p3 ))


faces : Material -> List ( ( Point3d, Vector3d ), ( Point3d, Vector3d ), ( Point3d, Vector3d ) ) -> Drawable
faces material vertexTriples =
    case BoundingBox3d.aggregate (List.map faceBoundingBox vertexTriples) of
        Just overallBoundingBox ->
            case material of
                Types.SimpleMaterial colorType color ->
                    let
                        faceAttributes ( ( p1, _ ), ( p2, _ ), ( p3, _ ) ) =
                            ( simpleAttributes color p1
                            , simpleAttributes color p2
                            , simpleAttributes color p3
                            )

                        facesMesh =
                            WebGL.triangles <|
                                List.map faceAttributes vertexTriples
                    in
                    Types.MeshDrawable <|
                        Types.SimpleMesh colorType overallBoundingBox facesMesh

                Types.PhysicalMaterial properties ->
                    let
                        faceAttributes ( ( p1, n1 ), ( p2, n2 ), ( p3, n3 ) ) =
                            ( physicalAttributes properties p1 n1
                            , physicalAttributes properties p2 n2
                            , physicalAttributes properties p3 n3
                            )

                        facesMesh =
                            WebGL.triangles <|
                                List.map faceAttributes vertexTriples
                    in
                    Types.MeshDrawable <|
                        Types.PhysicalMesh overallBoundingBox facesMesh

        Nothing ->
            Types.EmptyDrawable


indexedFaces : Material -> List ( Point3d, Vector3d ) -> List ( Int, Int, Int ) -> Drawable
indexedFaces material vertices givenFaces =
    let
        vertexPoints =
            List.map Tuple.first vertices
    in
    case BoundingBox3d.containingPoints vertexPoints of
        Just boundingBox ->
            case material of
                Types.SimpleMaterial colorType color ->
                    let
                        webGLMesh =
                            WebGL.indexedTriangles
                                (List.map (simpleAttributes color) vertexPoints)
                                givenFaces
                    in
                    Types.MeshDrawable <|
                        Types.SimpleMesh colorType boundingBox webGLMesh

                Types.PhysicalMaterial properties ->
                    let
                        toAttributes ( point, normalDirection ) =
                            physicalAttributes properties point normalDirection

                        webGLMesh =
                            WebGL.indexedTriangles
                                (List.map toAttributes vertices)
                                givenFaces
                    in
                    Types.MeshDrawable <|
                        Types.PhysicalMesh boundingBox webGLMesh

        Nothing ->
            Types.EmptyDrawable


mesh : Material -> TriangularMesh ( Point3d, Vector3d ) -> Drawable
mesh material mesh_ =
    indexedFaces material
        (Array.toList (TriangularMesh.vertices mesh_))
        (TriangularMesh.faceIndices mesh_)


isNotEmpty : Drawable -> Bool
isNotEmpty drawable =
    drawable /= Types.EmptyDrawable


group : List Drawable -> Drawable
group drawables =
    case List.filter isNotEmpty drawables of
        [] ->
            Types.EmptyDrawable

        nonEmptyDrawables ->
            Types.DrawableGroup nonEmptyDrawables


transformBy : (Placement -> Placement) -> Drawable -> Drawable
transformBy placementTransformation givenDrawable =
    case givenDrawable of
        Types.TransformedDrawable placement transformedDrawable ->
            Types.TransformedDrawable (placementTransformation placement)
                transformedDrawable

        Types.EmptyDrawable ->
            Types.EmptyDrawable

        Types.MeshDrawable _ ->
            Types.TransformedDrawable
                (placementTransformation Placement.identity)
                givenDrawable

        Types.DrawableGroup _ ->
            Types.TransformedDrawable
                (placementTransformation Placement.identity)
                givenDrawable


rotateAround : Axis3d -> Float -> Drawable -> Drawable
rotateAround axis angle givenDrawable =
    transformBy (Placement.rotateAround axis angle) givenDrawable


translateBy : Vector3d -> Drawable -> Drawable
translateBy displacement givenDrawable =
    transformBy (Placement.translateBy displacement) givenDrawable


mirrorAcross : Plane3d -> Drawable -> Drawable
mirrorAcross plane givenDrawable =
    transformBy (Placement.mirrorAcross plane) givenDrawable


relativeTo : Frame3d -> Drawable -> Drawable
relativeTo frame givenDrawable =
    transformBy (Placement.relativeTo frame) givenDrawable


placeIn : Frame3d -> Drawable -> Drawable
placeIn frame givenDrawable =
    transformBy (Placement.placeIn frame) givenDrawable


scaleAbout : Point3d -> Float -> Drawable -> Drawable
scaleAbout point scale givenDrawable =
    transformBy (Placement.scaleAbout point scale) givenDrawable
