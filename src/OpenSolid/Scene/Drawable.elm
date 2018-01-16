module OpenSolid.Scene.Drawable
    exposing
        ( Drawable
        , body
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

import Array.Hamt as Array exposing (Array)
import Color exposing (Color)
import OpenSolid.Axis3d as Axis3d exposing (Axis3d)
import OpenSolid.Body3d as Body3d exposing (Body3d)
import OpenSolid.BoundingBox3d as BoundingBox3d exposing (BoundingBox3d)
import OpenSolid.Direction3d as Direction3d exposing (Direction3d)
import OpenSolid.Frame3d as Frame3d exposing (Frame3d)
import OpenSolid.LineSegment3d as LineSegment3d exposing (LineSegment3d)
import OpenSolid.Mesh as Mesh exposing (Mesh)
import OpenSolid.Plane3d as Plane3d exposing (Plane3d)
import OpenSolid.Point3d as Point3d exposing (Point3d)
import OpenSolid.Polyline3d as Polyline3d exposing (Polyline3d)
import OpenSolid.Scene.Material as Material exposing (Material)
import OpenSolid.Scene.Placement as Placement exposing (Placement)
import OpenSolid.Scene.Types as Types
import OpenSolid.Surface3d as Surface3d exposing (Surface3d)
import OpenSolid.Triangle3d as Triangle3d exposing (Triangle3d)
import OpenSolid.Vector3d as Vector3d exposing (Vector3d)
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

                mesh =
                    WebGL.lines (List.map toAttributes lineSegments_)
            in
            Types.MeshDrawable <|
                Types.SimpleMesh Types.FlatColor overallBoundingBox mesh

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

                mesh =
                    WebGL.points <| List.map toAttributes points_
            in
            Types.MeshDrawable <|
                Types.SimpleMesh Types.FlatColor boundingBox mesh

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
            Color.toRgb color

        ( r, g, b ) =
            ( toFloat red / 255
            , toFloat green / 255
            , toFloat blue / 255
            )
    in
    \point ->
        let
            ( x, y, z ) =
                Point3d.coordinates point
        in
        { x = x
        , y = y
        , z = z
        , r = r
        , g = g
        , b = b
        }


triangleFan : Color -> List Point3d -> Drawable
triangleFan color points =
    case BoundingBox3d.containingPoints points of
        Just boundingBox ->
            let
                mesh =
                    WebGL.triangleFan <|
                        List.map (simpleAttributes color) points
            in
            Types.MeshDrawable <|
                Types.SimpleMesh Types.FlatColor boundingBox mesh

        Nothing ->
            Types.EmptyDrawable


triangles : Material -> List Triangle3d -> Drawable
triangles material triangles =
    case BoundingBox3d.aggregate (List.map Triangle3d.boundingBox triangles) of
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
                            WebGL.triangles (List.map toAttributes triangles)
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
                            WebGL.triangles (List.map toAttributes triangles)
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

                        mesh =
                            WebGL.triangles <|
                                List.map faceAttributes vertexTriples
                    in
                    Types.MeshDrawable <|
                        Types.SimpleMesh colorType overallBoundingBox mesh

                Types.PhysicalMaterial properties ->
                    let
                        faceAttributes ( ( p1, n1 ), ( p2, n2 ), ( p3, n3 ) ) =
                            ( physicalAttributes properties p1 n1
                            , physicalAttributes properties p2 n2
                            , physicalAttributes properties p3 n3
                            )

                        mesh =
                            WebGL.triangles <|
                                List.map faceAttributes vertexTriples
                    in
                    Types.MeshDrawable <|
                        Types.PhysicalMesh overallBoundingBox mesh

        Nothing ->
            Types.EmptyDrawable


indexedFaces : Material -> List ( Point3d, Vector3d ) -> List ( Int, Int, Int ) -> Drawable
indexedFaces material vertices faces =
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
                                faces
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
                                faces
                    in
                    Types.MeshDrawable <|
                        Types.PhysicalMesh boundingBox webGLMesh

        Nothing ->
            Types.EmptyDrawable


mesh : Material -> Mesh ( Point3d, Vector3d ) -> Drawable
mesh material mesh_ =
    indexedFaces material
        (Array.toList (Mesh.vertices mesh_))
        (Mesh.faceIndices mesh_)


surface : Float -> Material -> Surface3d -> Drawable
surface tolerance material surface_ =
    mesh material (Surface3d.toMesh tolerance surface_)


body : Float -> Material -> Body3d -> Drawable
body tolerance material body_ =
    Body3d.surfaces body_
        |> List.map (Surface3d.toMesh tolerance)
        |> Mesh.combine
        |> mesh material


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
transformBy placementTransformation drawable =
    case drawable of
        Types.TransformedDrawable placement drawable ->
            Types.TransformedDrawable (placementTransformation placement)
                drawable

        Types.EmptyDrawable ->
            Types.EmptyDrawable

        Types.MeshDrawable _ ->
            Types.TransformedDrawable
                (placementTransformation Placement.identity)
                drawable

        Types.DrawableGroup _ ->
            Types.TransformedDrawable
                (placementTransformation Placement.identity)
                drawable


rotateAround : Axis3d -> Float -> Drawable -> Drawable
rotateAround axis angle drawable =
    transformBy (Placement.rotateAround axis angle) drawable


translateBy : Vector3d -> Drawable -> Drawable
translateBy displacement drawable =
    transformBy (Placement.translateBy displacement) drawable


mirrorAcross : Plane3d -> Drawable -> Drawable
mirrorAcross plane drawable =
    transformBy (Placement.mirrorAcross plane) drawable


relativeTo : Frame3d -> Drawable -> Drawable
relativeTo frame drawable =
    transformBy (Placement.relativeTo frame) drawable


placeIn : Frame3d -> Drawable -> Drawable
placeIn frame drawable =
    transformBy (Placement.placeIn frame) drawable


scaleAbout : Point3d -> Float -> Drawable -> Drawable
scaleAbout point scale drawable =
    transformBy (Placement.scaleAbout point scale) drawable
