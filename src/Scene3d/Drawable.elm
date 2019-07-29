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

import Angle exposing (Angle)
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
import Quantity exposing (Quantity(..), Unitless)
import Scene3d.Material as Material exposing (Material)
import Scene3d.Transformation as Transformation exposing (Transformation)
import Scene3d.Types as Types exposing (Bounds, Drawable_)
import Triangle3d exposing (Triangle3d)
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)
import WebGL


type alias Drawable units coordinates =
    Types.Drawable units coordinates


empty : Drawable units coordinates
empty =
    Types.Drawable Types.EmptyDrawable


toBounds : BoundingBox3d units coordinates -> Bounds
toBounds boundingBox =
    let
        { minX, maxX, minY, maxY, minZ, maxZ } =
            BoundingBox3d.extrema boundingBox

        (Quantity fMinX) =
            minX

        (Quantity fMaxX) =
            maxX

        (Quantity fMinY) =
            minY

        (Quantity fMaxY) =
            maxY

        (Quantity fMinZ) =
            minZ

        (Quantity fMaxZ) =
            maxZ
    in
    { minX = fMinX
    , maxX = fMaxX
    , minY = fMinY
    , maxY = fMaxY
    , minZ = fMinZ
    , maxZ = fMaxZ
    }


lineSegments : Color -> List (LineSegment3d units coordinates) -> Drawable units coordinates
lineSegments color givenSegments =
    Types.Drawable <|
        let
            segmentBoundingBoxes =
                List.map LineSegment3d.boundingBox givenSegments
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
                        WebGL.lines (List.map toAttributes givenSegments)
                in
                Types.MeshDrawable <|
                    Types.SimpleMesh Types.FlatColor
                        (toBounds overallBoundingBox)
                        lineMesh

            Nothing ->
                Types.EmptyDrawable


polyline : Color -> Polyline3d units coordinates -> Drawable units coordinates
polyline color givenPolyline =
    Types.Drawable <|
        case Polyline3d.boundingBox givenPolyline of
            Just boundingBox ->
                let
                    toAttributes =
                        simpleAttributes color

                    vertexAttributes =
                        Polyline3d.vertices givenPolyline
                            |> List.map toAttributes

                    webGLMesh =
                        WebGL.lineStrip vertexAttributes
                in
                Types.MeshDrawable <|
                    Types.SimpleMesh Types.FlatColor
                        (toBounds boundingBox)
                        webGLMesh

            Nothing ->
                Types.EmptyDrawable


points : Color -> List (Point3d units coordinates) -> Drawable units coordinates
points color givenPoints =
    Types.Drawable <|
        case BoundingBox3d.containingPoints givenPoints of
            Just boundingBox ->
                let
                    toAttributes =
                        simpleAttributes color

                    pointsMesh =
                        WebGL.points <| List.map toAttributes givenPoints
                in
                Types.MeshDrawable <|
                    Types.SimpleMesh Types.FlatColor
                        (toBounds boundingBox)
                        pointsMesh

            Nothing ->
                Types.EmptyDrawable


physicalAttributes : { r : Float, g : Float, b : Float, rg : Float, mt : Float } -> Point3d units coordinates -> Vector3d Unitless coordinates -> Types.PhysicalAttributes
physicalAttributes { r, g, b, rg, mt } point normal =
    let
        p =
            Point3d.unwrap point

        n =
            Vector3d.unwrap normal
    in
    { x = p.x
    , y = p.y
    , z = p.z
    , nx = n.x
    , ny = n.y
    , nz = n.z
    , r = r
    , g = g
    , b = b
    , rg = rg
    , mt = mt
    }


simpleAttributes : Color -> Point3d units coordinates -> Types.SimpleAttributes
simpleAttributes color =
    let
        { red, green, blue } =
            Color.toRgba color
    in
    \point ->
        let
            { x, y, z } =
                Point3d.unwrap point
        in
        { x = x
        , y = y
        , z = z
        , r = red
        , g = green
        , b = blue
        }


triangleFan : Color -> List (Point3d units coordinates) -> Drawable units coordinates
triangleFan color givenPoints =
    Types.Drawable <|
        case BoundingBox3d.containingPoints givenPoints of
            Just boundingBox ->
                let
                    fanMesh =
                        WebGL.triangleFan <|
                            List.map (simpleAttributes color) givenPoints
                in
                Types.MeshDrawable <|
                    Types.SimpleMesh Types.FlatColor (toBounds boundingBox) fanMesh

            Nothing ->
                Types.EmptyDrawable


triangles : Material -> List (Triangle3d units coordinates) -> Drawable units coordinates
triangles material givenTriangles =
    Types.Drawable <|
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
                            Types.SimpleMesh colorType
                                (toBounds overallBoundingBox)
                                webGLMesh

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
                            Types.PhysicalMesh (toBounds overallBoundingBox)
                                webGLMesh

            Nothing ->
                Types.EmptyDrawable


type alias Vertex units coordinates =
    ( Point3d units coordinates, Vector3d Unitless coordinates )


type alias FaceVertices units coordinates =
    ( Vertex units coordinates, Vertex units coordinates, Vertex units coordinates )


faceBoundingBox : FaceVertices units coordinates -> BoundingBox3d units coordinates
faceBoundingBox ( ( p1, _ ), ( p2, _ ), ( p3, _ ) ) =
    Triangle3d.boundingBox (Triangle3d.fromVertices p1 p2 p3)


faces : Material -> List ( ( Point3d units coordinates, Vector3d Unitless coordinates ), ( Point3d units coordinates, Vector3d Unitless coordinates ), ( Point3d units coordinates, Vector3d Unitless coordinates ) ) -> Drawable units coordinates
faces material vertexTriples =
    Types.Drawable <|
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
                            Types.SimpleMesh colorType
                                (toBounds overallBoundingBox)
                                facesMesh

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
                            Types.PhysicalMesh (toBounds overallBoundingBox)
                                facesMesh

            Nothing ->
                Types.EmptyDrawable


indexedFaces : Material -> List ( Point3d units coordinates, Vector3d Unitless coordinates ) -> List ( Int, Int, Int ) -> Drawable units coordinates
indexedFaces material vertices givenFaces =
    Types.Drawable <|
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
                            Types.SimpleMesh colorType (toBounds boundingBox) webGLMesh

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
                            Types.PhysicalMesh (toBounds boundingBox) webGLMesh

            Nothing ->
                Types.EmptyDrawable


mesh : Material -> TriangularMesh ( Point3d units coordinates, Vector3d Unitless coordinates ) -> Drawable units coordinates
mesh material mesh_ =
    indexedFaces material
        (Array.toList (TriangularMesh.vertices mesh_))
        (TriangularMesh.faceIndices mesh_)


collectNonempty : List (Drawable units coordinates) -> List Drawable_ -> List Drawable_
collectNonempty drawables accumulated =
    case drawables of
        [] ->
            accumulated

        (Types.Drawable Types.EmptyDrawable) :: rest ->
            collectNonempty rest accumulated

        (Types.Drawable drawable_) :: rest ->
            collectNonempty rest (drawable_ :: accumulated)


group : List (Drawable units coordinates) -> Drawable units coordinates
group drawables =
    Types.Drawable <|
        case collectNonempty drawables [] of
            [] ->
                Types.EmptyDrawable

            [ singleDrawable ] ->
                singleDrawable

            nonEmptyDrawables ->
                Types.DrawableGroup nonEmptyDrawables


transformBy : Transformation -> Drawable units1 coordinates1 -> Drawable units2 coordinates2
transformBy transformation (Types.Drawable drawable_) =
    Types.Drawable <|
        case drawable_ of
            Types.TransformedDrawable existingTransformation transformedDrawable ->
                Types.TransformedDrawable
                    (Transformation.compose existingTransformation transformation)
                    transformedDrawable

            Types.EmptyDrawable ->
                Types.EmptyDrawable

            Types.MeshDrawable _ ->
                Types.TransformedDrawable transformation drawable_

            Types.DrawableGroup _ ->
                Types.TransformedDrawable transformation drawable_


rotateAround : Axis3d units coordinates -> Angle -> Drawable units coordinates -> Drawable units coordinates
rotateAround axis angle givenDrawable =
    transformBy (Transformation.rotateAround axis angle) givenDrawable


translateBy : Vector3d units coordinates -> Drawable units coordinates -> Drawable units coordinates
translateBy displacement givenDrawable =
    transformBy (Transformation.translateBy displacement) givenDrawable


mirrorAcross : Plane3d units coordinates -> Drawable units coordinates -> Drawable units coordinates
mirrorAcross plane givenDrawable =
    transformBy (Transformation.mirrorAcross plane) givenDrawable


relativeTo : Frame3d units globalCoordinates { defines : localCoordinates } -> Drawable units globalCoordinates -> Drawable units localCoordinates
relativeTo frame givenDrawable =
    transformBy (Transformation.relativeTo frame) givenDrawable


placeIn : Frame3d units globalCoordinates { defines : localCoordinates } -> Drawable units localCoordinates -> Drawable units globalCoordinates
placeIn frame givenDrawable =
    transformBy (Transformation.placeIn frame) givenDrawable


scaleAbout : Point3d units coordinates -> Float -> Drawable units coordinates -> Drawable units coordinates
scaleAbout point scale givenDrawable =
    transformBy (Transformation.scaleAbout point scale) givenDrawable
