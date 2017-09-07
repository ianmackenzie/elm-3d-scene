module OpenSolid.Scene.SimpleGeometry
    exposing
        ( SimpleGeometry
        , colored
        , empty
        , indexedTriangles
        , lines
        , points
        , polyline
        , triangleFan
        , triangles
        )

import Math.Vector3 exposing (Vec3)
import OpenSolid.BoundingBox3d as BoundingBox3d exposing (BoundingBox3d)
import OpenSolid.Interop.LinearAlgebra.Point3d as Point3d
import OpenSolid.LineSegment3d as LineSegment3d exposing (LineSegment3d)
import OpenSolid.Point3d as Point3d exposing (Point3d)
import OpenSolid.Polyline3d as Polyline3d exposing (Polyline3d)
import OpenSolid.Scene.Node exposing (Node)
import OpenSolid.Scene.Types as Types
import OpenSolid.Triangle3d as Triangle3d exposing (Triangle3d)
import WebGL


type alias SimpleGeometry =
    Types.SimpleGeometry


colored : Vec3 -> SimpleGeometry -> Node
colored color geometry =
    case geometry of
        Types.SimpleGeometry boundingBox mesh ->
            Types.LeafNode (Types.ColoredGeometry color boundingBox mesh)

        Types.EmptySimpleGeometry ->
            Types.EmptyNode


empty : SimpleGeometry
empty =
    Types.EmptySimpleGeometry


triangles : List Triangle3d -> SimpleGeometry
triangles triangles_ =
    case BoundingBox3d.hullOf (List.map Triangle3d.boundingBox triangles_) of
        Just boundingBox ->
            let
                toAttributes triangle =
                    let
                        ( p1, p2, p3 ) =
                            Triangle3d.vertices triangle
                    in
                    ( { position = Point3d.toVec3 p1 }
                    , { position = Point3d.toVec3 p2 }
                    , { position = Point3d.toVec3 p3 }
                    )

                mesh =
                    WebGL.triangles (List.map toAttributes triangles_)
            in
            Types.SimpleGeometry boundingBox mesh

        Nothing ->
            Types.EmptySimpleGeometry


indexedTriangles : List Point3d -> List ( Int, Int, Int ) -> SimpleGeometry
indexedTriangles vertices faces =
    case Point3d.hullOf vertices of
        Just boundingBox ->
            let
                toAttribute point =
                    { position = Point3d.toVec3 point }

                vertexAttributes =
                    List.map toAttribute vertices

                mesh =
                    WebGL.indexedTriangles vertexAttributes faces
            in
            Types.SimpleGeometry boundingBox mesh

        Nothing ->
            Types.EmptySimpleGeometry


triangleFan : List Point3d -> SimpleGeometry
triangleFan points =
    case Point3d.hullOf points of
        Just boundingBox ->
            let
                toAttribute point =
                    { position = Point3d.toVec3 point }

                mesh =
                    WebGL.triangleFan (List.map toAttribute points)
            in
            Types.SimpleGeometry boundingBox mesh

        Nothing ->
            Types.EmptySimpleGeometry


lines : List LineSegment3d -> SimpleGeometry
lines lineSegments =
    let
        segmentBoundingBoxes =
            List.map LineSegment3d.boundingBox lineSegments
    in
    case BoundingBox3d.hullOf segmentBoundingBoxes of
        Just boundingBox ->
            let
                toAttributes lineSegment =
                    let
                        ( p1, p2 ) =
                            LineSegment3d.endpoints lineSegment
                    in
                    ( { position = Point3d.toVec3 p1 }
                    , { position = Point3d.toVec3 p2 }
                    )

                mesh =
                    WebGL.lines (List.map toAttributes lineSegments)
            in
            Types.SimpleGeometry boundingBox mesh

        Nothing ->
            Types.EmptySimpleGeometry


polyline : Polyline3d -> SimpleGeometry
polyline polyline_ =
    case Polyline3d.boundingBox polyline_ of
        Just boundingBox ->
            let
                toAttribute point =
                    { position = Point3d.toVec3 point }

                vertexAttributes =
                    Polyline3d.vertices polyline_
                        |> List.map toAttribute

                mesh =
                    WebGL.lineStrip vertexAttributes
            in
            Types.SimpleGeometry boundingBox mesh

        Nothing ->
            Types.EmptySimpleGeometry


points : List Point3d -> SimpleGeometry
points points_ =
    case Point3d.hullOf points_ of
        Just boundingBox ->
            let
                toAttribute point =
                    { position = Point3d.toVec3 point }

                mesh =
                    WebGL.points (List.map toAttribute points_)
            in
            Types.SimpleGeometry boundingBox mesh

        Nothing ->
            Types.EmptySimpleGeometry
