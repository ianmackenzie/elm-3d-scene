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
import OpenSolid.BoundingBox3d as BoundingBox3d
import OpenSolid.Frame3d as Frame3d
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.LineSegment3d as LineSegment3d
import OpenSolid.Polyline3d as Polyline3d
import OpenSolid.Scene.Node exposing (Node)
import OpenSolid.Scene.Types as Types
import OpenSolid.Triangle3d as Triangle3d
import OpenSolid.WebGL.LineSegment3d as LineSegment3d
import OpenSolid.WebGL.Point3d as Point3d
import OpenSolid.WebGL.Polyline3d as Polyline3d
import OpenSolid.WebGL.Triangle3d as Triangle3d
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
                mesh =
                    WebGL.triangles
                        (List.map Triangle3d.vertexPositions triangles_)
            in
            Types.SimpleGeometry boundingBox mesh

        Nothing ->
            Types.EmptySimpleGeometry


indexedTriangles : List Point3d -> List ( Int, Int, Int ) -> SimpleGeometry
indexedTriangles vertices faces =
    case BoundingBox3d.containing vertices of
        Just boundingBox ->
            let
                vertexPositions =
                    List.map Point3d.toVertexPosition vertices

                mesh =
                    WebGL.indexedTriangles vertexPositions faces
            in
            Types.SimpleGeometry boundingBox mesh

        Nothing ->
            Types.EmptySimpleGeometry


triangleFan : List Point3d -> SimpleGeometry
triangleFan points =
    case BoundingBox3d.containing points of
        Just boundingBox ->
            let
                mesh =
                    WebGL.triangleFan (List.map Point3d.toVertexPosition points)
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
                mesh =
                    WebGL.lines
                        (List.map LineSegment3d.vertexPositions lineSegments)
            in
            Types.SimpleGeometry boundingBox mesh

        Nothing ->
            Types.EmptySimpleGeometry


polyline : Polyline3d -> SimpleGeometry
polyline polyline_ =
    case Polyline3d.boundingBox polyline_ of
        Just boundingBox ->
            let
                mesh =
                    WebGL.lineStrip (Polyline3d.vertexPositions polyline_)
            in
            Types.SimpleGeometry boundingBox mesh

        Nothing ->
            Types.EmptySimpleGeometry


points : List Point3d -> SimpleGeometry
points points_ =
    case BoundingBox3d.containing points_ of
        Just boundingBox ->
            let
                mesh =
                    WebGL.points (List.map Point3d.toVertexPosition points_)
            in
            Types.SimpleGeometry boundingBox mesh

        Nothing ->
            Types.EmptySimpleGeometry
