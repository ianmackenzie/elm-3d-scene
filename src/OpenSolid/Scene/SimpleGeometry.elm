module OpenSolid.Scene.SimpleGeometry
    exposing
        ( SimpleGeometry
        , indexedTriangles
        , lines
        , points
        , polyline
        , triangleFan
        , triangles
        )

import Math.Vector3 exposing (Vec3)
import OpenSolid.BoundingBox3d as BoundingBox3d
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.LineSegment3d as LineSegment3d
import OpenSolid.Polyline3d as Polyline3d
import OpenSolid.Scene.Types as Types
import OpenSolid.Triangle3d as Triangle3d
import OpenSolid.WebGL.LineSegment3d as LineSegment3d
import OpenSolid.WebGL.Point3d as Point3d
import OpenSolid.WebGL.Polyline3d as Polyline3d
import OpenSolid.WebGL.Triangle3d as Triangle3d
import WebGL


type alias SimpleGeometry =
    Types.Geometry { vertexPosition : Vec3 }


triangles : List Triangle3d -> SimpleGeometry
triangles triangles_ =
    let
        boundingBox =
            BoundingBox3d.hullOf (List.map Triangle3d.boundingBox triangles_)

        mesh =
            WebGL.triangles (List.map Triangle3d.vertexPositions triangles_)
    in
    Types.Geometry boundingBox mesh


indexedTriangles : List Point3d -> List ( Int, Int, Int ) -> SimpleGeometry
indexedTriangles vertices faces =
    let
        boundingBox =
            BoundingBox3d.containing vertices

        vertexPositions =
            List.map Point3d.toVertexPosition vertices

        mesh =
            WebGL.indexedTriangles vertexPositions faces
    in
    Types.Geometry boundingBox mesh


triangleFan : List Point3d -> SimpleGeometry
triangleFan points =
    let
        boundingBox =
            BoundingBox3d.containing points

        mesh =
            WebGL.triangleFan (List.map Point3d.toVertexPosition points)
    in
    Types.Geometry boundingBox mesh


lines : List LineSegment3d -> SimpleGeometry
lines lineSegments =
    let
        segmentBoundingBoxes =
            List.map LineSegment3d.boundingBox lineSegments

        boundingBox =
            BoundingBox3d.hullOf segmentBoundingBoxes

        mesh =
            WebGL.lines (List.map LineSegment3d.vertexPositions lineSegments)
    in
    Types.Geometry boundingBox mesh


polyline : Polyline3d -> SimpleGeometry
polyline polyline_ =
    let
        boundingBox =
            Polyline3d.boundingBox polyline_

        mesh =
            WebGL.lineStrip (Polyline3d.vertexPositions polyline_)
    in
    Types.Geometry boundingBox mesh


points : List Point3d -> SimpleGeometry
points points_ =
    let
        boundingBox =
            BoundingBox3d.containing points_

        mesh =
            WebGL.points (List.map Point3d.toVertexPosition points_)
    in
    Types.Geometry boundingBox mesh
