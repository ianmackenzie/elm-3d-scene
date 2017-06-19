module OpenSolid.Scene.Geometry
    exposing
        ( Geometry
        , indexedTriangles
        , triangles
        )

import Math.Vector3 exposing (Vec3)
import OpenSolid.BoundingBox3d as BoundingBox3d
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Scene.Types as Types
import OpenSolid.Triangle3d as Triangle3d
import OpenSolid.WebGL.Direction3d as Direction3d
import OpenSolid.WebGL.Point3d as Point3d
import OpenSolid.WebGL.Triangle3d as Triangle3d
import WebGL


type alias Geometry =
    Types.Geometry { vertexPosition : Vec3, vertexNormal : Vec3 }


triangles : List Triangle3d -> Geometry
triangles triangles_ =
    let
        boundingBox =
            BoundingBox3d.hullOf (List.map Triangle3d.boundingBox triangles_)

        mesh =
            WebGL.triangles
                (List.map Triangle3d.vertexPositionsAndNormals triangles_)
    in
    Types.Geometry boundingBox mesh


indexedTriangles : List ( Point3d, Direction3d ) -> List ( Int, Int, Int ) -> Geometry
indexedTriangles vertices faces =
    let
        vertexPoints =
            List.map Tuple.first vertices

        boundingBox =
            BoundingBox3d.containing vertexPoints

        toAttributes ( point, normalDirection ) =
            { vertexPosition = Point3d.toVec3 point
            , vertexNormal = Direction3d.toVec3 normalDirection
            }

        vertexAttributes =
            List.map toAttributes vertices

        mesh =
            WebGL.indexedTriangles vertexAttributes faces
    in
    Types.Geometry boundingBox mesh
