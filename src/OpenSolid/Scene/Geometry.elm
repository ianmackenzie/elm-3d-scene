module OpenSolid.Scene.Geometry
    exposing
        ( Geometry
        , faces
        , facets
        , indexedFaces
        , shaded
        )

import OpenSolid.BoundingBox3d as BoundingBox3d
import OpenSolid.Frame3d as Frame3d
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Scene.Material exposing (Material)
import OpenSolid.Scene.Node exposing (Node)
import OpenSolid.Scene.Types as Types
import OpenSolid.Triangle3d as Triangle3d
import OpenSolid.WebGL.Direction3d as Direction3d
import OpenSolid.WebGL.Point3d as Point3d
import OpenSolid.WebGL.Triangle3d as Triangle3d
import WebGL


type alias Geometry =
    Types.Geometry


shaded : Material -> Geometry -> Node
shaded material geometry =
    Types.LeafNode (Types.ShadedGeometry material geometry)


facets : List Triangle3d -> Geometry
facets triangles =
    let
        boundingBox =
            BoundingBox3d.hullOf (List.map Triangle3d.boundingBox triangles)

        mesh =
            WebGL.triangles
                (List.map Triangle3d.vertexPositionsAndNormals triangles)
    in
    Types.Geometry boundingBox mesh


type alias Vertex =
    ( Point3d, Direction3d )


type alias FaceVertices =
    ( Vertex, Vertex, Vertex )


faceBoundingBox : FaceVertices -> BoundingBox3d
faceBoundingBox ( ( p1, _ ), ( p2, _ ), ( p3, _ ) ) =
    Triangle3d.boundingBox (Triangle3d ( p1, p2, p3 ))


faceAttributes : FaceVertices -> ( Types.VertexAttributes, Types.VertexAttributes, Types.VertexAttributes )
faceAttributes ( ( p1, n1 ), ( p2, n2 ), ( p3, n3 ) ) =
    ( { vertexPosition = Point3d.toVec3 p1
      , vertexNormal = Direction3d.toVec3 n1
      }
    , { vertexPosition = Point3d.toVec3 p2
      , vertexNormal = Direction3d.toVec3 n2
      }
    , { vertexPosition = Point3d.toVec3 p3
      , vertexNormal = Direction3d.toVec3 n3
      }
    )


faces : List ( ( Point3d, Direction3d ), ( Point3d, Direction3d ), ( Point3d, Direction3d ) ) -> Geometry
faces vertexTriples =
    let
        boundingBox =
            BoundingBox3d.hullOf (List.map faceBoundingBox vertexTriples)

        mesh =
            WebGL.triangles (List.map faceAttributes vertexTriples)
    in
    Types.Geometry boundingBox mesh


vertexAttributes : Vertex -> Types.VertexAttributes
vertexAttributes ( point, normalDirection ) =
    { vertexPosition = Point3d.toVec3 point
    , vertexNormal = Direction3d.toVec3 normalDirection
    }


indexedFaces : List ( Point3d, Direction3d ) -> List ( Int, Int, Int ) -> Geometry
indexedFaces vertices faces =
    let
        vertexPoints =
            List.map Tuple.first vertices

        boundingBox =
            BoundingBox3d.containing vertexPoints

        attributes =
            List.map vertexAttributes vertices

        mesh =
            WebGL.indexedTriangles attributes faces
    in
    Types.Geometry boundingBox mesh
