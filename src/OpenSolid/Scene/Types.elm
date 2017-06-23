module OpenSolid.Scene.Types exposing (..)

import Math.Vector3 exposing (Vec3)
import OpenSolid.Geometry.Types exposing (..)
import WebGL exposing (Texture)


type alias SimpleVertexAttributes =
    { vertexPosition : Vec3 }


type SimpleGeometry
    = SimpleGeometry (Maybe BoundingBox3d) (WebGL.Mesh SimpleVertexAttributes)


type alias VertexAttributes =
    { vertexPosition : Vec3, vertexNormal : Vec3 }


type Geometry
    = Geometry (Maybe BoundingBox3d) (WebGL.Mesh VertexAttributes)


type Light
    = AmbientLight { color : Vec3, lookupTexture : WebGL.Texture }
    | DirectionalLight { color : Vec3, direction : Vec3 }
    | PointLight { color : Vec3, position : Vec3 }


type Material
    = PhysicallyBasedMaterial
        { baseColor : Vec3
        , roughness : Float
        , metallic : Float
        }


type Drawable
    = ColoredGeometry Vec3 SimpleGeometry
    | ShadedGeometry Material Geometry


type Node
    = LeafNode Drawable
    | GroupNode (List Node)
    | TransformedNode Frame3d Node
