module OpenSolid.Scene.Types exposing (..)

import Math.Vector3 exposing (Vec3)
import OpenSolid.BoundingBox3d as BoundingBox3d exposing (BoundingBox3d)
import OpenSolid.Frame3d as Frame3d exposing (Frame3d)
import WebGL exposing (Texture)


type alias SimpleVertexAttributes =
    { position : Vec3 }


type SimpleGeometry
    = SimpleGeometry BoundingBox3d (WebGL.Mesh SimpleVertexAttributes)
    | EmptySimpleGeometry


type alias VertexAttributes =
    { position : Vec3, normal : Vec3 }


type Geometry
    = Geometry BoundingBox3d (WebGL.Mesh VertexAttributes)
    | EmptyGeometry


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
    | EmissiveMaterial Vec3


type Drawable
    = ColoredGeometry Vec3 BoundingBox3d (WebGL.Mesh SimpleVertexAttributes)
    | ShadedGeometry Material BoundingBox3d (WebGL.Mesh VertexAttributes)


type Placement
    = Placement
        { frame : Frame3d
        , scale : Float
        , isRightHanded : Bool
        }


type Node
    = EmptyNode
    | LeafNode Drawable
    | GroupNode (List Node)
    | TransformedNode Placement Node
