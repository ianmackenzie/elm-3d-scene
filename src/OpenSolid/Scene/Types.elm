module OpenSolid.Scene.Types exposing (..)

import Math.Vector3 exposing (Vec3)
import OpenSolid.BoundingBox3d as BoundingBox3d exposing (BoundingBox3d)
import OpenSolid.Frame3d as Frame3d exposing (Frame3d)
import WebGL exposing (Texture)


type alias SimpleAttributes =
    { position : Vec3
    , color : Vec3
    }


type alias PhysicalAttributes =
    { position : Vec3
    , normal : Vec3
    , baseColor : Vec3
    , roughness : Float
    , metallic : Float
    }


type Material
    = SimpleMaterial ColorType Vec3
    | PhysicalMaterial
        { baseColor : Vec3
        , roughness : Float
        , metallic : Float
        }


type ColorType
    = FlatColor
    | EmissiveColor


type Placement
    = Placement
        { frame : Frame3d
        , scale : Float
        , isRightHanded : Bool
        }


type Mesh
    = SimpleMesh ColorType BoundingBox3d (WebGL.Mesh SimpleAttributes)
    | PhysicalMesh BoundingBox3d (WebGL.Mesh PhysicalAttributes)


type Drawable
    = MeshDrawable Mesh
    | EmptyDrawable
    | DrawableGroup (List Drawable)
    | TransformedDrawable Placement Drawable


type Light
    = AmbientLight { color : Vec3, lookupTexture : WebGL.Texture }
    | DirectionalLight { color : Vec3, direction : Vec3 }
    | PointLight { color : Vec3, position : Vec3 }
