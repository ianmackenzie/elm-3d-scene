module Scene3d.Types exposing
    ( ColorType(..)
    , Drawable(..)
    , Light(..)
    , Material(..)
    , Mesh(..)
    , PhysicalAttributes
    , Placement(..)
    , SimpleAttributes
    )

import BoundingBox3d exposing (BoundingBox3d)
import Color exposing (Color)
import Frame3d exposing (Frame3d)
import Math.Vector3 exposing (Vec3)
import WebGL
import WebGL.Texture exposing (Texture)


type alias SimpleAttributes =
    { x : Float
    , y : Float
    , z : Float
    , r : Float
    , g : Float
    , b : Float
    }


type alias PhysicalAttributes =
    { x : Float
    , y : Float
    , z : Float
    , nx : Float
    , ny : Float
    , nz : Float
    , r : Float
    , g : Float
    , b : Float
    , rg : Float
    , mt : Float
    }


type Material
    = SimpleMaterial ColorType Color
    | PhysicalMaterial
        { r : Float
        , g : Float
        , b : Float
        , rg : Float
        , mt : Float
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
    = AmbientLight { color : Vec3, lookupTexture : Texture }
    | DirectionalLight { color : Vec3, direction : Vec3 }
    | PointLight { color : Vec3, position : Vec3 }
