module Scene3d.Types exposing
    ( Bounds
    , ColorType(..)
    , Drawable(..)
    , Drawable_(..)
    , Light(..)
    , Material(..)
    , Mesh(..)
    , PhysicalAttributes
    , SimpleAttributes
    , Transformation
    )

import BoundingBox3d exposing (BoundingBox3d)
import Color exposing (Color)
import Frame3d exposing (Frame3d)
import Math.Matrix4 exposing (Mat4)
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


type ColorType
    = FlatColor
    | EmissiveColor


type Material
    = SimpleMaterial ColorType Color
    | PhysicalMaterial
        { r : Float
        , g : Float
        , b : Float
        , rg : Float
        , mt : Float
        }


type alias Transformation =
    { ix : Float
    , iy : Float
    , iz : Float
    , jx : Float
    , jy : Float
    , jz : Float
    , kx : Float
    , ky : Float
    , kz : Float
    , px : Float
    , py : Float
    , pz : Float
    , scale : Float
    , isRightHanded : Bool
    }


type alias Bounds =
    { minX : Float
    , maxX : Float
    , minY : Float
    , maxY : Float
    , minZ : Float
    , maxZ : Float
    }


type Mesh
    = SimpleMesh ColorType Bounds (WebGL.Mesh SimpleAttributes)
    | PhysicalMesh Bounds (WebGL.Mesh PhysicalAttributes)


type Drawable_
    = MeshDrawable Mesh
    | EmptyDrawable
    | DrawableGroup (List Drawable_)
    | TransformedDrawable Transformation Drawable_


type Drawable units coordinates
    = Drawable Drawable_


type Light units coordinates
    = AmbientLight { color : Vec3, lookupTexture : Texture }
    | DirectionalLight { color : Vec3, direction : Vec3 }
    | PointLight { color : Vec3, position : Vec3 }
