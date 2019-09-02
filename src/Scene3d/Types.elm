module Scene3d.Types exposing
    ( AmbientLighting(..)
    , Bounds
    , Chromaticity(..)
    , Color(..)
    , DeformableVertex
    , DrawFunction
    , Drawable(..)
    , Light(..)
    , LightMatrices
    , Material(..)
    , Mesh(..)
    , MeshData(..)
    , Node(..)
    , PlainVertex
    , Shadow(..)
    , ShadowEdge
    , SmoothVertex
    , TexturableVertex
    , Transformation
    )

import BoundingBox3d exposing (BoundingBox3d)
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Frame3d exposing (Frame3d)
import Illuminance exposing (Lux)
import Length exposing (Meters)
import LineSegment3d exposing (LineSegment3d)
import LuminousFlux exposing (Lumens)
import Math.Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import Point3d exposing (Point3d)
import Polyline3d exposing (Polyline3d)
import Quantity exposing (Unitless)
import Triangle3d exposing (Triangle3d)
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)
import WebGL
import WebGL.Settings
import WebGL.Texture exposing (Texture)


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


type alias PlainVertex =
    { position : Vec3
    }


type alias SmoothVertex =
    { position : Vec3
    , normal : Vec3
    }


type alias TexturableVertex =
    { position : Vec3
    , normal : Vec3
    , uv : Vec2
    }


type alias DeformableVertex =
    { position : Vec3
    , normal : Vec3
    , uv : Vec2
    , tangent : Vec3
    }


type Color
    = ConstantColor Vec3
    | EmissiveColor Vec3


type Material primitives
    = ConstantMaterial Vec3
    | EmissiveMaterial Vec3
    | LambertianMaterial Vec3
    | PhysicalMaterial Vec3 Float Float


type Mesh coordinates primitives
    = EmptyMesh
    | Mesh (MeshData coordinates) (Maybe (Shadow coordinates))


type MeshData coordinates
    = Triangles (BoundingBox3d Meters coordinates) (List (Triangle3d Meters coordinates)) (WebGL.Mesh PlainVertex) Bool
    | Facets (BoundingBox3d Meters coordinates) (List (Triangle3d Meters coordinates)) (WebGL.Mesh SmoothVertex) Bool
    | Indexed (BoundingBox3d Meters coordinates) (TriangularMesh (Point3d Meters coordinates)) (WebGL.Mesh PlainVertex) Bool
    | Smooth (BoundingBox3d Meters coordinates) (TriangularMesh { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates }) (WebGL.Mesh SmoothVertex) Bool
    | LineSegments (BoundingBox3d Meters coordinates) (List (LineSegment3d Meters coordinates)) (WebGL.Mesh PlainVertex)
    | Polyline (BoundingBox3d Meters coordinates) (Polyline3d Meters coordinates) (WebGL.Mesh PlainVertex)
    | Points (BoundingBox3d Meters coordinates) (List (Point3d Meters coordinates)) (WebGL.Mesh PlainVertex)


type alias ShadowEdge coordinates =
    { startPoint : Point3d Meters coordinates
    , endPoint : Point3d Meters coordinates
    , leftNormal : Vector3d Unitless coordinates
    , rightNormal : Vector3d Unitless coordinates
    }


type Shadow coordinates
    = EmptyShadow
    | Shadow (List (ShadowEdge coordinates)) (WebGL.Mesh SmoothVertex)


type alias LightMatrices =
    { lights12 : Mat4
    , lights34 : Mat4
    , lights56 : Mat4
    , lights78 : Mat4
    }


type alias DrawFunction =
    Mat4 -- scene properties
    -> Float -- model scale
    -> Mat4 -- model matrix
    -> Bool -- model matrix is right-handed
    -> Mat4 -- view matrix
    -> Mat4 -- ambient lighting
    -> LightMatrices -- lights
    -> List WebGL.Settings.Setting -- stencil, depth, blend etc.
    -> WebGL.Entity


type Node
    = EmptyNode
    | MeshNode DrawFunction
    | ShadowNode DrawFunction
    | Group (List Node)
    | Transformed Transformation Node


type Drawable coordinates
    = Drawable Node


type Chromaticity
    = Chromaticity { x : Float, y : Float }


type Light coordinates
    = Light
        { type_ : Float
        , radius : Float
        , x : Float
        , y : Float
        , z : Float
        , r : Float
        , g : Float
        , b : Float
        }


type AmbientLighting coordinates
    = AmbientLighting Mat4
