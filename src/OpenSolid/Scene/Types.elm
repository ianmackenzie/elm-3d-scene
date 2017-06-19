module OpenSolid.Scene.Types exposing (..)

import Color exposing (Color)
import Math.Vector3 exposing (Vec3)
import OpenSolid.Geometry.Types exposing (..)
import WebGL


type Geometry a
    = Geometry (Maybe BoundingBox3d) (WebGL.Mesh a)


type Light
    = DirectionalLight Color Direction3d


type Lighting
    = SingleLight Light


type Material
    = PhysicallyBasedMaterial
        { baseColor : Color
        , roughness : Float
        , metallic : Bool
        }


type Drawable
    = ColoredGeometry Color (Geometry { vertexPosition : Vec3 })
    | ShadedGeometry Material Lighting (Geometry { vertexPosition : Vec3, vertexNormal : Vec3 })


type Node
    = LeafNode Frame3d Drawable
    | GroupNode Frame3d (List Node)
