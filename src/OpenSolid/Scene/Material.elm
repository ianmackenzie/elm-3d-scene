module OpenSolid.Scene.Material
    exposing
        ( Material
        , emissive
        , metal
        , nonmetal
        )

import Color exposing (Color)
import Math.Vector3 exposing (Vec3)
import OpenSolid.Scene.Color as Color
import OpenSolid.Scene.Types as Types


type alias Material =
    Types.Material


flat : Color -> Material
flat color =
    Types.SimpleMaterial Types.FlatColor (Color.toVec3 color)


metal : { color : Vec3, roughness : Float } -> Material
metal { color, roughness } =
    Types.PhysicalMaterial
        { baseColor = color
        , roughness = roughness
        , metallic = 1.0
        }


nonmetal : { color : Vec3, roughness : Float } -> Material
nonmetal { color, roughness } =
    Types.PhysicalMaterial
        { baseColor = color
        , roughness = roughness
        , metallic = 0.0
        }


emissive : Vec3 -> Material
emissive =
    Types.SimpleMaterial Types.EmissiveColor
