module OpenSolid.Scene.Material
    exposing
        ( Material
        , emissive
        , metal
        , nonmetal
        )

import Math.Vector3 exposing (Vec3)
import OpenSolid.Scene.Types as Types


type alias Material =
    Types.Material


metal : { color : Vec3, roughness : Float } -> Material
metal { color, roughness } =
    Types.PhysicallyBasedMaterial
        { baseColor = color
        , roughness = roughness
        , metallic = 1.0
        }


nonmetal : { color : Vec3, roughness : Float } -> Material
nonmetal { color, roughness } =
    Types.PhysicallyBasedMaterial
        { baseColor = color
        , roughness = roughness
        , metallic = 0.0
        }


emissive : Vec3 -> Material
emissive =
    Types.EmissiveMaterial
