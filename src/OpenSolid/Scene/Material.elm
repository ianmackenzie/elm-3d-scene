module OpenSolid.Scene.Material
    exposing
        ( Material
        , metal
        , nonmetal
        )

import Color exposing (Color)
import OpenSolid.Scene.Types as Types
import OpenSolid.WebGL.Color as Color


type alias Material =
    Types.Material


metal : { color : Color, roughness : Float } -> Material
metal { color, roughness } =
    Types.PhysicallyBasedMaterial
        { baseColor = Color.toVec3 color
        , roughness = roughness
        , metallic = 1.0
        }


nonmetal : { color : Color, roughness : Float } -> Material
nonmetal { color, roughness } =
    Types.PhysicallyBasedMaterial
        { baseColor = Color.toVec3 color
        , roughness = roughness
        , metallic = 0.0
        }
