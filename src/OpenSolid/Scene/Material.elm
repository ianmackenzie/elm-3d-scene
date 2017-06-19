module OpenSolid.Scene.Material
    exposing
        ( Material
        , physicallyBased
        )

import Color exposing (Color)
import OpenSolid.Scene.Types as Types


type alias Material =
    Types.Material


physicallyBased : { baseColor : Color, roughness : Float, metallic : Bool } -> Material
physicallyBased =
    Types.PhysicallyBasedMaterial
