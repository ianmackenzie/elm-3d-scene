module OpenSolid.SceneGraph.Material
    exposing
        ( Material
        , physicallyBased
        )

import OpenSolid.SceneGraph.Types as Types


type alias Material =
    Types.Material


physicallyBased : { baseColor : Color, roughness : Float, metallic : Float } -> Material
physicallyBased =
    Types.PhysicallyBasedMaterial
