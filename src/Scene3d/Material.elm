module Scene3d.Material exposing
    ( Material
    , solidColor, diffuse, emissive
    , metal, nonmetal, hybrid
    -- , solidColorTexture
    -- , emissiveTexture
    -- , diffuseTexture
    -- , texturedMetal
    -- , texturedNonmetal
    -- , texturedHybrid
    -- , withNormalMap
    )

{-|

@docs Material


## Simple materials

@docs solidColor, diffuse, emissive


## Physically-based materials

@docs metal, nonmetal, hybrid

-}

import Color exposing (Color)
import Luminance exposing (Luminance)
import Scene3d.Chromaticity exposing (Chromaticity)
import Scene3d.Mesh exposing (Yes)


type Material properties
    = SolidColor Color
    | Emissive Luminance Chromaticity
    | Diffuse Color
    | Pbr { baseColor : Color, roughness : Float, metallic : Float }



-- | SolidColorTexture Texture
-- | EmissiveTexture Luminance Texture
-- | DiffuseTexture Texture
-- | PbrTextures { baseColor : Texture, roughness : Texture, metallic : Float }


solidColor : Color -> Material properties
solidColor =
    SolidColor


emissive : Luminance -> Chromaticity -> Material properties
emissive =
    Emissive


diffuse : Color -> Material { a | hasNormals : Yes }
diffuse =
    Diffuse


metal : { baseColor : Color, roughness : Float } -> Material { a | hasNormals : Yes }
metal { baseColor, roughness } =
    Pbr { baseColor = baseColor, roughness = roughness, metallic = 1 }


nonmetal : { baseColor : Color, roughness : Float } -> Material { a | hasNormals : Yes }
nonmetal { baseColor, roughness } =
    Pbr { baseColor = baseColor, roughness = roughness, metallic = 0 }


hybrid : { baseColor : Color, roughness : Float, metallic : Float } -> Material { a | hasNormals : Yes }
hybrid arguments =
    Pbr arguments



-- solidColorTexture : Texture -> Material { a | hasUv : Yes }
-- emissiveTexture : Luminance -> Texture -> Material { a | hasUv : Yes }
-- diffuseTexture : Texture -> Material { a | hasNormals : Yes, hasUv : Yes }
-- texturedMetal : { baseColor : Texture, roughness : Texture } -> Material { a | hasNormals : Yes, hasUv : Yes }
-- texturedNonmetal : { baseColor : Texture, roughness : Texture } -> Material { a | hasNormals : Yes, hasUv : Yes }
-- texturedHybrid : { baseColor : Texture, roughness : Texture, metallic : Float } -> Material { a | hasNormals : Yes, hasUv : Yes }
-- {-| Add a normal map to an existing material
-- -}
-- withNormalMap :
--     Texture
--     -> Material { a | hasUv : Yes, hasNormals : Yes, hasTangents : Yes }
--     -> Material { a | hasUv : Yes, hasNormals : Yes, hasTangents : Yes }
