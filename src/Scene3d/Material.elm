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
import Math.Vector3 exposing (Vec3)
import Scene3d.Chromaticity exposing (Chromaticity)
import Scene3d.ColorConversions as ColorConversions
import Scene3d.Mesh exposing (Yes)
import Scene3d.Types as Types exposing (LinearRgb(..))


type alias Material properties =
    Types.Material properties


solidColor : Color -> Material properties
solidColor givenColor =
    let
        ( red, green, blue ) =
            Color.toRGB givenColor
    in
    Types.ConstantMaterial <|
        Math.Vector3.vec3 (red / 255) (green / 255) (blue / 255)


emissive : Luminance -> Chromaticity -> Material properties
emissive luminance chromaticity =
    let
        (LinearRgb chromaticityRgb) =
            ColorConversions.chromaticityToLinearRgb chromaticity

        nits =
            Luminance.inNits luminance
    in
    Types.EmissiveMaterial (LinearRgb (Math.Vector3.scale nits chromaticityRgb))


diffuse : Color -> Material { a | hasNormals : Yes }
diffuse givenColor =
    Types.LambertianMaterial (ColorConversions.colorToLinearRgb givenColor)


metal : { baseColor : Color, roughness : Float } -> Material { a | hasNormals : Yes }
metal { baseColor, roughness } =
    hybrid { baseColor = baseColor, roughness = roughness, metallic = 1 }


nonmetal : { baseColor : Color, roughness : Float } -> Material { a | hasNormals : Yes }
nonmetal { baseColor, roughness } =
    hybrid { baseColor = baseColor, roughness = roughness, metallic = 0 }


hybrid : { baseColor : Color, roughness : Float, metallic : Float } -> Material { a | hasNormals : Yes }
hybrid { baseColor, roughness, metallic } =
    Types.PbrMaterial
        (ColorConversions.colorToLinearRgb baseColor)
        (clamp 0 1 roughness)
        (clamp 0 1 metallic)



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
