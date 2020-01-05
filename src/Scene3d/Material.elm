module Scene3d.Material exposing
    ( Material
    , Plain, ForMeshWithNormals, ForMeshWithUvs, ForMeshWithNormalsAndUvs, ForMeshWithTangents
    , solidColor, diffuse, emissive
    , metal, nonmetal, hybrid
    , colorTexture
    -- , emissiveTexture
    -- , diffuseTexture
    -- , texturedMetal
    -- , texturedNonmetal
    -- , texturedHybrid
    -- , withNormalMap
    )

{-|

@docs Material

@docs Plain, ForMeshWithNormals, ForMeshWithUvs, ForMeshWithNormalsAndUvs, ForMeshWithTangents


## Simple materials

@docs solidColor, diffuse, emissive


## Physically-based materials

@docs metal, nonmetal, hybrid


## Simple textured materials

@docs colorTexture

-}

import Color exposing (Color)
import Luminance exposing (Luminance)
import Math.Vector3 exposing (Vec3)
import Scene3d.Chromaticity exposing (Chromaticity)
import Scene3d.ColorConversions as ColorConversions
import Scene3d.Types as Types exposing (LinearRgb(..))
import WebGL.Texture exposing (Texture)


type alias Material properties =
    Types.Material properties


type alias Plain =
    Material {}


type alias ForMeshWithNormals =
    Material { normals : () }


type alias ForMeshWithUvs =
    Material { uvs : () }


type alias ForMeshWithNormalsAndUvs =
    Material { normals : (), uvs : () }


type alias ForMeshWithTangents =
    Material { normals : (), uvs : (), tangents : () }


solidColor : Color -> Material a
solidColor givenColor =
    let
        ( red, green, blue ) =
            Color.toRGB givenColor
    in
    Types.ConstantMaterial <|
        Math.Vector3.vec3 (red / 255) (green / 255) (blue / 255)


emissive : Luminance -> Chromaticity -> Material a
emissive luminance chromaticity =
    let
        (LinearRgb chromaticityRgb) =
            ColorConversions.chromaticityToLinearRgb chromaticity

        nits =
            Luminance.inNits luminance
    in
    Types.EmissiveMaterial (LinearRgb (Math.Vector3.scale nits chromaticityRgb))


diffuse : Color -> Material { a | normals : () }
diffuse givenColor =
    Types.LambertianMaterial (ColorConversions.colorToLinearRgb givenColor)


metal : { baseColor : Color, roughness : Float } -> Material { a | normals : () }
metal { baseColor, roughness } =
    hybrid { baseColor = baseColor, roughness = roughness, metallic = 1 }


nonmetal : { baseColor : Color, roughness : Float } -> Material { a | normals : () }
nonmetal { baseColor, roughness } =
    hybrid { baseColor = baseColor, roughness = roughness, metallic = 0 }


hybrid : { baseColor : Color, roughness : Float, metallic : Float } -> Material { a | normals : () }
hybrid { baseColor, roughness, metallic } =
    Types.PbrMaterial
        (ColorConversions.colorToLinearRgb baseColor)
        (clamp 0 1 roughness)
        (clamp 0 1 metallic)


colorTexture : Texture -> Material { a | uvs : () }
colorTexture givenTexture =
    Types.ColorTextureMaterial givenTexture



-- solidColorTexture : Texture -> Material { a | uvs : () }
-- emissiveTexture : Luminance -> Texture -> Material { a | uvs : () }
-- diffuseTexture : Texture -> Material { a | normals : (), uvs : () }
-- texturedMetal : { baseColor : Texture, roughness : Texture } -> Material { a | normals : (), uvs : () }
-- texturedNonmetal : { baseColor : Texture, roughness : Texture } -> Material { a | normals : (), uvs : () }
-- texturedHybrid : { baseColor : Texture, roughness : Texture, metallic : Float } -> Material { a | normals : (), uvs : () }
-- {-| Add a normal map to an existing material
-- -}
-- withNormalMap :
--     Texture
--     -> Material { a | uvs : (), normals : (), tangents : () }
--     -> Material { a | uvs : (), normals : (), tangents : () }
