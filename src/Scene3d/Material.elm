module Scene3d.Material exposing
    ( Material
    , color, emissive, matte, metal, nonmetal, pbr
    , Channel, constant, load
    , texturedColor, texturedEmissive, texturedMatte, texturedMetal, texturedNonmetal, texturedPbr
    , NormalMap
    , normalMappedMatte, normalMappedMetal, normalMappedNonmetal, normalMappedPbr
    , Plain, Unlit, Uniform, Textured, NormalMapped
    , plain, unlit, uniform, textured
    )

{-|

@docs Material


# Simple materials

@docs color, emissive, matte, metal, nonmetal, pbr


# Textured materials

@docs Channel, constant, load

@docs texturedColor, texturedEmissive, texturedMatte, texturedMetal, texturedNonmetal, texturedPbr


# Normal-mapped materials

@docs NormalMap

@docs normalMappedMatte, normalMappedMetal, normalMappedNonmetal, normalMappedPbr


# Type annotations

@docs Plain, Unlit, Uniform, Textured, NormalMapped

@docs plain, unlit, uniform, textured

-}

import Color exposing (Color)
import Luminance exposing (Luminance)
import Math.Vector3 exposing (Vec3)
import Scene3d.Chromaticity exposing (Chromaticity)
import Scene3d.ColorConversions as ColorConversions
import Scene3d.Mesh exposing (Attributes)
import Scene3d.Types as Types exposing (LinearRgb(..))
import Task exposing (Task)
import WebGL.Texture


type alias Material attributes =
    Types.Material attributes


color : Color -> Material a
color givenColor =
    Types.UnlitMaterial (Types.Constant (toVec3 givenColor))


matte : Color -> Material { a | normal : Attributes }
matte materialColor =
    Types.LambertianMaterial
        (Types.Constant (ColorConversions.colorToLinearRgb materialColor))
        (Types.Constant Types.VerticalNormal)


emissive : Color -> Luminance -> Material a
emissive givenColor brightness =
    Types.EmissiveMaterial
        (Types.Constant (ColorConversions.colorToLinearRgb givenColor))
        (Luminance.inNits brightness)


metal : { baseColor : Color, roughness : Float } -> Material { a | normal : Attributes }
metal { baseColor, roughness } =
    pbr { baseColor = baseColor, roughness = roughness, metallic = 1 }


nonmetal : { baseColor : Color, roughness : Float } -> Material { a | normal : Attributes }
nonmetal { baseColor, roughness } =
    pbr { baseColor = baseColor, roughness = roughness, metallic = 0 }


pbr : { baseColor : Color, roughness : Float, metallic : Float } -> Material { a | normal : Attributes }
pbr { baseColor, roughness, metallic } =
    Types.PbrMaterial
        (Types.Constant (ColorConversions.colorToLinearRgb baseColor))
        (Types.Constant (clamp 0 1 roughness))
        (Types.Constant (clamp 0 1 metallic))
        (Types.Constant Types.VerticalNormal)


type alias Channel value =
    Types.Channel value


constant : value -> Channel value
constant givenValue =
    Types.Constant givenValue


load : String -> Task WebGL.Texture.Error (Channel value)
load url =
    WebGL.Texture.load url
        |> Task.map
            (\texture ->
                Types.Textured
                    { url = url
                    , options = WebGL.Texture.defaultOptions
                    , data = texture
                    }
            )


map : (a -> b) -> Channel a -> Channel b
map function channel =
    case channel of
        Types.Constant value ->
            Types.Constant (function value)

        Types.Textured texture ->
            Types.Textured texture


toVec3 : Color -> Vec3
toVec3 givenColor =
    let
        ( red, green, blue ) =
            Color.toRGB givenColor
    in
    Math.Vector3.vec3 (red / 255) (green / 255) (blue / 255)


texturedColor : Channel Color -> Material { a | uv : Attributes }
texturedColor colorChannel =
    Types.UnlitMaterial (map toVec3 colorChannel)


texturedMatte : Channel Color -> Material { a | normal : Attributes, uv : Attributes }
texturedMatte colorChannel =
    Types.LambertianMaterial
        (map ColorConversions.colorToLinearRgb colorChannel)
        (Types.Constant Types.VerticalNormal)


texturedEmissive : Channel Color -> Luminance -> Material { a | uv : Attributes }
texturedEmissive colorChannel brightness =
    Types.EmissiveMaterial
        (map ColorConversions.colorToLinearRgb colorChannel)
        (Luminance.inNits brightness)


texturedMetal :
    { baseColor : Channel Color
    , roughness : Channel Float
    }
    -> Material { a | normal : Attributes, uv : Attributes }
texturedMetal { baseColor, roughness } =
    texturedPbr
        { baseColor = baseColor
        , roughness = roughness
        , metallic = constant 1
        }


texturedNonmetal :
    { baseColor : Channel Color
    , roughness : Channel Float
    }
    -> Material { a | normal : Attributes, uv : Attributes }
texturedNonmetal { baseColor, roughness } =
    texturedPbr
        { baseColor = baseColor
        , roughness = roughness
        , metallic = constant 0
        }


texturedPbr :
    { baseColor : Channel Color
    , roughness : Channel Float
    , metallic : Channel Float
    }
    -> Material { a | normal : Attributes, uv : Attributes }
texturedPbr { baseColor, roughness, metallic } =
    Types.PbrMaterial
        (map ColorConversions.colorToLinearRgb baseColor)
        (map (clamp 0 1) roughness)
        (map (clamp 0 1) metallic)
        (Types.Constant Types.VerticalNormal)


type alias NormalMap =
    Types.NormalMap


normalMappedMatte : Channel Color -> Channel NormalMap -> NormalMapped
normalMappedMatte colorChannel normalMapChannel =
    Types.LambertianMaterial
        (map ColorConversions.colorToLinearRgb colorChannel)
        normalMapChannel


normalMappedMetal :
    { baseColor : Channel Color
    , roughness : Channel Float
    , normalMap : Channel NormalMap
    }
    -> NormalMapped
normalMappedMetal { baseColor, roughness, normalMap } =
    normalMappedPbr
        { baseColor = baseColor
        , roughness = roughness
        , metallic = constant 1
        , normalMap = normalMap
        }


normalMappedNonmetal :
    { baseColor : Channel Color
    , roughness : Channel Float
    , normalMap : Channel NormalMap
    }
    -> NormalMapped
normalMappedNonmetal { baseColor, roughness, normalMap } =
    normalMappedPbr
        { baseColor = baseColor
        , roughness = roughness
        , metallic = constant 0
        , normalMap = normalMap
        }


normalMappedPbr :
    { baseColor : Channel Color
    , roughness : Channel Float
    , metallic : Channel Float
    , normalMap : Channel NormalMap
    }
    -> NormalMapped
normalMappedPbr { baseColor, roughness, metallic, normalMap } =
    Types.PbrMaterial
        (map ColorConversions.colorToLinearRgb baseColor)
        (map (clamp 0 1) roughness)
        (map (clamp 0 1) metallic)
        normalMap


{-| A simple material that doesn't require any particular vertex attributes to
be present and so can be applied to any mesh. The only possibilities here are
[`color`](#color) and [`emissive`](#emissive).
-}
type alias Plain =
    Material {}


{-| A material that can be applied to an [`Uniform`](Scene3d-Mesh#Uniform) mesh
that has normal vectors but no UV coordinates. This includes the `Plain`
materials plus [`matte`](#matte), [`metal`](#metal), [`nonmetal`](#nonmetal) and
[`pbr`](#pbr).
-}
type alias Uniform =
    Material { normal : Attributes }


{-| A material that can be applied to an [`Unlit`](Scene3d-Mesh#Unlit) mesh that
has UV coordinates but no normal vectors. This includes the `Plain` materials
plus their textured versions [`texturedColor`](#texturedColor) and
[`texturedEmissive`](#texturedEmissive).
-}
type alias Unlit =
    Material { uv : Attributes }


{-| A material that can be applied to a [`Textured`](Scene3d-Mesh#Textured) mesh
that has normal vectors and UV coordinates. This includes all the `Unlit` and
`Uniform` materials plus the textured versions of the `Uniform` materials
([`texturedMatte`](#texturedMatte), [`texturedMetal`](#texturedMetal) etc.)
-}
type alias Textured =
    Material { normal : Attributes, uv : Attributes }


{-| A mesh with normal and tangent vectors but no UV coordinates, allowing for
some specialized material models such as brushed metal but no texturing.
-}
type alias Anisotropic =
    Material { normal : Attributes, tangent : Attributes }


{-| A mesh with normal vectors, UV coordinates and tangent vectors at each
vertex, allowing for full texturing including normal maps.
-}
type alias NormalMapped =
    Material { normal : Attributes, uv : Attributes, tangent : Attributes }


plain : Plain -> Material a
plain =
    coerce


uniform : Uniform -> Material { a | normal : Attributes }
uniform =
    coerce


unlit : Unlit -> Material { a | uv : Attributes }
unlit =
    coerce


textured : Textured -> Material { a | normal : Attributes, uv : Attributes }
textured =
    coerce


anisotropic : Anisotropic -> Material { a | normal : Attributes, tangent : Attributes }
anisotropic =
    coerce


coerce : Material a -> Material b
coerce material =
    case material of
        Types.UnlitMaterial colorChannel ->
            Types.UnlitMaterial colorChannel

        Types.EmissiveMaterial colorChannel brightness ->
            Types.EmissiveMaterial colorChannel brightness

        Types.LambertianMaterial colorChannel normalMapChannel ->
            Types.LambertianMaterial colorChannel normalMapChannel

        Types.PbrMaterial colorChannel roughnessChannel metallicChannel normalMapChannel ->
            Types.PbrMaterial colorChannel roughnessChannel metallicChannel normalMapChannel
