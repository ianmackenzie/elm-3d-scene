module Scene3d.Material exposing
    ( Material
    , color, emissive, matte, metal, nonmetal, pbr
    , Texture, constant, load, loadWith, loadChannel, loadChannelWith
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

@docs Texture, constant, load, loadWith, loadChannel, loadChannelWith

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
import Scene3d.Material.Channel as Channel exposing (Channel)
import Scene3d.Mesh exposing (No, Yes)
import Scene3d.Types as Types exposing (LinearRgb(..))
import Task exposing (Task)
import WebGL.Texture


type alias Material attributes =
    Types.Material attributes


color : Color -> Material a
color givenColor =
    Types.UnlitMaterial (Types.Constant (toVec3 givenColor))


matte : Color -> Material { a | normals : Yes }
matte materialColor =
    Types.LambertianMaterial
        (Types.Constant (ColorConversions.colorToLinearRgb materialColor))
        (Types.Constant Types.VerticalNormal)


emissive : Color -> Luminance -> Material a
emissive givenColor brightness =
    Types.EmissiveMaterial
        (Types.Constant (ColorConversions.colorToLinearRgb givenColor))
        (Luminance.inNits brightness)


metal : { baseColor : Color, roughness : Float } -> Material { a | normals : Yes }
metal { baseColor, roughness } =
    pbr { baseColor = baseColor, roughness = roughness, metallic = 1 }


nonmetal : { baseColor : Color, roughness : Float } -> Material { a | normals : Yes }
nonmetal { baseColor, roughness } =
    pbr { baseColor = baseColor, roughness = roughness, metallic = 0 }


pbr : { baseColor : Color, roughness : Float, metallic : Float } -> Material { a | normals : Yes }
pbr { baseColor, roughness, metallic } =
    Types.PbrMaterial
        (Types.Constant (ColorConversions.colorToLinearRgb baseColor))
        (Types.Constant (clamp 0 1 roughness))
        (Types.Constant (clamp 0 1 metallic))
        (Types.Constant Types.VerticalNormal)


type alias Texture value =
    Types.Texture value


constant : value -> Texture value
constant givenValue =
    Types.Constant givenValue


load : String -> Task WebGL.Texture.Error (Texture value)
load url =
    loadWith WebGL.Texture.defaultOptions url


loadWith : WebGL.Texture.Options -> String -> Task WebGL.Texture.Error (Texture value)
loadWith options url =
    loadImpl options Channel.luminance url


type alias Channel =
    Channel.Channel


loadChannel : Channel -> String -> Task WebGL.Texture.Error (Texture Float)
loadChannel channel url =
    loadChannelWith WebGL.Texture.defaultOptions channel url


loadChannelWith : WebGL.Texture.Options -> Channel -> String -> Task WebGL.Texture.Error (Texture Float)
loadChannelWith options channel url =
    loadImpl options channel url


loadImpl : WebGL.Texture.Options -> Channel -> String -> Task WebGL.Texture.Error (Texture value)
loadImpl options (Types.Channel channel) url =
    WebGL.Texture.load url
        |> Task.map
            (\data ->
                Types.Texture
                    { url = url
                    , options = options
                    , data = data
                    , channel = channel
                    }
            )


map : (a -> b) -> Texture a -> Texture b
map function texture =
    case texture of
        Types.Constant value ->
            Types.Constant (function value)

        Types.Texture properties ->
            Types.Texture properties


toVec3 : Color -> Vec3
toVec3 givenColor =
    let
        ( red, green, blue ) =
            Color.toRGB givenColor
    in
    Math.Vector3.vec3 (red / 255) (green / 255) (blue / 255)


texturedColor : Texture Color -> Material { a | uvs : Yes }
texturedColor colorTexture =
    Types.UnlitMaterial (map toVec3 colorTexture)


texturedMatte : Texture Color -> Material { a | normals : Yes, uvs : Yes }
texturedMatte colorTexture =
    Types.LambertianMaterial
        (map ColorConversions.colorToLinearRgb colorTexture)
        (Types.Constant Types.VerticalNormal)


texturedEmissive : Texture Color -> Luminance -> Material { a | uvs : Yes }
texturedEmissive colorTexture brightness =
    Types.EmissiveMaterial
        (map ColorConversions.colorToLinearRgb colorTexture)
        (Luminance.inNits brightness)


texturedMetal :
    { baseColor : Texture Color
    , roughness : Texture Float
    }
    -> Material { a | normals : Yes, uvs : Yes }
texturedMetal { baseColor, roughness } =
    texturedPbr
        { baseColor = baseColor
        , roughness = roughness
        , metallic = constant 1
        }


texturedNonmetal :
    { baseColor : Texture Color
    , roughness : Texture Float
    }
    -> Material { a | normals : Yes, uvs : Yes }
texturedNonmetal { baseColor, roughness } =
    texturedPbr
        { baseColor = baseColor
        , roughness = roughness
        , metallic = constant 0
        }


texturedPbr :
    { baseColor : Texture Color
    , roughness : Texture Float
    , metallic : Texture Float
    }
    -> Material { a | normals : Yes, uvs : Yes }
texturedPbr { baseColor, roughness, metallic } =
    Types.PbrMaterial
        (map ColorConversions.colorToLinearRgb baseColor)
        (map (clamp 0 1) roughness)
        (map (clamp 0 1) metallic)
        (Types.Constant Types.VerticalNormal)


type alias NormalMap =
    Types.NormalMap


normalMappedMatte : Texture Color -> Texture NormalMap -> NormalMapped
normalMappedMatte colorTexture normalMapTexture =
    Types.LambertianMaterial
        (map ColorConversions.colorToLinearRgb colorTexture)
        normalMapTexture


normalMappedMetal :
    { baseColor : Texture Color
    , roughness : Texture Float
    , normalMap : Texture NormalMap
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
    { baseColor : Texture Color
    , roughness : Texture Float
    , normalMap : Texture NormalMap
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
    { baseColor : Texture Color
    , roughness : Texture Float
    , metallic : Texture Float
    , normalMap : Texture NormalMap
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
    Material { normals : Yes, uvs : No, tangents : No }


{-| A material that can be applied to an [`Unlit`](Scene3d-Mesh#Unlit) mesh that
has UV coordinates but no normal vectors. This includes the `Plain` materials
plus their textured versions [`texturedColor`](#texturedColor) and
[`texturedEmissive`](#texturedEmissive).
-}
type alias Unlit =
    Material { normals : No, uvs : Yes, tangents : No }


{-| A material that can be applied to a [`Textured`](Scene3d-Mesh#Textured) mesh
that has normal vectors and UV coordinates. This includes all the `Unlit` and
`Uniform` materials plus the textured versions of the `Uniform` materials
([`texturedMatte`](#texturedMatte), [`texturedMetal`](#texturedMetal) etc.)
-}
type alias Textured =
    Material { normals : Yes, uvs : Yes, tangents : No }


{-| A mesh with normal and tangent vectors but no UV coordinates, allowing for
some specialized material models such as brushed metal but no texturing.
-}
type alias Anisotropic =
    Material { normals : Yes, uvs : No, tangents : Yes }


{-| A mesh with normal vectors, UV coordinates and tangent vectors at each
vertex, allowing for full texturing including normal maps.
-}
type alias NormalMapped =
    Material { normals : Yes, uvs : Yes, tangents : Yes }


plain : Plain -> Material a
plain =
    coerce


uniform : Uniform -> Material { a | normals : Yes }
uniform =
    coerce


unlit : Unlit -> Material { a | uvs : Yes }
unlit =
    coerce


textured : Textured -> Material { a | normals : Yes, uvs : Yes }
textured =
    coerce


anisotropic : Anisotropic -> Material { a | normals : Yes, tangents : Yes }
anisotropic =
    coerce


coerce : Material a -> Material b
coerce material =
    case material of
        Types.UnlitMaterial colorTexture ->
            Types.UnlitMaterial colorTexture

        Types.EmissiveMaterial colorTexture brightness ->
            Types.EmissiveMaterial colorTexture brightness

        Types.LambertianMaterial colorTexture normalMapTexture ->
            Types.LambertianMaterial colorTexture normalMapTexture

        Types.PbrMaterial colorTexture roughnessTexture metallicTexture normalMapTexture ->
            Types.PbrMaterial colorTexture roughnessTexture metallicTexture normalMapTexture
