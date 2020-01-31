module Scene3d.Material exposing
    ( Material, Pbr
    , color, emissive, matte, pbr
    , Channel, constant, load
    , colorTexture, emissiveTexture, matteTexture, texturedPbr
    , Plain, Unlit, Untextured, Textured
    , plain, unlit, untextured
    -- , withNormalMap
    )

{-|

@docs Material, Pbr


# Simple materials

@docs color, emissive, matte, pbr


# Textured materials

@docs Channel, constant, load

@docs colorTexture, emissiveTexture, matteTexture, texturedPbr


# Specific types

@docs Plain, Unlit, Untextured, Textured

@docs plain, unlit, untextured

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
    Types.LambertianMaterial (Types.Constant (ColorConversions.colorToLinearRgb materialColor))


emissive : Color -> Luminance -> Material a
emissive givenColor brightness =
    Types.EmissiveMaterial
        (Types.Constant (ColorConversions.colorToLinearRgb givenColor))
        (Luminance.inNits brightness)


type alias Pbr =
    { baseColor : Color
    , roughness : Float
    , metallic : Float
    }


pbr : Pbr -> Material { a | normal : Attributes }
pbr { baseColor, roughness, metallic } =
    Types.PbrMaterial
        (Types.Constant (ColorConversions.colorToLinearRgb baseColor))
        (Types.Constant (clamp 0 1 roughness))
        (Types.Constant (clamp 0 1 metallic))


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


colorTexture : Channel Color -> Material { a | uv : Attributes }
colorTexture colorChannel =
    Types.UnlitMaterial (map toVec3 colorChannel)


matteTexture : Channel Color -> Material { a | normal : Attributes, uv : Attributes }
matteTexture colorChannel =
    Types.LambertianMaterial (map ColorConversions.colorToLinearRgb colorChannel)


emissiveTexture : Channel Color -> Luminance -> Material { a | uv : Attributes }
emissiveTexture colorChannel brightness =
    Types.EmissiveMaterial
        (map ColorConversions.colorToLinearRgb colorChannel)
        (Luminance.inNits brightness)


type alias TexturedPbr =
    { baseColor : Channel Color
    , roughness : Channel Float
    , metallic : Channel Float
    }


texturedPbr : TexturedPbr -> Material { a | normal : Attributes, uv : Attributes }
texturedPbr { baseColor, roughness, metallic } =
    Types.PbrMaterial
        (map ColorConversions.colorToLinearRgb baseColor)
        (map (clamp 0 1) roughness)
        (map (clamp 0 1) metallic)



-- withNormalMap :
--     Texture
--     -> Material { a | uv : Attributes, normal : Attributes, tangents : () }
--     -> Material { a | uv : Attributes, normal : Attributes, tangents : () }


{-| A simple material that doesn't require any particular vertex attributes to
be present and so can be applied to any mesh. The only possibilities here are
[`color`](#color) and [`emissive`](#emissive).
-}
type alias Plain =
    Material {}


{-| A material that can be applied to an [`Untextured`](Scene3d-Mesh#Untextured)
mesh that has normal vectors but no UV coordinates. This includes the `Plain`
materials plus [`matte`](#matte), [`metal`](#metal), [`nonmetal`](#nonmetal) and
[`pbr`](#pbr).
-}
type alias Untextured =
    Material { normal : Attributes }


{-| A material that can be applied to an [`Unlit`](Scene3d-Mesh#Unlit) mesh that
has UV coordinates but no normal vectors. This includes the `Plain` materials
plus their textured versions [`colorTexture`](#colorTexture) and
[`emissiveTexture`](#emissiveTexture).
-}
type alias Unlit =
    Material { uv : Attributes }


{-| A material that can be applied to a [`Textured`](Scene3d-Mesh#Textured) mesh
that has normal vectors and UV coordinates. This includes all the `Unlit` and
`Untextured` materials plus the textured versions of the `Untextured` materials
([`matteTexture`](#matteTexture), [`texturedMetal`](#texturedMetal) etc.)
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


untextured : Untextured -> Material { a | normal : Attributes }
untextured =
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

        Types.LambertianMaterial colorChannel ->
            Types.LambertianMaterial colorChannel

        Types.PbrMaterial colorChannel roughnessChannel metallicChannel ->
            Types.PbrMaterial colorChannel roughnessChannel metallicChannel
