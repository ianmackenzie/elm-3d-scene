module Scene3d.Material exposing
    ( Material
    , color, emissive, matte
    , metal, nonmetal, pbr
    , Texture, constant, load
    , texturedColor, texturedEmissive, texturedMatte, texturedMetal, texturedNonmetal, texturedPbr
    , loadWith, nearestNeighborFiltering, bilinearFiltering, trilinearFiltering
    , Plain, Unlit, Uniform, Textured
    , plain, unlit, uniform
    )

{-|

@docs Material


# Simple materials

@docs color, emissive, matte


# Physically-based materials

[Physically based rendering](https://learnopengl.com/PBR/Theory) (PBR) is a
modern rendering technique that attempts to realistically render real-world
materials such as metals and plastics. `elm-3d-scene` uses a fairly simple,
common variant of PBR where materials have three main parameters:

  - Base color
  - Roughness, with 0 meaning perfectly smooth (shiny) and 1 meaning very rough
    (matte)
  - 'Metallicness', usually either 0 or 1, with 0 meaning non-metal and 1
    meaning metal

@docs metal, nonmetal, pbr


# Textured materials

[Textured](https://en.wikipedia.org/wiki/Texture_mapping) materials behave just
like their non-textured versions above, but require a mesh that has [UV](https://learnopengl.com/Getting-started/Textures)
(texture) coordinates. Color, roughness and metallicness can then be controlled
by a texture image instead of being restricted to constant values.

@docs Texture, constant, load

@docs texturedColor, texturedEmissive, texturedMatte, texturedMetal, texturedNonmetal, texturedPbr


## Customized textures

@docs loadWith, nearestNeighborFiltering, bilinearFiltering, trilinearFiltering


# Type annotations

The functions in this module all return values with a 'free' type parameter -
for example, the return type of `Material.matte` is

    Material coordinates { a | normals : () }

This makes most code simpler (it means that such a material can work with _any_
kind of mesh that has normal vectors, even if for example that mesh also has
texture coordinates) but makes it tricky to store a `Material` value in your own
data structures without those data structures _also_ needing a type parameter.
The `coordinates` type parameter can usually be set to just `WorldCoordinates`
(a type you will need to define yourself), but the `a` is a bit trickier.

The type aliases and functions below help deal with this problem in a convenient
way. To store a material in a data structure, you can use one of the type
aliases. For example, the material above might be stored as a

    Material.Uniform WorldCoordinates

Then, if you need to turn this value _back_ into a

    Material coordinates { a | normals : () }

(so that you could apply it to a textured mesh, for example) you can use
`Material.uniform` to do so. You can think of `Material.uniform material` as
saying "yes, I know this is a uniform material, but I still want to apply it to
this textured mesh".

@docs Plain, Unlit, Uniform, Textured

@docs plain, unlit, uniform

-}

import Color exposing (Color)
import Luminance exposing (Luminance)
import Math.Vector3 exposing (Vec3)
import Quantity
import Scene3d.ColorConversions as ColorConversions
import Scene3d.Types as Types exposing (Chromaticity, LinearRgb(..))
import Task exposing (Task)
import WebGL.Texture


{-| A `Material` controls the color, reflectivity etc. of a given object. It may
be constant across the object or be textured.

The `attributes` type parameter of a material is used to restrict what objects
it can be used with. For example, `Material.matte` returns a value with an
`attributes` type of `{ a | normals : () }`; you can read this as "this material
can be applied to any mesh that has normals".

The `coordinates` type parameter is currently unused but will be used in the
future for things like [procedural textures](https://en.wikipedia.org/wiki/Procedural_texture)
defined in a particular coordinate system; those textures will then only be able
to be applied to objects defined in the same coordinate system.

-}
type alias Material coordinates attributes =
    Types.Material coordinates attributes


{-| A simple constant color material. This material can be applied to any
object and ignores lighting entirely - the entire object will have exactly the
given color regardless of lights or scene exposure/white balance settings.
-}
color : Color -> Material coordinates attributes
color givenColor =
    Types.UnlitMaterial Types.UseMeshUvs (Types.Constant (toVec3 givenColor))


{-| A perfectly matte ([Lambertian](https://en.wikipedia.org/wiki/Lambertian_reflectance))
material which reflects light equally in all directions. Lambertian materials
are faster to render than physically-based materials like `Material.metal` or
`Material.nonmetal`, so consider using them for large surfaces like floors that
don't need to be shiny.
-}
matte : Color -> Material coordinates { a | normals : () }
matte materialColor =
    Types.LambertianMaterial Types.UseMeshUvs
        (Types.Constant (ColorConversions.colorToLinearRgb materialColor))
        (Types.Constant Types.VerticalNormal)


{-| An emissive or 'glowing' material, where you specify the [chromaticity](Scene3d#Chromaticity)
and intensity of the emitted light.
-}
emissive : Chromaticity -> Luminance -> Material coordinates attributes
emissive givenChromaticity brightness =
    let
        baseColor =
            ColorConversions.chromaticityToLinearRgb (Quantity.float 1) givenChromaticity
    in
    Types.EmissiveMaterial Types.UseMeshUvs (Types.Constant baseColor) brightness


{-| A metal material such as steel, aluminum, gold etc. See [here](https://docs.unrealengine.com/en-US/Engine/Rendering/Materials/PhysicallyBased/index.html)
and [here](https://www.chaosgroup.com/blog/understanding-metalness) for base
colors of different metals.
-}
metal : { baseColor : Color, roughness : Float } -> Material coordinates { a | normals : () }
metal { baseColor, roughness } =
    pbr { baseColor = baseColor, roughness = roughness, metallic = 1 }


{-| A non-metal material such as plastic, wood, paper etc.
-}
nonmetal : { baseColor : Color, roughness : Float } -> Material coordinates { a | normals : () }
nonmetal { baseColor, roughness } =
    pbr { baseColor = baseColor, roughness = roughness, metallic = 0 }


{-| A custom PBR material with a `metallic` parameter that can be anywhere
between 0 and 1. Values in between 0 and 1 can be used to approximate things
like dusty metal, where a surface can be thought of as partially metal and
partially non-metal.
-}
pbr :
    { baseColor : Color, roughness : Float, metallic : Float }
    -> Material coordinates { a | normals : () }
pbr { baseColor, roughness, metallic } =
    Types.PbrMaterial Types.UseMeshUvs
        (Types.Constant (ColorConversions.colorToLinearRgb baseColor))
        (Types.Constant (clamp 0 1 roughness))
        (Types.Constant (clamp 0 1 metallic))
        (Types.Constant Types.VerticalNormal)


{-| A `Texture` value represents an image that is mapped over the surface of an
object. Textures can be used to control the color at different points on an
object (`Texture Color`) but can also be used to control roughness or
metallicness when using a physically-based material (`Texture Float`).
-}
type alias Texture value =
    Types.Texture value


{-| A special texture that has the same value everywhere. This can be useful
with materials like [`texturedPbr`](#texturedPbr) which take multiple `Texture`
arguments; sometimes you might want to use an actual texture for color but a
constant value for roughness (or vice versa).
-}
constant : value -> Texture value
constant givenValue =
    Types.Constant givenValue


{-| Load a texture from a given URL. Note that the resulting value can be used
as either a `Texture Color` _or_ a `Texture Float` - if used as a
`Texture Float` then it will be the greyscale value of each pixel that is used
(more precisely, its [luminance](https://en.wikipedia.org/wiki/Relative_luminance)).

The loaded texture will use [bilinear texture filtering](#bilinearFiltering). To
use nearest-neighbor filtering, trilinear filtering or to customize other
texture options, use [`loadWith`](#loadWith) instead.

-}
load : String -> Task WebGL.Texture.Error (Texture value)
load url =
    loadWith bilinearFiltering url


{-| Load a texture with particular [options](https://package.elm-lang.org/packages/elm-explorations/webgl/latest/WebGL-Texture#Options),
which control things like what form of [texture filtering](https://en.wikipedia.org/wiki/Texture_filtering)
is used and how out-of-range texture coordinates are interpreted (clamped,
wrapped around, etc.).

This module contains a few sensible defaults ([`nearestNeighborFiltering`](#nearestNeighborFiltering),
[`bilinearFiltering`](#bilinearFiltering), and [`trilinearFiltering`](#trilinearFiltering))
but you can directly construct your own custom options record if desired.

-}
loadWith : WebGL.Texture.Options -> String -> Task WebGL.Texture.Error (Texture value)
loadWith options url =
    loadImpl options url


loadImpl : WebGL.Texture.Options -> String -> Task WebGL.Texture.Error (Texture value)
loadImpl options url =
    WebGL.Texture.loadWith options url
        |> Task.map
            (\data ->
                Types.Texture
                    { url = url
                    , options = options
                    , data = data
                    }
            )


{-| Don't interpolate between texture pixels at all when rendering; each
on-screen pixel will simply get the color of the _nearest_ texture pixel. This
can be useful if you're deliberately going for a 'pixelated' look and want
texture pixels to show up exactly on screen without any blurring.

    nearestNeighborFiltering =
        { minify = WebGL.Texture.nearest
        , magnify = WebGL.Texture.nearest
        , horizontalWrap = WebGL.Texture.repeat
        , verticalWrap = WebGL.Texture.repeat
        , flipY = True
        }

-}
nearestNeighborFiltering : WebGL.Texture.Options
nearestNeighborFiltering =
    { minify = WebGL.Texture.nearest
    , magnify = WebGL.Texture.nearest
    , horizontalWrap = WebGL.Texture.repeat
    , verticalWrap = WebGL.Texture.repeat
    , flipY = True
    }


{-| Apply some simple texture smoothing; each on-screen pixel will be a weighted
average of the four closest texture pixels. No [mipmapping](https://en.wikipedia.org/wiki/Mipmap)
is used, so some pixelation/aliasing may still occur especially for far-away
objects where one texture pixel is much smaller than one screen pixel.

    bilinearFiltering =
        { minify = WebGL.Texture.linear
        , magnify = WebGL.Texture.linear
        , horizontalWrap = WebGL.Texture.repeat
        , verticalWrap = WebGL.Texture.repeat
        , flipY = True
        }

-}
bilinearFiltering : WebGL.Texture.Options
bilinearFiltering =
    { minify = WebGL.Texture.linear
    , magnify = WebGL.Texture.linear
    , horizontalWrap = WebGL.Texture.repeat
    , verticalWrap = WebGL.Texture.repeat
    , flipY = True
    }


{-| Interpolate between nearby texture pixels as with bilinear filtering, but
_also_ interpolate between the two nearest [mipmap](https://en.wikipedia.org/wiki/Mipmap)
levels. This will generally give the smoothest possible appearance, but may lead
to excessive blurriness especially when viewing surfaces at very shallow angles.

    trilinearFiltering =
        { minify = WebGL.Texture.linearMipmapLinear
        , magnify = WebGL.Texture.linear
        , horizontalWrap = WebGL.Texture.repeat
        , verticalWrap = WebGL.Texture.repeat
        , flipY = True
        }

-}
trilinearFiltering : WebGL.Texture.Options
trilinearFiltering =
    { minify = WebGL.Texture.linearMipmapLinear
    , magnify = WebGL.Texture.linear
    , horizontalWrap = WebGL.Texture.repeat
    , verticalWrap = WebGL.Texture.repeat
    , flipY = True
    }


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


{-| A textured plain-color material, unaffected by lighting.
-}
texturedColor : Texture Color -> Material coordinates { a | uvs : () }
texturedColor colorTexture =
    Types.UnlitMaterial Types.UseMeshUvs (map toVec3 colorTexture)


{-| A textured matte material.
-}
texturedMatte : Texture Color -> Material coordinates { a | normals : (), uvs : () }
texturedMatte colorTexture =
    Types.LambertianMaterial Types.UseMeshUvs
        (map ColorConversions.colorToLinearRgb colorTexture)
        (Types.Constant Types.VerticalNormal)


{-| A textured emissive material. The color from the texture will be multiplied
by the given luminance to obtain the final emissive color.
-}
texturedEmissive : Texture Color -> Luminance -> Material coordinates { a | uvs : () }
texturedEmissive colorTexture brightness =
    Types.EmissiveMaterial Types.UseMeshUvs
        (map ColorConversions.colorToLinearRgb colorTexture)
        brightness


{-| A textured metal material. If you only have a texture for one of the two
parameters (base color and roughness), you can use [`Material.constant`](#constant)
for the other.
-}
texturedMetal :
    { baseColor : Texture Color
    , roughness : Texture Float
    }
    -> Material coordinates { a | normals : (), uvs : () }
texturedMetal { baseColor, roughness } =
    texturedPbr
        { baseColor = baseColor
        , roughness = roughness
        , metallic = constant 1
        }


{-| A textured non-metal material.
-}
texturedNonmetal :
    { baseColor : Texture Color
    , roughness : Texture Float
    }
    -> Material coordinates { a | normals : (), uvs : () }
texturedNonmetal { baseColor, roughness } =
    texturedPbr
        { baseColor = baseColor
        , roughness = roughness
        , metallic = constant 0
        }


{-| A fully custom textured PBR material, where textures can be used to control
all three parameters.
-}
texturedPbr :
    { baseColor : Texture Color
    , roughness : Texture Float
    , metallic : Texture Float
    }
    -> Material coordinates { a | normals : (), uvs : () }
texturedPbr { baseColor, roughness, metallic } =
    Types.PbrMaterial Types.UseMeshUvs
        (map ColorConversions.colorToLinearRgb baseColor)
        (map (clamp 0 1) roughness)
        (map (clamp 0 1) metallic)
        (Types.Constant Types.VerticalNormal)


type alias NormalMap =
    Types.NormalMap


normalMappedMatte : Texture Color -> Texture NormalMap -> NormalMapped coordinates
normalMappedMatte colorTexture normalMapTexture =
    Types.LambertianMaterial Types.UseMeshUvs
        (map ColorConversions.colorToLinearRgb colorTexture)
        normalMapTexture


normalMappedMetal :
    { baseColor : Texture Color
    , roughness : Texture Float
    , normalMap : Texture NormalMap
    }
    -> NormalMapped coordinates
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
    -> NormalMapped coordinates
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
    -> NormalMapped coordinates
normalMappedPbr { baseColor, roughness, metallic, normalMap } =
    Types.PbrMaterial Types.UseMeshUvs
        (map ColorConversions.colorToLinearRgb baseColor)
        (map (clamp 0 1) roughness)
        (map (clamp 0 1) metallic)
        normalMap


{-| A material that doesn't require any particular vertex attributes. The only
possibilities here are [`color`](#color) and [`emissive`](#emissive).
-}
type alias Plain coordinates =
    Material coordinates {}


{-| A material that requires normal vectors but not UV coordinates: [`matte`](#matte),
[`metal`](#metal), [`nonmetal`](#nonmetal) or [`pbr`](#pbr).
-}
type alias Uniform coordinates =
    Material coordinates { normals : () }


{-| A material that requires UV (texture) coordinates but not normal vectors:
[`texturedColor`](#texturedColor) or [`texturedEmissive`](#texturedEmissive).
-}
type alias Unlit coordinates =
    Material coordinates { uvs : () }


{-| A material that requires both normal vectors and UV coordinates:
[`texturedMatte`](#texturedMatte), [`texturedMetal`](#texturedMetal),
[`texturedNonmetal`](#texturedNonmetal) or [`texturedPbr`](#texturedPbr).
-}
type alias Textured coordinates =
    Material coordinates { normals : (), uvs : () }


type alias Anisotropic coordinates =
    Material coordinates { normals : (), tangents : () }


type alias NormalMapped coordinates =
    Material coordinates { normals : (), uvs : (), tangents : () }


{-| Convert a `Plain` material (that can only be applied to a [`Plain`](Scene3d-Mesh#Plain)
mesh) back into one that can be applied to _any_ mesh.
-}
plain : Plain coordinates -> Material coordinates attributes
plain =
    coerce


{-| Convert a `Uniform` material (one that can only be applied to a [`Uniform`](Scene3d-Mesh#Uniform)
mesh) back into one that can be applied to _any_ mesh that has normal vectors.
-}
uniform : Uniform coordinates -> Material coordinates { a | normals : () }
uniform =
    coerce


{-| Convert an `Unlit` material (one that can only be applied to an [`Unlit`](Scene3d-Mesh#Unlit)
mesh) back into one that can be applied to _any_ mesh that has texture
coordinates.
-}
unlit : Unlit coordinates -> Material coordinates { a | uvs : () }
unlit =
    coerce


textured : Textured coordinates -> Material coordinates { a | normals : (), uvs : () }
textured =
    coerce


anisotropic : Anisotropic coordinates -> Material coordinates { a | normals : (), tangents : () }
anisotropic =
    coerce


coerce : Material coordinates a -> Material coordinates b
coerce material =
    case material of
        Types.UnlitMaterial textureMap colorTexture ->
            Types.UnlitMaterial textureMap colorTexture

        Types.EmissiveMaterial textureMap colorTexture brightness ->
            Types.EmissiveMaterial textureMap colorTexture brightness

        Types.LambertianMaterial textureMap colorTexture normalMapTexture ->
            Types.LambertianMaterial textureMap colorTexture normalMapTexture

        Types.PbrMaterial textureMap colorTexture roughnessTexture metallicTexture normalMapTexture ->
            Types.PbrMaterial textureMap colorTexture roughnessTexture metallicTexture normalMapTexture
