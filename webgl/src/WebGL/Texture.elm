module WebGL.Texture
    exposing
        ( Bigger
        , Error(..)
        , Options
        , Resize
        , Smaller
        , Texture
        , Wrap
        , clampToEdge
        , defaultOptions
        , linear
        , linearMipmapLinear
        , linearMipmapNearest
        , load
        , loadWith
        , mirroredRepeat
        , nearest
        , nearestMipmapLinear
        , nearestMipmapNearest
        , nonPowerOfTwoOptions
        , repeat
        , size
        )

{-|


# Texture

@docs Texture, load, Error, size


# Custom Loading

@docs loadWith, Options, defaultOptions


## Resizing

@docs Resize, linear, nearest
@docs nearestMipmapLinear, nearestMipmapNearest
@docs linearMipmapNearest, linearMipmapLinear
@docs Bigger, Smaller


## Wrapping

@docs Wrap, repeat, clampToEdge, mirroredRepeat


# Things You Shouldn’t Do

@docs nonPowerOfTwoOptions

-}

import Elm.Kernel.Texture
import Task exposing (Task)


{-| Use `Texture` to pass the `sampler2D` uniform value to the shader.
You can create a texture with [`load`](#load) or [`loadWith`](#loadWith)
and measure its dimensions with [`size`](#size).
-}
type Texture
    = Texture


{-| Loads a texture from the given url with default options.
PNG and JPEG are known to work, but other formats have not been as
well-tested yet.

The Y axis of the texture is flipped automatically for you, so it has
the same direction as in the clip-space, i.e. pointing up.

If you need to change flipping, filtering or wrapping, you can use
[`loadWith`](#loadWith).

    load url =
        loadWith defaultOptions url

-}
load : String -> Task Error Texture
load =
    loadWith defaultOptions


{-| Loading a texture can result in two kinds of errors:

  - `LoadError` means the image did not load for some reason. Maybe
    it was a network problem, or maybe it was a bad file format.

  - `SizeError` means you are trying to load a weird shaped image.
    For most operations you want a rectangle where the width is a power
    of two and the height is a power of two. This is more efficient on
    the GPU and it makes mipmapping possible. You can use
    [`nonPowerOfTwoOptions`](#nonPowerOfTwoOptions) to get things working
    now, but it is way better to create power-of-two assets!

-}
type Error
    = LoadError
    | SizeError Int Int


{-| Same as load, but allows to set options.
-}
loadWith : Options -> String -> Task Error Texture
loadWith { magnify, minify, horizontalWrap, verticalWrap, flipY } url =
    let
        expand (Resize mag) (Resize min) (Wrap hor) (Wrap vert) =
            Elm.Kernel.Texture.load mag min hor vert flipY url
    in
    expand magnify minify horizontalWrap verticalWrap


{-| `Options` describe how to:

  - `magnify` - how to [`Resize`](#Resize) into a bigger texture
  - `minify` - how to [`Resize`](#Resize) into a smaller texture
  - `horizontalWrap` - how to [`Wrap`](#Wrap) the texture horizontally if the width is not a power of two
  - `verticalWrap` - how to [`Wrap`](#Wrap) the texture vertically if the height is not a power of two
  - `flipY` - flip the Y axis of the texture so it has the same direction
    as the clip-space, i.e. pointing up.

You can read more about these parameters in the
[specification](https://www.khronos.org/opengles/sdk/docs/man/xhtml/glTexParameter.xml).

-}
type alias Options =
    { magnify : Resize Bigger
    , minify : Resize Smaller
    , horizontalWrap : Wrap
    , verticalWrap : Wrap
    , flipY : Bool
    }


{-| Default options for the loaded texture.

    { magnify = linear
    , minify = nearestMipmapLinear
    , horizontalWrap = repeat
    , verticalWrap = repeat
    , flipY = True
    }

-}
defaultOptions : Options
defaultOptions =
    { magnify = linear
    , minify = nearestMipmapLinear
    , horizontalWrap = repeat
    , verticalWrap = repeat
    , flipY = True
    }


{-| The exact options needed to load textures with weird shapes.
If your image width or height is not a power of two, you need these
options:

    { magnify = linear
    , minify = nearest
    , horizontalWrap = clampToEdge
    , verticalWrap = clampToEdge
    , flipY = True
    }

-}
nonPowerOfTwoOptions : Options
nonPowerOfTwoOptions =
    { magnify = linear
    , minify = nearest
    , horizontalWrap = clampToEdge
    , verticalWrap = clampToEdge
    , flipY = True
    }



-- RESIZING


{-| How to resize a texture.
-}
type Resize a
    = Resize Int


{-| Returns the weighted average of the four texture elements that are closest
to the center of the pixel being textured.
-}
linear : Resize a
linear =
    Resize 9729


{-| Returns the value of the texture element that is nearest
(in Manhattan distance) to the center of the pixel being textured.
-}
nearest : Resize a
nearest =
    Resize 9728


{-| Chooses the mipmap that most closely matches the size of the pixel being
textured and uses the `nearest` criterion (the texture element nearest to
the center of the pixel) to produce a texture value.

A mipmap is an ordered set of arrays representing the same image at
progressively lower resolutions.

This is the default value of the minify filter.

-}
nearestMipmapNearest : Resize Smaller
nearestMipmapNearest =
    Resize 9984


{-| Chooses the mipmap that most closely matches the size of the pixel being
textured and uses the `linear` criterion (a weighted average of the four
texture elements that are closest to the center of the pixel) to produce a
texture value.
-}
linearMipmapNearest : Resize Smaller
linearMipmapNearest =
    Resize 9985


{-| Chooses the two mipmaps that most closely match the size of the pixel being
textured and uses the `nearest` criterion (the texture element nearest to the
center of the pixel) to produce a texture value from each mipmap. The final
texture value is a weighted average of those two values.
-}
nearestMipmapLinear : Resize Smaller
nearestMipmapLinear =
    Resize 9986


{-| Chooses the two mipmaps that most closely match the size of the pixel being
textured and uses the `linear` criterion (a weighted average of the four
texture elements that are closest to the center of the pixel) to produce a
texture value from each mipmap. The final texture value is a weighted average
of those two values.
-}
linearMipmapLinear : Resize Smaller
linearMipmapLinear =
    Resize 9987


{-| Helps restrict `options.magnify` to only allow
[`linear`](#linear) and [`nearest`](#nearest).
-}
type Bigger
    = Bigger


{-| Helps restrict `options.magnify`, while also allowing
`options.minify` to use mipmapping resizes, like
[`nearestMipmapNearest`](#nearestMipmapNearest).
-}
type Smaller
    = Smaller


{-| Sets the wrap parameter for texture coordinate.
-}
type Wrap
    = Wrap Int


{-| Causes the integer part of the coordinate to be ignored. This is the
default value for both texture axis.
-}
repeat : Wrap
repeat =
    Wrap 10497


{-| Causes coordinates to be clamped to the range 1 2N 1 - 1 2N, where N is
the size of the texture in the direction of clamping.
-}
clampToEdge : Wrap
clampToEdge =
    Wrap 33071


{-| Causes the coordinate c to be set to the fractional part of the texture
coordinate if the integer part is even; if the integer part is odd, then
the coordinate is set to 1 - frac, where frac represents the fractional part
of the coordinate.
-}
mirroredRepeat : Wrap
mirroredRepeat =
    Wrap 33648


{-| Return the (width, height) size of a texture. Useful for sprite sheets
or other times you may want to use only a potion of a texture image.
-}
size : Texture -> ( Int, Int )
size =
    Elm.Kernel.Texture.size
