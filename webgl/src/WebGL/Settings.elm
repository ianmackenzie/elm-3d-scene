module WebGL.Settings
    exposing
        ( FaceMode
        , Setting
        , back
        , colorMask
        , cullFace
        , front
        , frontAndBack
        , polygonOffset
        , sampleAlphaToCoverage
        , sampleCoverage
        , scissor
        )

{-|


# Settings

@docs Setting, scissor, colorMask, polygonOffset, sampleAlphaToCoverage
@docs sampleCoverage, cullFace


## Face Modes

@docs FaceMode, front, back, frontAndBack

-}

import WebGL.Internal as I


{-| Lets you customize how an [`Entity`](WebGL#Entity) is rendered. So if you
only want to see the red part of your entity, you would use
[`entityWith`](WebGL#entityWith) and [`colorMask`](#colorMask) to say:

    entityWith [colorMask True False False False]
        vertShader fragShader mesh uniforms

    -- vertShader : Shader attributes uniforms varyings
    -- fragShader : Shader {} uniforms varyings
    -- mesh : Mesh attributes
    -- uniforms : uniforms

-}
type alias Setting =
    I.Setting


{-| Set the scissor box, which limits the drawing of fragments to the
screen to a specified rectangle.

The arguments are the coordinates of the lower left corner, width and height.

-}
scissor : Int -> Int -> Int -> Int -> Setting
scissor =
    I.Scissor


{-| Specify whether or not each channel (red, green, blue, alpha) should be
output on the screen.
-}
colorMask : Bool -> Bool -> Bool -> Bool -> Setting
colorMask =
    I.ColorMask


{-| When you want to draw the highlighting wireframe on top of the solid
object, the lines may fade in and out of the coincident polygons,
which is sometimes called "stitching" and is visually unpleasant.

[Polygon Offset](https://www.glprogramming.com/red/chapter06.html#name4)
helps to avoid "stitching" by adding an offset to pixel’s depth
values before the depth test is performed and before the value is written
into the depth buffer.

    polygonOffset factor units

This adds an `offset = m * factor + r * units`, where

  - `m = max (dz / dx) (dz / dy)` is the maximum depth slope of the polygon.
    The depth slope is the change in `z` (depth) values divided by the change in
    either `x` or `y` coordinates, as you traverse a polygon;
  - `r` is the smallest value guaranteed to produce a resolvable difference in
    window coordinate depth values. The value `r` is an implementation-specific
    constant.

The question is: "How much offset is enough?". It really depends on the slope.
For polygons that are parallel to the near and far clipping planes,
the depth slope is zero, so the minimum offset is needed:

    polygonOffset 0 1

For polygons that are at a great angle to the clipping planes, the depth slope
can be significantly greater than zero. Use small non-zero values for factor,
such as `0.75` or `1.0` should be enough to generate distinct depth values:

    polygonOffset 0.75 1

-}
polygonOffset : Float -> Float -> Setting
polygonOffset =
    I.PolygonOffset


{-| When you render overlapping transparent entities, like grass or hair, you
may notice that alpha blending doesn’t really work with depth testing, because
depth test ignores transparency.
[Alpha To Coverage](https://wiki.polycount.com/wiki/Transparency_map#Alpha_To_Coverage)
is a way to address this issue without sorting transparent entities.

It works by computing a temporary coverage value, where each bit is determined
by the alpha value at the corresponding sample location. The temporary coverage
value is then ANDed with the fragment coverage value.

Requires [`WebGL.antialias`](WebGL#antialias) option.

-}
sampleAlphaToCoverage : Setting
sampleAlphaToCoverage =
    I.SampleAlphaToCoverage


{-| Specifies multisample coverage parameters. The fragment's coverage is ANDed
with the temporary coverage value.

  - the first argument specifies sample coverage value, that is clamped to the
    range from 0 to 1;
  - the second argument represents if the coverage masks should be inverted.

Requires [`WebGL.antialias`](WebGL#antialias) option.

-}
sampleCoverage : Float -> Bool -> Setting
sampleCoverage =
    I.SampleCoverage


{-| Excludes polygons based on winding (the order of the vertices) in window
coordinates. Polygons with counter-clock-wise winding are front-facing.
-}
cullFace : FaceMode -> Setting
cullFace (FaceMode faceMode) =
    I.CullFace faceMode


{-| Targets the polygons based on their facing.
-}
type FaceMode
    = FaceMode Int


{-| -}
front : FaceMode
front =
    FaceMode 1028


{-| -}
back : FaceMode
back =
    FaceMode 1029


{-| -}
frontAndBack : FaceMode
frontAndBack =
    FaceMode 1032
