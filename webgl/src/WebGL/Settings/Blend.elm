module WebGL.Settings.Blend
    exposing
        ( Blender
        , Factor
        , add
        , constantAlpha
        , constantColor
        , custom
        , customAdd
        , customReverseSubtract
        , customSubtract
        , dstAlpha
        , dstColor
        , one
        , oneMinusConstantAlpha
        , oneMinusConstantColor
        , oneMinusDstAlpha
        , oneMinusDstColor
        , oneMinusSrcAlpha
        , oneMinusSrcColor
        , reverseSubtract
        , srcAlpha
        , srcAlphaSaturate
        , srcColor
        , subtract
        , zero
        )

{-|


# Blenders

@docs add, subtract, reverseSubtract


# Blend Factors

@docs Factor, zero, one, srcColor, oneMinusSrcColor, dstColor
@docs oneMinusDstColor, srcAlpha, oneMinusSrcAlpha, dstAlpha
@docs oneMinusDstAlpha, srcAlphaSaturate


# Custom Blenders

@docs custom, Blender, customAdd, customSubtract, customReverseSubtract
@docs constantColor, oneMinusConstantColor, constantAlpha
@docs oneMinusConstantAlpha

-}

import WebGL.Internal as I
import WebGL.Settings exposing (Setting)


{-| Add the color of the current `Renderable` (the source color)
with whatever is behind it (the destination color). For example,
here is the “default” blender:

    add one zero

The resulting color will be `(src * 1) + (dest * 0)`, which means
we do not use the destination color at all!
You can get a feel for all the different blending factors
[here](https://threejs.org/examples/webgl_materials_blending_custom.html).

-}
add : Factor -> Factor -> Setting
add factor1 factor2 =
    custom
        { r = 0
        , g = 0
        , b = 0
        , a = 0
        , color = customAdd factor1 factor2
        , alpha = customAdd factor1 factor2
        }


{-| Similar to [`add`](#add), but it does `(src * factor1) - (dest * factor2)`.
For example:

    subtract one one

This would do `(src * 1) - (dest * 1)` so you would take away colors
based on the background.

-}
subtract : Factor -> Factor -> Setting
subtract factor1 factor2 =
    custom
        { r = 0
        , g = 0
        , b = 0
        , a = 0
        , color = customSubtract factor1 factor2
        , alpha = customSubtract factor1 factor2
        }


{-| Similar to [`add`](#add), but it does `(dest * factor2) - (src * factor1)`.
This one is weird.
-}
reverseSubtract : Factor -> Factor -> Setting
reverseSubtract factor1 factor2 =
    custom
        { r = 0
        , g = 0
        , b = 0
        , a = 0
        , color = customReverseSubtract factor1 factor2
        , alpha = customReverseSubtract factor1 factor2
        }


{-| -}
type Factor
    = Factor Int


{-| -}
zero : Factor
zero =
    Factor 0


{-| -}
one : Factor
one =
    Factor 1


{-| -}
srcColor : Factor
srcColor =
    Factor 768


{-| -}
oneMinusSrcColor : Factor
oneMinusSrcColor =
    Factor 769


{-| -}
dstColor : Factor
dstColor =
    Factor 774


{-| -}
oneMinusDstColor : Factor
oneMinusDstColor =
    Factor 775


{-| -}
srcAlpha : Factor
srcAlpha =
    Factor 770


{-| -}
oneMinusSrcAlpha : Factor
oneMinusSrcAlpha =
    Factor 771


{-| -}
dstAlpha : Factor
dstAlpha =
    Factor 772


{-| -}
oneMinusDstAlpha : Factor
oneMinusDstAlpha =
    Factor 773


{-| -}
srcAlphaSaturate : Factor
srcAlphaSaturate =
    Factor 776



-- BLENDING WITH CONSTANT COLORS


{-| It is possible to do some very fancy blending with
`custom`. For example, you can blend the color value and
the alpha values separately:

    myBlender : Float -> Setting
    myBlender alpha =
        custom
            { r = 0
            , g = 0
            , b = 0
            , a = alpha
            , color = customAdd one zero
            , alpha = customAdd one constantAlpha
            }

-}
custom :
    { r : Float
    , g : Float
    , b : Float
    , a : Float
    , color : Blender
    , alpha : Blender
    }
    -> Setting
custom { r, g, b, a, color, alpha } =
    let
        expand (Blender eq1 f11 f12) (Blender eq2 f21 f22) =
            I.Blend eq1 f11 f12 eq2 f21 f22 r g b a
    in
    expand color alpha


{-| A `Blender` mixes the color of the current `Entity` (the source color)
with whatever is behind it (the destination color).
You can get a feel for all the options [here](https://threejs.org/examples/webgl_materials_blending_custom.html).
-}
type Blender
    = Blender Int Int Int


{-| -}
customAdd : Factor -> Factor -> Blender
customAdd (Factor factor1) (Factor factor2) =
    Blender 32774 factor1 factor2


{-| -}
customSubtract : Factor -> Factor -> Blender
customSubtract (Factor factor1) (Factor factor2) =
    Blender 32778 factor1 factor2


{-| -}
customReverseSubtract : Factor -> Factor -> Blender
customReverseSubtract (Factor factor1) (Factor factor2) =
    Blender 32779 factor1 factor2


{-| This uses the constant `r`, `g`, `b`, and `a` values
given to [`custom`](#custom). If you use this `Factor` with
[`add`](#add), the constant color will default to black.

Because of
[restriction in WebGL](https://www.khronos.org/registry/webgl/specs/latest/1.0/#6.13),
you cannot create a `Blender`, that has one factor set to
`constantColor` or `oneMinusConstantColor` and another set to
`constantAlpha` or `oneMinusConstantAlpha`.

-}
constantColor : Factor
constantColor =
    Factor 32769


{-| -}
oneMinusConstantColor : Factor
oneMinusConstantColor =
    Factor 32770


{-| -}
constantAlpha : Factor
constantAlpha =
    Factor 32771


{-| -}
oneMinusConstantAlpha : Factor
oneMinusConstantAlpha =
    Factor 32772
