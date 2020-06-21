module WebGL.Settings.DepthTest
    exposing
        ( Options
        , always
        , default
        , equal
        , greater
        , greaterOrEqual
        , less
        , lessOrEqual
        , never
        , notEqual
        )

{-| You can read more about depth-testing in the
[OpenGL wiki](https://www.khronos.org/opengl/wiki/Depth_Test)
or [OpenGL docs](https://www.opengl.org/sdk/docs/man2/xhtml/glDepthFunc.xml).


# Depth Test

@docs default


# Custom Tests

@docs Options, less, never, always, equal, greater, notEqual
@docs lessOrEqual, greaterOrEqual

-}

import WebGL.Internal as I
import WebGL.Settings exposing (Setting)


{-| With every pixel, we have to figure out which color to show.

Imagine you have many entities in the same line of sight. The floor,
then a table, then a plate. When depth-testing is off, you go through
the entities in the order they appear in your *code*! That means if
you describe the floor last, it will be “on top” of the table and plate.

Depth-testing means the color is chosen based on the distance from the
camera. So `default` uses the color closest to the camera. This means
the plate will be on top of the table, and both are on top of the floor.
Seems more reasonable!

There are a bunch of ways you can customize the depth test, shown later,
and you can use them to define `default` like this:

    default =
        less { write = True, near = 0, far = 1 }

Requires [`WebGL.depth`](WebGL#depth) option in
[`toHtmlWith`](WebGL#toHtmlWith).

-}
default : Setting
default =
    less { write = True, near = 0, far = 1 }


{-| When rendering, you have a buffer of pixels. Depth-testing works by
creating a second buffer with exactly the same number of entries, but
instead of holding colors, each entry holds the distance from the camera.
You go through all your entities, writing into the depth buffer, and then
you draw the color of the “winner”.

Which color wins? This is based on a bunch of comparison functions:

    less options           -- value < depth
    never options          -- Never pass
    always options         -- Always pass
    equal options          -- value == depth
    greater options        -- value > depth
    notEqual options       -- value != depth
    lessOrEqual options    -- value <= depth
    greaterOrEqual options -- value >= depth

If the test passes, the current value will be written into the depth buffer, so
the next pixels will be tested against it. Sometimes you may want to disable
writing. For example, when using depth test together with stencil test to create
[reflection effect](https://open.gl/depthstencils) you want to draw the
reflection *underneath* the floor, in this case you set `write = False`
when drawing the floor. The
[crate example](https://github.com/elm-explorations/webgl/blob/master/examples/crate.elm)
shows how to do it in Elm.

`near` and `far` allow to allocate a portion of the depth range from 0 to 1.
For example, if you want to render GUI on top of the scene, you can
set `near = 0.1, far = 1` for the scene and then render the GUI with
`near = 0, far = 0.1`.

-}
type alias Options =
    { write : Bool
    , near : Float
    , far : Float
    }


{-| -}
less : Options -> Setting
less { write, near, far } =
    I.DepthTest 513 write near far


{-| -}
never : Options -> Setting
never { write, near, far } =
    I.DepthTest 512 write near far


{-| -}
always : Options -> Setting
always { write, near, far } =
    I.DepthTest 519 write near far


{-| -}
equal : Options -> Setting
equal { write, near, far } =
    I.DepthTest 514 write near far


{-| -}
greater : Options -> Setting
greater { write, near, far } =
    I.DepthTest 516 write near far


{-| -}
notEqual : Options -> Setting
notEqual { write, near, far } =
    I.DepthTest 517 write near far


{-| -}
lessOrEqual : Options -> Setting
lessOrEqual { write, near, far } =
    I.DepthTest 515 write near far


{-| -}
greaterOrEqual : Options -> Setting
greaterOrEqual { write, near, far } =
    I.DepthTest 518 write near far
