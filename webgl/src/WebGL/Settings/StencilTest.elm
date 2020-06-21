module WebGL.Settings.StencilTest
    exposing
        ( Operation
        , Test
        , always
        , decrement
        , decrementWrap
        , equal
        , greater
        , greaterOrEqual
        , increment
        , incrementWrap
        , invert
        , keep
        , less
        , lessOrEqual
        , never
        , notEqual
        , replace
        , test
        , testSeparate
        , zero
        )

{-| You can read more about stencil-testing in the
[OpenGL wiki](https://www.khronos.org/opengl/wiki/Stencil_Test)
or [OpenGL docs](https://www.opengl.org/sdk/docs/man2/xhtml/glStencilFunc.xml).


# Stencil Test

@docs test


## Tests

@docs Test, always, equal, never, less, greater, notEqual
@docs lessOrEqual, greaterOrEqual


## Operations

@docs Operation, replace, keep, zero, increment, decrement, invert
@docs incrementWrap, decrementWrap


# Separate Test

@docs testSeparate

-}

import WebGL.Internal as I
import WebGL.Settings exposing (Setting)


{-| When you need to draw an intercection of two entities, e.g. a reflection in
the mirror, you can test against the stencil buffer, that has to be enabled
with [`stencil`](WebGL#stencil) option in [`toHtmlWith`](WebGL#toHtmlWith).

Stencil test decides if the pixel should be drawn on the screen.
Depending on the results, it performs one of the following
[operations](#Operation) on the stencil buffer:

  - `fail`—the operation to use when the stencil test fails;
  - `zfail`—the operation to use when the stencil test passes, but the depth
    test fails;
  - `zpass`—the operation to use when both the stencil test and the depth test
    pass, or when the stencil test passes and there is no depth buffer or depth
    testing is disabled.

For example, draw the mirror `Entity` on the screen and fill the stencil buffer
with all 1's:

    test
        { ref = 1
        , mask = 0xFF
        , test = always    -- pass for each pixel
        , fail = keep      -- noop
        , zfail = keep     -- noop
        , zpass = replace  -- write ref to the stencil buffer
        , writeMask = 0xFF -- enable all stencil bits for writing
        }

Crop the reflection `Entity` using the values from the stencil buffer:

    test
        { ref = 1
        , mask = 0xFF
        , test = equal  -- pass when the stencil value is equal to ref = 1
        , fail = keep   -- noop
        , zfail = keep  -- noop
        , zpass = keep  -- noop
        , writeMask = 0 -- disable writing to the stencil buffer
        }

You can see the complete example
[here](https://github.com/elm-explorations/webgl/blob/master/examples/crate.elm).

-}
test :
    { ref : Int
    , mask : Int
    , test : Test
    , fail : Operation
    , zfail : Operation
    , zpass : Operation
    , writeMask : Int
    }
    -> Setting
test stencilTest =
    testSeparate
        { ref = stencilTest.ref, mask = stencilTest.mask, writeMask = stencilTest.writeMask }
        { test = stencilTest.test, fail = stencilTest.fail, zfail = stencilTest.zfail, zpass = stencilTest.zpass }
        { test = stencilTest.test, fail = stencilTest.fail, zfail = stencilTest.zfail, zpass = stencilTest.zpass }


{-| The `Test` allows you to define how to compare the reference value
with the stencil buffer value, in order to set the conditions under which
the pixel will be drawn.

    always         -- Always pass
    equal          -- ref & mask == stencil & mask
    never          -- Never pass
    less           -- ref & mask < stencil & mask
    greater        -- ref & mask > stencil & mask
    notEqual       -- ref & mask != stencil & mask
    lessOrEqual    -- ref & mask <= stencil & mask
    greaterOrEqual -- ref & mask >= stencil & mask

-}
type Test
    = Test Int


{-| -}
always : Test
always =
    Test 519


{-| -}
equal : Test
equal =
    Test 514


{-| -}
never : Test
never =
    Test 512


{-| -}
less : Test
less =
    Test 513


{-| -}
greater : Test
greater =
    Test 516


{-| -}
notEqual : Test
notEqual =
    Test 517


{-| -}
lessOrEqual : Test
lessOrEqual =
    Test 515


{-| -}
greaterOrEqual : Test
greaterOrEqual =
    Test 518


{-| Defines how to update the value in the stencil buffer.
-}
type Operation
    = Operation Int


{-| Sets the stencil buffer value to `ref` from the stencil test.
-}
replace : Operation
replace =
    Operation 7681


{-| Keeps the current stencil buffer value. Use this as a noop.
-}
keep : Operation
keep =
    Operation 7680


{-| Sets the stencil buffer value to 0.
-}
zero : Operation
zero =
    Operation 0


{-| Increments the current stencil buffer value. Clamps to the maximum
representable unsigned value.
-}
increment : Operation
increment =
    Operation 7682


{-| Decrements the current stencil buffer value. Clamps to 0.
-}
decrement : Operation
decrement =
    Operation 7683


{-| Bitwise inverts the current stencil buffer value.
-}
invert : Operation
invert =
    Operation 5386


{-| Increments the current stencil buffer value. Wraps stencil buffer value to
zero when incrementing the maximum representable unsigned value.
-}
incrementWrap : Operation
incrementWrap =
    Operation 34055


{-| Decrements the current stencil buffer value.
Wraps stencil buffer value to the maximum representable unsigned
value when decrementing a stencil buffer value of zero.
-}
decrementWrap : Operation
decrementWrap =
    Operation 34056


{-| Different options for front and back facing polygons.
-}
testSeparate :
    { ref : Int, mask : Int, writeMask : Int }
    -> { test : Test, fail : Operation, zfail : Operation, zpass : Operation }
    -> { test : Test, fail : Operation, zfail : Operation, zpass : Operation }
    -> Setting
testSeparate { ref, mask, writeMask } options1 options2 =
    let
        expandTest (Test expandedTest) fn =
            fn expandedTest

        expandOp (Operation op) fn =
            fn op

        expand options =
            expandTest options.test
                >> expandOp options.fail
                >> expandOp options.zfail
                >> expandOp options.zpass
    in
        I.StencilTest ref mask writeMask
            |> expand options1
            |> expand options2
