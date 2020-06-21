module Intersection exposing (main)

{-
   Draws a red and a green triangles, where the green triangle is only
   visible in the intercection with the red triangle.
   A green outline marks a hidden area of the green triangle.

   This example helps to understand the separate stencil test.
-}

import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Mesh, Shader)
import WebGL.Settings exposing (Setting)
import WebGL.Settings.StencilTest as StencilTest


main : Html ()
main =
    WebGL.toHtmlWith
        [ WebGL.stencil 0 ]
        [ width 400
        , height 400
        , style "display" "block"
        ]
        [ WebGL.entityWith
            [ stencilTest ]
            vertexShader
            fragmentShader
            redAndGreenTriangles
            {}
        , WebGL.entity
            vertexShader
            fragmentShader
            greenOutline
            {}
        ]


{-| Rendering with the following setting will write 1's to the stencil buffer
for all front-facing triangles, and then test the subsequent back-facing
triangles against the stencil buffer, so they be rendered only for the areas
that are marked with 1's in the stencil buffer.

`StencilTest.testSeparate` takes two options, one for front-, and another for
back-facing triangles:

  - The front-facing stencil test always passes, and replaces the stencil buffer
    with 1.
  - The back-facing stencil test only passes when the value in the stencil buffer
    is equal to 1. It does not modify the stencil buffer.

-}
stencilTest : Setting
stencilTest =
    StencilTest.testSeparate
        { ref = 1
        , mask = 0xFF
        , writeMask = 0xFF
        }
        { test = StencilTest.always
        , fail = StencilTest.keep
        , zfail = StencilTest.keep
        , zpass = StencilTest.replace
        }
        { test = StencilTest.equal
        , fail = StencilTest.keep
        , zfail = StencilTest.keep
        , zpass = StencilTest.keep
        }



-- Mesh


type alias Vertex =
    { position : Vec3
    , color : Vec3
    }


redAndGreenTriangles : Mesh Vertex
redAndGreenTriangles =
    let
        -- the red triangle is front-facing
        redTriangle =
            ( Vertex (vec3 -1 0.5 0) (vec3 1 0 0)
            , Vertex (vec3 0 -0.5 0) (vec3 1 0 0)
            , Vertex (vec3 1 0.5 0) (vec3 1 0 0)
            )

        -- the green triangle is back-facing
        greenTriangle =
            ( Vertex (vec3 -1 -0.5 0) (vec3 0 1 0)
            , Vertex (vec3 0 0.5 0) (vec3 0 1 0)
            , Vertex (vec3 1 -0.5 0) (vec3 0 1 0)
            )
    in
    WebGL.triangles
        [ redTriangle
        , greenTriangle
        ]


greenOutline : Mesh Vertex
greenOutline =
    WebGL.lineLoop
        [ Vertex (vec3 -1 -0.5 0) (vec3 0 1 0)
        , Vertex (vec3 0 0.5 0) (vec3 0 1 0)
        , Vertex (vec3 1 -0.5 0) (vec3 0 1 0)
        ]



-- Shaders


vertexShader : Shader Vertex {} { vcolor : Vec3 }
vertexShader =
    [glsl|

        attribute vec3 position;
        attribute vec3 color;
        varying vec3 vcolor;

        void main () {
            gl_Position = vec4(position, 1.0);
            vcolor = color;
        }

    |]


fragmentShader : Shader {} {} { vcolor : Vec3 }
fragmentShader =
    [glsl|

        precision mediump float;
        varying vec3 vcolor;

        void main () {
            gl_FragColor = vec4(vcolor, 1.0);
        }

    |]
