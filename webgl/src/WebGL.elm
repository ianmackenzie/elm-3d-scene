module WebGL exposing
    ( Mesh, triangles
    , Shader
    , Entity, entity
    , toHtml
    , entityWith, toHtmlWith, Option, alpha, depth, stencil, antialias
    , clearColor, preserveDrawingBuffer
    , indexedTriangles, lines, lineStrip, lineLoop, points, triangleFan
    , triangleStrip
    )

{-| The WebGL API is for high performance rendering. Definitely read about
[how WebGL works](https://package.elm-lang.org/packages/elm-explorations/webgl/latest)
and look at [some examples](https://github.com/elm-explorations/webgl/tree/master/examples)
before trying to do too much with just the documentation provided here.


# Mesh

@docs Mesh, triangles


# Shaders

@docs Shader


# Entities

@docs Entity, entity


# WebGL Html

@docs toHtml


# Advanced Usage

@docs entityWith, toHtmlWith, Option, alpha, depth, stencil, antialias
@docs clearColor, preserveDrawingBuffer


# Meshes

@docs indexedTriangles, lines, lineStrip, lineLoop, points, triangleFan
@docs triangleStrip

-}

import Elm.Kernel.WebGL
import Html exposing (Attribute, Html)
import WebGL.Internal as I
import WebGL.Settings as Settings exposing (Setting)
import WebGL.Settings.DepthTest as DepthTest


{-| Mesh forms geometry from the specified vertices. Each vertex contains a
bunch of attributes, defined as a custom record type, e.g.:

    type alias Attributes =
        { position : Vec3
        , color : Vec3
        }

The supported types in attributes are: `Int`, `Float`, `Texture`
and `Vec2`, `Vec3`, `Vec4`, `Mat4` from the
[linear-algebra](https://package.elm-lang.org/packages/elm-explorations/linear-algebra/latest)
package.

Do not generate meshes in `view`, [read more about this here](https://package.elm-lang.org/packages/elm-explorations/webgl/latest#making-the-most-of-the-gpu).

-}
type Mesh attributes
    = Mesh1 RenderInfo (List attributes)
    | Mesh2 RenderInfo (List ( attributes, attributes ))
    | Mesh3 RenderInfo (List ( attributes, attributes, attributes ))
    | MeshIndexed3 RenderInfo (List attributes) (List ( Int, Int, Int ))


type alias RenderInfo =
    { mode : Int
    , elemSize : Int
    , indexSize : Int
    }


{-| Triangles are the basic building blocks of a mesh. You can put them together
to form any shape.

So when you create `triangles` you are really providing three sets of attributes
that describe the corners of each triangle.

-}
triangles : List ( attributes, attributes, attributes ) -> Mesh attributes
triangles =
    Mesh3 { mode = 0x04, elemSize = 3, indexSize = 0 }


{-| Creates a strip of triangles where each additional vertex creates an
additional triangle once the first three vertices have been drawn.
-}
triangleStrip : List attributes -> Mesh attributes
triangleStrip =
    Mesh1 { mode = 0x05, elemSize = 1, indexSize = 0 }


{-| Similar to [`triangleStrip`](#triangleStrip), but creates a fan shaped
output.
-}
triangleFan : List attributes -> Mesh attributes
triangleFan =
    Mesh1 { mode = 0x06, elemSize = 1, indexSize = 0 }


{-| Create triangles from vertices and indices, grouped in sets of three to
define each triangle by refering the vertices. This helps to avoid duplicated vertices whenever two triangles share an
edge.

    -- v2 +---+ v1
    --    |\  |
    --    | \ |
    --    |  \|
    -- v3 +---+ v0



For example, if you want to define a rectangle using
[`triangles`](#triangles), `v0` and `v2` will have to be duplicated:

    rectangle =
        triangles [ ( v0, v1, v2 ), ( v2, v3, v0 ) ]

This will use two vertices less:

    rectangle =
        indexedTriangles [ v0, v1, v2, v3 ] [ ( 0, 1, 2 ), ( 2, 3, 0 ) ]

-}
indexedTriangles : List attributes -> List ( Int, Int, Int ) -> Mesh attributes
indexedTriangles =
    MeshIndexed3 { mode = 0x04, elemSize = 1, indexSize = 3 }


{-| Connects each pair of vertices with a line.
-}
lines : List ( attributes, attributes ) -> Mesh attributes
lines =
    Mesh2 { mode = 0x01, elemSize = 2, indexSize = 0 }


{-| Connects each two subsequent vertices with a line.
-}
lineStrip : List attributes -> Mesh attributes
lineStrip =
    Mesh1 { mode = 0x03, elemSize = 1, indexSize = 0 }


{-| Similar to [`lineStrip`](#lineStrip), but connects the last vertex back to
the first.
-}
lineLoop : List attributes -> Mesh attributes
lineLoop =
    Mesh1 { mode = 0x02, elemSize = 1, indexSize = 0 }


{-| Draws a single dot per vertex.
-}
points : List attributes -> Mesh attributes
points =
    Mesh1 { mode = 0x00, elemSize = 1, indexSize = 0 }


{-| Shaders are programs for running many computations on the GPU in parallel.
They are written in a language called
[GLSL](https://en.wikipedia.org/wiki/OpenGL_Shading_Language). Read more about
shaders [here](https://github.com/elm-explorations/webgl/blob/master/README.md).

Normally you specify a shader with a `[glsl| |]` block. Elm compiler will parse
the shader code block and derive the type signature for your shader.

  - `attributes` define vertices in the [mesh](#Mesh);
  - `uniforms` allow you to pass scene parameters like
    transformation matrix, texture, screen size, etc.;
  - `varyings` define the output from the vertex shader.

`attributes`, `uniforms` and `varyings` are records with the fields of the
following types: `Int`, `Float`, [`Texture`](#Texture) and `Vec2`, `Vec3`, `Vec4`,
`Mat4` from the
[linear-algebra](https://package.elm-lang.org/packages/elm-explorations/linear-algebra/latest)
package.

-}
type Shader attributes uniforms varyings
    = Shader


{-| Conceptually, an encapsulation of the instructions to render something.
-}
type Entity
    = Entity


{-| Packages a vertex shader, a fragment shader, a mesh, and uniforms
as an `Entity`. This specifies a full rendering pipeline to be run
on the GPU. You can read more about the pipeline
[here](https://github.com/elm-explorations/webgl/blob/master/README.md).

The vertex shader receives `attributes` and `uniforms` and returns `varyings`
and `gl_Position`—the position of the vertex on the screen, defined as
`vec4(x, y, z, w)`, that means `(x/w, y/w, z/w)` in the clip space coordinates:

    --   (-1,1,1) +================+ (1,1,1)
    --           /|               /|
    --          / |     |        / |
    --(-1,1,-1)+================+ (1,1,-1)
    --         |  |     | /     |  |
    --         |  |     |/      |  |
    --         |  |     +-------|->|
    -- (-1,-1,1|) +--(0,0,0)----|--+ (1,-1,1)
    --         | /              | /
    --         |/               |/
    --         +================+
    --   (-1,-1,-1)         (1,-1,-1)



The fragment shader is called for each pixel inside the clip space with
`varyings` and `uniforms` and returns `gl_FragColor`—the color of
the pixel, defined as `vec4(r, g, b, a)` where each color component is a float
from 0 to 1.

Shaders and a mesh are cached so that they do not get resent to the GPU.
It should be relatively cheap to create new entities out of existing
values.

By default, [depth test](WebGL-Settings-DepthTest#default) is enabled for you.
If you need more [settings](WebGL-Settings), like
[blending](WebGL-Settings-Blend) or [stencil test](WebG-Settings-StencilTest),
then use [`entityWith`](#entityWith).

    entity =
        entityWith [ DepthTest.default ]

-}
entity :
    Shader attributes uniforms varyings
    -> Shader {} uniforms varyings
    -> Mesh attributes
    -> uniforms
    -> Entity
entity =
    entityWith [ DepthTest.default ]


{-| The same as [`entity`](#entity), but allows to configure an entity with
[settings](WebGL-Settings).
-}
entityWith :
    List Setting
    -> Shader attributes uniforms varyings
    -> Shader {} uniforms varyings
    -> Mesh attributes
    -> uniforms
    -> Entity
entityWith =
    Elm.Kernel.WebGL.entity


{-| Render a WebGL scene with the given html attributes, and entities.

`width` and `height` html attributes resize the drawing buffer, while
the corresponding css properties scale the canvas element.

To prevent blurriness on retina screens, you may want the drawing buffer
to be twice the size of the canvas element.

To remove an extra whitespace around the canvas, set `display: block`.

By default, alpha channel with premultiplied alpha, antialias and depth buffer
are enabled. Use [`toHtmlWith`](#toHtmlWith) for custom options.

    toHtml =
        toHtmlWith [ alpha True, antialias, depth 1 ]

-}
toHtml : List (Attribute msg) -> List Entity -> Html msg
toHtml =
    toHtmlWith [ alpha True, antialias, depth 1 ]


{-| Render a WebGL scene with the given options, html attributes, and entities.

Due to browser limitations, options will be applied only once,
when the canvas is created for the first time.

-}
toHtmlWith : List Option -> List (Attribute msg) -> List Entity -> Html msg
toHtmlWith options attributes entities =
    Elm.Kernel.WebGL.toHtml options attributes entities


{-| Provides a way to enable features and change the scene behavior
in [`toHtmlWith`](#toHtmlWith).
-}
type alias Option =
    I.Option


{-| Enable alpha channel in the drawing buffer. If the argument is `True`, then
the page compositor will assume the drawing buffer contains colors with
premultiplied alpha `(r * a, g * a, b * a, a)`.
-}
alpha : Bool -> Option
alpha =
    I.Alpha


{-| Enable the depth buffer, and prefill it with given value each time before
the scene is rendered. The value is clamped between 0 and 1.
-}
depth : Float -> Option
depth =
    I.Depth


{-| Enable the stencil buffer, specifying the index used to fill the
stencil buffer before we render the scene. The index is masked with 2^m - 1,
where m >= 8 is the number of bits in the stencil buffer. The default is 0.
-}
stencil : Int -> Option
stencil =
    I.Stencil


{-| Enable multisample antialiasing of the drawing buffer, if supported by
the platform. Useful when you need to have smooth lines and smooth edges of
triangles at a lower cost than supersampling (rendering to larger dimensions and
then scaling down with CSS transform).
-}
antialias : Option
antialias =
    I.Antialias


{-| Set the red, green, blue and alpha channels, that will be used to
fill the drawing buffer every time before drawing the scene. The values are
clamped between 0 and 1. The default is all 0's.
-}
clearColor : Float -> Float -> Float -> Float -> Option
clearColor =
    I.ClearColor


{-| By default, WebGL canvas swaps the drawing and display buffers.
This option forces it to copy the drawing buffer into the display buffer.

Even though this slows down the rendering, it allows you to extract an image
from the canvas element using `canvas.toBlob()` in JavaScript without having
to worry about synchronization between frames.

-}
preserveDrawingBuffer : Option
preserveDrawingBuffer =
    I.PreserveDrawingBuffer
