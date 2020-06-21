module Crate exposing (main)

{-
   This example was inspired by https://open.gl/depthstencils
   It demonstrates how to use the stencil buffer.
-}

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Task
import WebGL exposing (Mesh, Shader, Entity)
import WebGL.Settings.Blend as Blend
import WebGL.Settings.DepthTest as DepthTest
import WebGL.Settings.StencilTest as StencilTest
import WebGL.Texture as Texture exposing (Error, Texture)
import Json.Decode exposing (Value)


type alias Model =
    { texture : Maybe Texture
    , theta : Float
    }


type Msg
    = TextureLoaded (Result Error Texture)
    | Animate Float


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        TextureLoaded textureResult ->
            ( { model | texture = Result.toMaybe textureResult }, Cmd.none )

        Animate dt ->
            ( { model | theta = model.theta + dt / 10000 }, Cmd.none )


init : ( Model, Cmd Msg )
init =
    ( { texture = Nothing, theta = 0 }
    , Task.attempt TextureLoaded (Texture.load "texture/wood-crate.jpg")
    )


main : Program Value Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , subscriptions = (\model -> onAnimationFrameDelta Animate)
        , update = update
        }



-- View


view : Model -> Html Msg
view { texture, theta } =
    WebGL.toHtmlWith
        [ WebGL.alpha True
        , WebGL.antialias
        , WebGL.depth 1
        , WebGL.stencil 0
        ]
        [ width 400
        , height 400
        , style "display" "block"
        ]
        (texture
            |> Maybe.map (scene (perspective theta))
            |> Maybe.withDefault []
        )


perspective : Float -> Mat4
perspective angle =
    List.foldr Mat4.mul
        Mat4.identity
        [ Mat4.makePerspective 45 1 0.01 100
        , Mat4.makeLookAt (vec3 0 3 8) (vec3 0 0 0) (vec3 0 1 0)
        , Mat4.makeRotate (3 * angle) (vec3 0 1 0)
        ]


scene : Mat4 -> Texture -> List Entity
scene camera texture =
    [ WebGL.entity
        crateVertex
        crateFragment
        crateMesh
        { texture = texture
        , perspective = camera
        }
    , WebGL.entityWith
        [ DepthTest.less
            { write = False
            , near = 0
            , far = 1
            }
        , StencilTest.test
            { ref = 1
            , mask = 0xFF
            , test = StencilTest.always
            , fail = StencilTest.keep
            , zfail = StencilTest.keep
            , zpass = StencilTest.replace
            , writeMask = 0xFF
            }
        ]
        floorVertex
        floorFragment
        floorMesh
        { texture = texture
        , perspective = camera
        }
    , WebGL.entityWith
        [ StencilTest.test
            { ref = 1
            , mask = 0xFF
            , test = StencilTest.equal
            , fail = StencilTest.keep
            , zfail = StencilTest.keep
            , zpass = StencilTest.keep
            , writeMask = 0
            }
        , DepthTest.default
        , Blend.custom
            { r = 0
            , g = 0
            , b = 0
            , a = 0.5
            , color = Blend.customAdd Blend.constantAlpha Blend.zero
            , alpha = Blend.customAdd Blend.one Blend.zero
            }
        ]
        crateVertex
        crateFragment
        crateMesh
        { texture = texture
        , perspective =
            Mat4.mul camera (Mat4.makeScale (vec3 1 -1 1))
        }
    ]



-- Meshes


type alias Vertex =
    { position : Vec3
    , coord : Vec2
    }


crateMesh : Mesh Vertex
crateMesh =
    [ ( 0, 0 ), ( 90, 0 ), ( 180, 0 ), ( 270, 0 ), ( 0, 90 ), ( 0, 270 ) ]
        |> List.concatMap rotatedFace
        |> WebGL.triangles


rotatedFace : ( Float, Float ) -> List ( Vertex, Vertex, Vertex )
rotatedFace ( angleXZ, angleYZ ) =
    let
        transformMat =
            List.foldr Mat4.mul
                Mat4.identity
                [ Mat4.makeTranslate (vec3 0 1 0)
                , Mat4.makeRotate (degrees angleXZ) Vec3.j
                , Mat4.makeRotate (degrees angleYZ) Vec3.i
                , Mat4.makeTranslate (vec3 0 0 1)
                ]

        transform vertex =
            { vertex
                | position =
                    Mat4.transform
                        transformMat
                        vertex.position
            }

        transformTriangle ( a, b, c ) =
            ( transform a, transform b, transform c )
    in
        List.map transformTriangle square


square : List ( Vertex, Vertex, Vertex )
square =
    let
        topLeft =
            { position = vec3 -1 1 0, coord = vec2 0 1 }

        topRight =
            { position = vec3 1 1 0, coord = vec2 1 1 }

        bottomLeft =
            { position = vec3 -1 -1 0, coord = vec2 0 0 }

        bottomRight =
            { position = vec3 1 -1 0, coord = vec2 1 0 }
    in
        [ ( topLeft, topRight, bottomLeft )
        , ( bottomLeft, topRight, bottomRight )
        ]


floorMesh : Mesh { position : Vec3 }
floorMesh =
    let
        topLeft =
            { position = vec3 -2 0 -2 }

        topRight =
            { position = vec3 2 0 -2 }

        bottomLeft =
            { position = vec3 -2 0 2 }

        bottomRight =
            { position = vec3 2 0 2 }
    in
        WebGL.triangles
            [ ( topLeft, topRight, bottomLeft )
            , ( bottomLeft, topRight, bottomRight )
            ]



-- Shaders


type alias Uniforms =
    { perspective : Mat4
    , texture : Texture
    }


crateVertex : Shader Vertex Uniforms { vcoord : Vec2 }
crateVertex =
    [glsl|

        attribute vec3 position;
        attribute vec2 coord;
        uniform mat4 perspective;
        varying vec2 vcoord;

        void main () {
          gl_Position = perspective * vec4(position, 1.0);
          vcoord = coord;
        }

    |]


crateFragment : Shader {} { u | texture : Texture } { vcoord : Vec2 }
crateFragment =
    [glsl|

        precision mediump float;
        uniform sampler2D texture;
        varying vec2 vcoord;

        void main () {
          gl_FragColor = texture2D(texture, vcoord);
        }

    |]


floorVertex : Shader { position : Vec3 } Uniforms {}
floorVertex =
    [glsl|

        attribute vec3 position;
        uniform mat4 perspective;

        void main () {
          gl_Position = perspective * vec4(position, 1.0);
        }

    |]


floorFragment : Shader attributes Uniforms {}
floorFragment =
    [glsl|

        precision mediump float;

        void main () {
          gl_FragColor = vec4(0.0, 0.0, 0.0, 1.0);
        }

    |]
