module Lookup exposing (..)

import Html exposing (Html)
import Html.Attributes
import Math.Vector2 exposing (Vec2)
import Task
import WebGL exposing (Shader)
import WebGL.Texture as Texture exposing (Texture)


type alias Model =
    Maybe (Result Texture.Error Texture)


type Msg
    = LoadComplete (Result Texture.Error Texture)


init : ( Model, Cmd Msg )
init =
    ( Nothing, Task.attempt LoadComplete (Texture.load "lookup.png") )


update : Msg -> Model -> ( Model, Cmd Msg )
update (LoadComplete result) _ =
    ( Just result, Cmd.none )


vertexShader : Shader { vertexPosition : Vec2 } a {}
vertexShader =
    [glsl|
        attribute vec2 vertexPosition;

        void main() {
            gl_Position = vec4(vertexPosition.x, vertexPosition.y, 0, 1);
        }
    |]


fragmentShader : Shader {} { sampler : Texture, dimensions : Vec2 } {}
fragmentShader =
    [glsl|
        precision mediump float;

        uniform sampler2D sampler;
        uniform vec2 dimensions;

        void main() {
            vec2 textureCoordinates = gl_FragCoord.xy / dimensions;
            vec4 textureColor = texture2D(sampler, textureCoordinates);
            float red = textureColor.r + (1.0 / 255.0) * textureColor.g;
            float green = textureColor.b + (1.0 / 255.0) * textureColor.a;
            gl_FragColor = vec4(red, green, 0.0, 1.0);
        }
    |]


textureImage : Texture -> Html Msg
textureImage texture =
    let
        ( width, height ) =
            Texture.size texture

        vertex x y =
            { vertexPosition = Math.Vector2.vec2 x y }

        quad =
            WebGL.triangles
                [ ( vertex -1 -1, vertex 1 -1, vertex 1 1 )
                , ( vertex -1 -1, vertex 1 1, vertex -1 1 )
                ]

        uniforms =
            { sampler = texture
            , dimensions = Math.Vector2.vec2 (toFloat width) (toFloat height)
            }
    in
    WebGL.toHtml [ Html.Attributes.width width, Html.Attributes.height height ]
        [ WebGL.entity vertexShader fragmentShader quad uniforms
        ]


view : Model -> Html Msg
view model =
    case model of
        Nothing ->
            Html.text ""

        Just (Ok texture) ->
            textureImage texture

        Just (Err _) ->
            Html.text "Error loading texture"


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
