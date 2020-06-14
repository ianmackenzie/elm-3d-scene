module SinglePointTest exposing (main)

import Html exposing (Html)
import Html.Attributes
import Math.Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import WebGL


dummyMesh : WebGL.Mesh { dummyAttribute : Float }
dummyMesh =
    WebGL.points [ { dummyAttribute = 0 } ]


vertexShader : WebGL.Shader { dummyAttribute : Float } { position : Vec2 } {}
vertexShader =
    [glsl|
        precision highp float;
        
        attribute lowp float dummyAttribute;
        
        uniform highp vec2 position;
        
        void main () {
            gl_Position = vec4(position, 0.0, 1.0);
            gl_PointSize = 10.0;
        }
    |]


fragmentShader : WebGL.Shader {} { position : Vec2 } {}
fragmentShader =
    [glsl|
        precision lowp float;
        
        void main () {
            gl_FragColor = vec4(0.0, 0.0, 1.0, 1.0);
        }
    |]


pointEntity : WebGL.Entity
pointEntity =
    WebGL.entity vertexShader fragmentShader dummyMesh { position = Math.Vector2.vec2 0 0 }


main : Html msg
main =
    WebGL.toHtml [ Html.Attributes.width 200, Html.Attributes.height 200 ] [ pointEntity ]
