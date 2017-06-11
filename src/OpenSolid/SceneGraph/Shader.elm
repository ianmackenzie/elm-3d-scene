module OpenSolid.SceneGraph.Shader exposing (..)

import Math.Matrix4 as Matrix4 exposing (Mat4)
import Math.Vector3 as Vector3 exposing (Vec3)
import Math.Vector4 as Vector4 exposing (Vec4)
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Triangle3d as Triangle3d
import OpenSolid.WebGL.Direction3d as Direction3d
import OpenSolid.WebGL.Point3d as Point3d
import WebGL


positionOnlyVertexShader : WebGL.Shader { vertexPosition : Vec3 } { a | modelMatrix : Mat4, modelViewProjectionMatrix : Mat4 } { position : Vec3 }
positionOnlyVertexShader =
    [glsl|
        attribute vec3 vertexPosition;

        uniform mat4 modelMatrix;
        uniform mat4 modelViewProjectionMatrix;

        varying vec3 position;

        void main () {
          gl_Position = modelViewProjectionMatrix * vec4(vertexPosition, 1.0);
          position = (modelMatrix * vec4(vertexPosition, 1.0)).xyz;
        }
    |]


positionAndNormalVertexShader : WebGL.Shader { vertexPosition : Vec3, vertexNormal : Vec3 } { a | modelMatrix : Mat4, modelViewProjectionMatrix : Mat4 } { position : Vec3, normal : Vec3 }
positionAndNormalVertexShader =
    [glsl|
        attribute vec3 vertexPosition;
        attribute vec3 vertexNormal;

        uniform mat4 modelMatrix;
        uniform mat4 modelViewProjectionMatrix;

        varying vec3 position;
        varying vec3 normal;

        void main () {
          gl_Position = modelViewProjectionMatrix * vec4(vertexPosition, 1.0);
          position = (modelMatrix * vec4(vertexPosition, 1.0)).xyz;
          normal = (modelMatrix * vec4(vertexNormal, 0.0)).xyz;
        }
    |]


solidColorShader : WebGL.Shader {} { a | color : Vec3 } { position : Vec3 }
solidColorShader =
    [glsl|
        precision mediump float;

        uniform vec3 color;

        varying vec3 position;

        void main () {
            gl_FragColor = vec4(color, 1.0);
        }
    |]


directionalLightShader : WebGL.Shader {} { a | baseColor : Vec3, roughness : Float, metallic : Float, lightColor : Vec3, lightDirection : Vec3 } { position : Vec3, normal : Vec3 }
directionalLightShader =
    [glsl|
        precision mediump float;

        uniform vec3 baseColor;
        uniform float roughness;
        uniform float metallic;
        uniform vec3 lightColor;
        uniform vec3 lightDirection;

        varying vec3 position;
        varying vec3 normal;

        void main() {
            gl_FragColor = vec4(baseColor, 1.0);
        }
    |]
