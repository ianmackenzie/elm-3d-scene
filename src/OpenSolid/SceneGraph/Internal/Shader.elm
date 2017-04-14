module OpenSolid.SceneGraph.Internal.Shader exposing (..)

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Triangle3d as Triangle3d
import OpenSolid.WebGL.Direction3d as Direction3d
import OpenSolid.WebGL.Point3d as Point3d
import Math.Vector3 as Vector3 exposing (Vec3)
import Math.Vector4 as Vector4 exposing (Vec4)
import Math.Matrix4 as Matrix4 exposing (Mat4)
import WebGL


type alias MeshAttributes =
    { vertexPosition : Vec3
    , vertexNormal : Vec3
    }


type alias MeshUniforms a =
    { a
        | modelMatrix : Mat4
        , modelViewMatrix : Mat4
        , modelViewProjectionMatrix : Mat4
    }


type alias MeshVaryings =
    { position : Vec3
    , normal : Vec3
    }


type alias MeshVertexShader a =
    WebGL.Shader MeshAttributes (MeshUniforms a) MeshVaryings


meshVertexShader : MeshVertexShader a
meshVertexShader =
    [glsl|
        attribute vec3 vertexPosition;
        attribute vec3 vertexNormal;

        uniform mat4 modelMatrix;
        uniform mat4 modelViewMatrix;
        uniform mat4 modelViewProjectionMatrix;

        varying vec3 position;
        varying vec3 normal;

        void main () {
          gl_Position = modelViewProjectionMatrix * vec4(vertexPosition, 1.0);
          position = (modelMatrix * vec4(vertexPosition, 1.0)).xyz;
          normal = (modelMatrix * vec4(vertexNormal, 0.0)).xyz;
        }
    |]


type alias MeshFragmentShader a =
    WebGL.Shader {} (MeshUniforms a) MeshVaryings


type alias SolidMaterialUniforms =
    { color : Vec4 }


solidFragmentShader : MeshFragmentShader SolidMaterialUniforms
solidFragmentShader =
    [glsl|
        precision mediump float;

        uniform vec4 color;

        varying vec3 position;
        varying vec3 normal;

        void main () {
            gl_FragColor = color;
        }
    |]


type alias VertexPosition =
    { vertexPosition : Vec3
    }


type alias PrimitiveUniforms =
    { modelViewProjectionMatrix : Mat4
    , color : Vec4
    , size : Float
    }


type alias PrimitiveVertexShader =
    WebGL.Shader VertexPosition PrimitiveUniforms {}


primitiveVertexShader : PrimitiveVertexShader
primitiveVertexShader =
    [glsl|
        attribute vec3 vertexPosition;

        uniform mat4 modelViewProjectionMatrix;

        void main () {
          gl_Position = modelViewProjectionMatrix * vec4(vertexPosition, 1.0);
        }
    |]


type alias PrimitiveFragmentShader =
    WebGL.Shader {} PrimitiveUniforms {}


primitiveFragmentShader : PrimitiveFragmentShader
primitiveFragmentShader =
    [glsl|
        precision mediump float;

        uniform vec4 color;

        void main () {
            gl_FragColor = color;
        }
    |]
