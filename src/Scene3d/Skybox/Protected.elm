module Scene3d.Skybox.Protected exposing (Skybox(..), SkyboxQuad, quad)

import Camera3d exposing (Camera3d)
import Geometry.Interop.LinearAlgebra.Point3d as Point3d
import Length exposing (Meters)
import Math.Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3)
import Point3d
import Quantity exposing (Quantity(..))
import Viewpoint3d
import WebGL
import WebGL.Matrices
import WebGL.Texture as WebGL



-- SKYBOX


type Skybox
    = EquirectTexture WebGL.Texture


toTexture : Skybox -> WebGL.Texture
toTexture (EquirectTexture texture) =
    texture



-- SKYBOX QUAD


type alias SkyboxQuad =
    WebGL.Entity


quad :
    { camera : Camera3d Meters coordinates
    , aspectRatio : Float
    , skybox : Skybox
    }
    -> SkyboxQuad
quad { camera, aspectRatio, skybox } =
    let
        eyeCamera =
            camera
                |> Camera3d.viewpoint
                |> Viewpoint3d.eyePoint
                |> Point3d.toVec3

        inverseViewProjectionMatrix =
            WebGL.Matrices.viewProjectionMatrix camera
                { aspectRatio = aspectRatio
                , nearClipDepth = Quantity 0.01
                , farClipDepth = Quantity 1.01
                }
                |> Math.Matrix4.inverse
                |> Maybe.withDefault Math.Matrix4.identity
    in
    WebGL.entity
        skyboxVertexShader
        skyboxFragmentShader
        mesh
        { skyboxTexture = toTexture skybox
        , eyePoint = eyeCamera
        , inverseViewProjectionMatrix = inverseViewProjectionMatrix
        }



-- SKYBOX SHADERS


type alias SkyboxVertex =
    { position : Vec2
    }


type alias SkyboxVarying =
    { vposition : Vec2
    }


type alias SkyboxUniforms =
    { skyboxTexture : WebGL.Texture
    , eyePoint : Vec3
    , inverseViewProjectionMatrix : Mat4
    }


skyboxVertexShader : WebGL.Shader SkyboxVertex SkyboxUniforms SkyboxVarying
skyboxVertexShader =
    [glsl|
        attribute vec2 position;
        varying vec2 vposition;

        void main() {
            vposition = position;
            gl_Position = vec4(position.x, position.y, 0, 1.0);
        }
    |]


skyboxFragmentShader : WebGL.Shader {} SkyboxUniforms SkyboxVarying
skyboxFragmentShader =
    [glsl|
        precision mediump float;

        const float PI = 3.1415926535897932384626433832795;
        const float M_PI = 1.0 / PI;      
        const float M_2PI = 1.0 / (2.0 * PI);

        uniform vec3 eyePoint;
        uniform sampler2D skyboxTexture;
        uniform mat4 inverseViewProjectionMatrix;

        varying vec2 vposition;

        void main() {
            vec2 textureCoordinate;

            vec4 projPos = inverseViewProjectionMatrix * vec4(vposition.x, vposition.y, 0.0, 1.0);
            vec3 skyboxPoint = projPos.xyz/projPos.w;

            vec3 skyboxRay = normalize(skyboxPoint - eyePoint);

            textureCoordinate.x = 0.5 + atan(skyboxRay.x, skyboxRay.y) * M_2PI;
            textureCoordinate.y = 0.5 + asin(skyboxRay.z) * M_PI;

            gl_FragColor = texture2D(skyboxTexture, textureCoordinate);
        }
    |]



-- SKYBOX MESH


type alias SkyboxMesh =
    WebGL.Mesh SkyboxVertex


mesh : SkyboxMesh
mesh =
    let
        bottomLeft =
            SkyboxVertex (vec2 -1 -1)

        bottomRight =
            SkyboxVertex (vec2 1 -1)

        topLeft =
            SkyboxVertex (vec2 -1 1)

        topRight =
            SkyboxVertex (vec2 1 1)
    in
    WebGL.triangles
        [ ( bottomLeft, bottomRight, topLeft )
        , ( topLeft, bottomRight, topRight )
        ]
