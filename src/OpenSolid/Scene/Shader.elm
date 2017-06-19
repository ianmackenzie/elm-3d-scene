module OpenSolid.Scene.Shader exposing (..)

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


physicallyBasedDirectionalLightShader : WebGL.Shader {} { a | baseColor : Vec3, roughness : Float, metallic : Float, lightColor : Vec3, lightDirection : Vec3, eyePoint : Vec3 } { position : Vec3, normal : Vec3 }
physicallyBasedDirectionalLightShader =
    [glsl|
        precision mediump float;

        uniform vec3 baseColor;
        uniform float roughness;
        uniform float metallic;
        uniform vec3 lightColor;
        uniform vec3 lightDirection;
        uniform vec3 eyePoint;

        varying vec3 position;
        varying vec3 normal;

        // leave pi out of the denominator and then don't multiply by it later
        float normalFactor(float alphaSquared, float dotNHSquared) {
            float tmp = dotNHSquared * (alphaSquared - 1.0) + 1.0;
            return alphaSquared / (tmp * tmp);
        }

        // leave dotNL and dotNV out of the numerator and then leave them out of
        // the denominator later
        float geometryFactor(float dotNL, float dotNV) {
            float tmp = roughness + 1.0;
            float k = 0.125 * tmp * tmp;
            float oneMinusK = 1.0 - k;
            return 1.0 / ((dotNL * oneMinusK + k) * (dotNV * oneMinusK + k));
        }

        vec3 fresnelColor(vec3 specularBaseColor, float dotVH) {
            vec3 white = vec3(1.0, 1.0, 1.0);
            float scale = exp2((-5.55473 * dotVH - 6.98316) * dotVH);
            return specularBaseColor + (white - specularBaseColor) * scale;
        }

        void main() {
            vec3 normalDirection = normalize(normal);
            vec3 viewDirection = normalize(eyePoint - position);
            vec3 halfDirection = normalize(viewDirection + lightDirection);
            float alpha = roughness * roughness;
            float alphaSquared = alpha * alpha;
            float dotVH = clamp(dot(viewDirection, halfDirection), 0.0, 1.0);
            float dotNH = clamp(dot(normalDirection, halfDirection), 0.0, 1.0);
            float dotNL = clamp(dot(normalDirection, lightDirection), 0.0, 1.0);
            float dotNV = clamp(dot(normalDirection, viewDirection), 0.0, 1.0);
            float dotNHSquared = dotNH * dotNH;
            float dielectric = 1.0 - metallic;
            vec3 diffuseBaseColor = dielectric * 0.96 * baseColor;
            vec3 specularBaseColor = 0.04 * dielectric + metallic * baseColor;

            float d = normalFactor(alphaSquared, dotNHSquared);
            float g = geometryFactor(dotNL, dotNV);
            vec3 f = fresnelColor(specularBaseColor, dotVH);
            vec3 specularColor = (0.25 * d * g) * f;

            vec3 linearColor = dotNL * (diffuseBaseColor + specularColor) * lightColor;
            float red = pow(linearColor.r, 0.45);
            float green = pow(linearColor.g, 0.45);
            float blue = pow(linearColor.b, 0.45);
            gl_FragColor = vec4(red, green, blue, 1.0);
        }
    |]
