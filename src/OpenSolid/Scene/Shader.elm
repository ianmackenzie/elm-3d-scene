module OpenSolid.Scene.Shader
    exposing
        ( ambient1
        , ambient2
        , ambient3
        , ambient4
        , ambientOnly
        , dummy
        , noAmbient1
        , noAmbient2
        , noAmbient3
        , noAmbient4
        , simple
        , simpleVertex
        , vertex
        )

import Math.Matrix4 as Matrix4 exposing (Mat4)
import Math.Vector3 as Vector3 exposing (Vec3)
import WebGL


type alias Varyings =
    { position : Vec3
    , normal : Vec3
    }


simpleVertex : WebGL.Shader { vertexPosition : Vec3 } { a | modelMatrix : Mat4, modelViewProjectionMatrix : Mat4 } { position : Vec3 }
simpleVertex =
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


vertex : WebGL.Shader { vertexPosition : Vec3, vertexNormal : Vec3 } { a | modelMatrix : Mat4, modelViewProjectionMatrix : Mat4 } Varyings
vertex =
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


simple : WebGL.Shader {} { a | color : Vec3 } { position : Vec3 }
simple =
    [glsl|
        precision mediump float;

        uniform vec3 color;

        varying vec3 position;

        void main () {
            gl_FragColor = vec4(color, 1.0);
        }
    |]


shaderSource : String
shaderSource =
    """
precision mediump float;

uniform vec3 baseColor;
uniform float roughness;
uniform float metallic;

uniform vec3 eyePoint;

#ifdef AMBIENT
uniform vec3 ambientLightColor;
uniform sampler2D ambientLookupTexture;
#endif

#ifdef LIGHT1
uniform int lightType1;
uniform vec3 lightColor1;
uniform vec3 lightVector1;
uniform float lightRadius1;
#endif

#ifdef LIGHT2
uniform int lightType2;
uniform vec3 lightColor2;
uniform vec3 lightVector2;
uniform float lightRadius2;
#endif

#ifdef LIGHT3
uniform int lightType3;
uniform vec3 lightColor3;
uniform vec3 lightVector3;
uniform float lightRadius3;
#endif

#ifdef LIGHT4
uniform int lightType4;
uniform vec3 lightColor4;
uniform vec3 lightVector4;
uniform float lightRadius4;
#endif

varying vec3 position;
varying vec3 normal;

#ifdef LIGHT1
// Leave pi out of the denominator and then don't multiply by it later
float normalFactor(float alphaSquared, float dotNHSquared) {
    float tmp = dotNHSquared * (alphaSquared - 1.0) + 1.0;
    return alphaSquared / (tmp * tmp);
}

// Leave dotNL and dotNV out of the numerator and then leave them out of
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

vec3 litColor(int lightType, vec3 lightColor, vec3 lightVector, float lightRadius, vec3 normalDirection, vec3 viewDirection, float dotNV, vec3 diffuseBaseColor, vec3 specularBaseColor, float alphaSquared) {
    vec3 lightDirection = vec3(0.0, 0.0, 0.0);
    if (lightType == 1) {
        // Directional light
        lightDirection = lightVector;
    } else if (lightType == 2) {
        // Point lightType
        vec3 displacement = lightVector - position;
        float distance = length(displacement);
        lightDirection = displacement / distance;
        lightColor = lightColor / (distance * distance);
    }

    vec3 halfDirection = normalize(viewDirection + lightDirection);
    float dotVH = clamp(dot(viewDirection, halfDirection), 0.0, 1.0);
    float dotNH = clamp(dot(normalDirection, halfDirection), 0.0, 1.0);
    float dotNL = clamp(dot(normalDirection, lightDirection), 0.0, 1.0);
    float dotNHSquared = dotNH * dotNH;

    float d = normalFactor(alphaSquared, dotNHSquared);
    float g = geometryFactor(dotNL, dotNV);
    vec3 f = fresnelColor(specularBaseColor, dotVH);
    vec3 specularColor = (0.25 * d * g) * f;

    return dotNL * (diffuseBaseColor + specularColor) * lightColor;
}
#endif

#ifdef AMBIENT
vec3 ambientLitColor(float dotNV, vec3 specularBaseColor, vec3 diffuseBaseColor) {
    vec2 textureCoordinates = vec2(dotNV, roughness);
    vec4 textureColor = texture2D(ambientLookupTexture, textureCoordinates);
    float scale = textureColor.r + (1.0 / 255.0) * textureColor.g;
    float offset = textureColor.b + (1.0 / 255.0) * textureColor.a;

    vec3 specularColor = specularBaseColor * scale + vec3(1.0, 1.0, 1.0) * offset;
    return (diffuseBaseColor + specularColor) * ambientLightColor;
}
#endif

void main() {
    vec3 normalDirection = normalize(normal);
    vec3 viewDirection = normalize(eyePoint - position);
    float dotNV = clamp(dot(normalDirection, viewDirection), 0.0, 1.0);

    float nonmetallic = 1.0 - metallic;
    vec3 diffuseBaseColor = nonmetallic * 0.96 * baseColor;
    vec3 specularBaseColor = nonmetallic * 0.04 * vec3(1.0, 1.0, 1.0) + metallic * baseColor;

    vec3 linearColor = vec3(0.0, 0.0, 0.0);

#ifdef AMBIENT
    linearColor += ambientLitColor(dotNV, specularBaseColor, diffuseBaseColor);
#endif

#ifdef LIGHT1
    float alpha = roughness * roughness;
    float alphaSquared = alpha * alpha;
    linearColor += litColor(lightType1, lightColor1, lightVector1, lightRadius1, normalDirection, viewDirection, dotNV, diffuseBaseColor, specularBaseColor, alphaSquared);
#endif

#ifdef LIGHT2
    linearColor += litColor(lightType2, lightColor2, lightVector2, lightRadius2, normalDirection, viewDirection, dotNV, diffuseBaseColor, specularBaseColor, alphaSquared);
#endif

#ifdef LIGHT3
    linearColor += litColor(lightType3, lightColor3, lightVector3, lightRadius3, normalDirection, viewDirection, dotNV, diffuseBaseColor, specularBaseColor, alphaSquared);
#endif

#ifdef LIGHT4
    linearColor += litColor(lightType4, lightColor4, lightVector4, lightRadius4, normalDirection, viewDirection, dotNV, diffuseBaseColor, specularBaseColor, alphaSquared);
#endif

    // Apply gamma correction
    float red = pow(linearColor.r, 0.45);
    float green = pow(linearColor.g, 0.45);
    float blue = pow(linearColor.b, 0.45);
    gl_FragColor = vec4(red, green, blue, 1.0);
}
"""


createShader : { ambientLighting : Bool, numLights : Int } -> WebGL.Shader {} a Varyings
createShader { ambientLighting, numLights } =
    let
        addAmbientDefinition definitions =
            if ambientLighting then
                "#define AMBIENT" :: definitions
            else
                definitions

        definitions =
            List.range 1 numLights
                |> List.map (\num -> "#define LIGHT" ++ toString num)
                |> addAmbientDefinition
                |> String.join "\n"
    in
    WebGL.unsafeShader (definitions ++ "\n" ++ shaderSource)


type alias BaseUniforms a =
    { a
        | eyePoint : Vec3
        , baseColor : Vec3
        , roughness : Float
        , metallic : Float
    }


type alias AmbientUniforms a =
    { a
        | ambientLightColor : Vec3
        , ambientLookupTexture : WebGL.Texture
    }


type alias Light1Uniforms a =
    { a
        | lightType1 : Int
        , lightColor1 : Vec3
        , lightVector1 : Vec3
        , lightRadius1 : Float
    }


type alias Light2Uniforms a =
    Light1Uniforms
        { a
            | lightType2 : Int
            , lightColor2 : Vec3
            , lightVector2 : Vec3
            , lightRadius2 : Float
        }


type alias Light3Uniforms a =
    Light2Uniforms
        { a
            | lightType3 : Int
            , lightColor3 : Vec3
            , lightVector3 : Vec3
            , lightRadius3 : Float
        }


type alias Light4Uniforms a =
    Light3Uniforms
        { a
            | lightType4 : Int
            , lightColor4 : Vec3
            , lightVector4 : Vec3
            , lightRadius4 : Float
        }


ambientOnly : WebGL.Shader {} (AmbientUniforms (BaseUniforms a)) Varyings
ambientOnly =
    createShader { ambientLighting = True, numLights = 0 }


ambient1 : WebGL.Shader {} (AmbientUniforms (Light1Uniforms (BaseUniforms a))) Varyings
ambient1 =
    createShader { ambientLighting = True, numLights = 1 }


ambient2 : WebGL.Shader {} (AmbientUniforms (Light2Uniforms (BaseUniforms a))) Varyings
ambient2 =
    createShader { ambientLighting = True, numLights = 2 }


ambient3 : WebGL.Shader {} (AmbientUniforms (Light3Uniforms (BaseUniforms a))) Varyings
ambient3 =
    createShader { ambientLighting = True, numLights = 3 }


ambient4 : WebGL.Shader {} (AmbientUniforms (Light4Uniforms (BaseUniforms a))) Varyings
ambient4 =
    createShader { ambientLighting = True, numLights = 4 }


noAmbient1 : WebGL.Shader {} (Light1Uniforms (BaseUniforms a)) Varyings
noAmbient1 =
    createShader { ambientLighting = False, numLights = 1 }


noAmbient2 : WebGL.Shader {} (Light2Uniforms (BaseUniforms a)) Varyings
noAmbient2 =
    createShader { ambientLighting = False, numLights = 2 }


noAmbient3 : WebGL.Shader {} (Light3Uniforms (BaseUniforms a)) Varyings
noAmbient3 =
    createShader { ambientLighting = False, numLights = 3 }


noAmbient4 : WebGL.Shader {} (Light4Uniforms (BaseUniforms a)) Varyings
noAmbient4 =
    createShader { ambientLighting = False, numLights = 4 }


dummy : WebGL.Shader {} { a | baseColor : Vec3 } Varyings
dummy =
    [glsl|
        precision mediump float;

        varying vec3 position;
        varying vec3 normal;

        uniform vec3 baseColor;

        void main() {
            // Apply gamma correction
            float red = pow(baseColor.r, 0.45);
            float green = pow(baseColor.g, 0.45);
            float blue = pow(baseColor.b, 0.45);
            gl_FragColor = vec4(red, green, blue, 1.0);
        }
    |]
