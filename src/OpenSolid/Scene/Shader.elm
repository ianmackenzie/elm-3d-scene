module OpenSolid.Scene.Shader
    exposing
        ( ambient1Fragment
        , ambient2Fragment
        , ambient3Fragment
        , ambient4Fragment
        , ambient5Fragment
        , ambient6Fragment
        , ambient7Fragment
        , ambient8Fragment
        , ambientOnlyFragment
        , dummyFragment
        , emissiveFragment
        , flatFragment
        , noAmbient1Fragment
        , noAmbient2Fragment
        , noAmbient3Fragment
        , noAmbient4Fragment
        , noAmbient5Fragment
        , noAmbient6Fragment
        , noAmbient7Fragment
        , noAmbient8Fragment
        , physicalVertex
        , simpleVertex
        )

import Math.Matrix4 as Matrix4 exposing (Mat4)
import Math.Vector3 as Vector3 exposing (Vec3)
import OpenSolid.Scene.Types as Types
import WebGL


type alias SimpleVaryings =
    { interpolatedPosition : Vec3
    , interpolatedColor : Vec3
    }


type alias PhysicalVaryings =
    { interpolatedPosition : Vec3
    , interpolatedNormal : Vec3
    , interpolatedBaseColor : Vec3
    , interpolatedRoughness : Float
    , interpolatedMetallic : Float
    }


simpleVertex : WebGL.Shader Types.SimpleAttributes { a | modelScale : Float, modelMatrix : Mat4, modelViewProjectionMatrix : Mat4 } SimpleVaryings
simpleVertex =
    [glsl|
        attribute float x;
        attribute float y;
        attribute float z;
        attribute float r;
        attribute float g;
        attribute float b;

        uniform float modelScale;
        uniform mat4 modelMatrix;
        uniform mat4 modelViewProjectionMatrix;

        varying vec3 interpolatedPosition;
        varying vec3 interpolatedColor;

        void main () {
            vec4 scaledPosition = vec4(modelScale * x, modelScale * y, modelScale * z, 1.0);
            gl_Position = modelViewProjectionMatrix * scaledPosition;
            interpolatedPosition = (modelMatrix * scaledPosition).xyz;
            interpolatedColor = vec3(r, g, b);
        }
    |]


physicalVertex : WebGL.Shader Types.PhysicalAttributes { a | modelScale : Float, modelMatrix : Mat4, modelViewProjectionMatrix : Mat4 } PhysicalVaryings
physicalVertex =
    [glsl|
        attribute float x;
        attribute float y;
        attribute float z;
        attribute float nx;
        attribute float ny;
        attribute float nz;
        attribute float r;
        attribute float g;
        attribute float b;
        attribute float rg;
        attribute float mt;

        uniform float modelScale;
        uniform mat4 modelMatrix;
        uniform mat4 modelViewProjectionMatrix;

        varying vec3 interpolatedPosition;
        varying vec3 interpolatedNormal;
        varying vec3 interpolatedBaseColor;
        varying float interpolatedRoughness;
        varying float interpolatedMetallic;

        void main () {
            vec4 scaledPosition = vec4(modelScale * x, modelScale * y, modelScale * z, 1.0);
            gl_Position = modelViewProjectionMatrix * scaledPosition;
            interpolatedPosition = (modelMatrix * scaledPosition).xyz;
            interpolatedNormal = (modelMatrix * vec4(nx, ny, nz, 0.0)).xyz;
            interpolatedBaseColor = vec3(r, g, b);
            interpolatedRoughness = rg;
            interpolatedMetallic = mt;
        }
    |]


flatFragment : WebGL.Shader {} a SimpleVaryings
flatFragment =
    [glsl|
        precision mediump float;

        varying vec3 interpolatedPosition;
        varying vec3 interpolatedColor;

        void main () {
            gl_FragColor = vec4(interpolatedColor, 1.0);
        }
    |]


emissiveFragment : WebGL.Shader {} { a | gammaCorrection : Float } SimpleVaryings
emissiveFragment =
    [glsl|
        precision mediump float;

        uniform float gammaCorrection;

        varying vec3 interpolatedPosition;
        varying vec3 interpolatedColor;

        void main () {
            float red = pow(interpolatedColor.r, gammaCorrection);
            float green = pow(interpolatedColor.g, gammaCorrection);
            float blue = pow(interpolatedColor.b, gammaCorrection);
            gl_FragColor = vec4(red, green, blue, 1.0);
        }
    |]


physicalFragmentSource : String
physicalFragmentSource =
    """
precision mediump float;

uniform vec3 eyePoint;
uniform float gammaCorrection;

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

#ifdef LIGHT5
uniform int lightType5;
uniform vec3 lightColor5;
uniform vec3 lightVector5;
uniform float lightRadius5;
#endif

#ifdef LIGHT6
uniform int lightType6;
uniform vec3 lightColor6;
uniform vec3 lightVector6;
uniform float lightRadius6;
#endif

#ifdef LIGHT7
uniform int lightType7;
uniform vec3 lightColor7;
uniform vec3 lightVector7;
uniform float lightRadius7;
#endif

#ifdef LIGHT8
uniform int lightType8;
uniform vec3 lightColor8;
uniform vec3 lightVector8;
uniform float lightRadius8;
#endif

varying vec3 interpolatedPosition;
varying vec3 interpolatedNormal;
varying vec3 interpolatedBaseColor;
varying float interpolatedRoughness;
varying float interpolatedMetallic;

#ifdef LIGHT1
// Leave pi out of the denominator and then don't multiply by it later
float normalFactor(float alphaSquared, float dotNHSquared) {
    float tmp = dotNHSquared * (alphaSquared - 1.0) + 1.0;
    return alphaSquared / (tmp * tmp);
}

// Leave dotNL and dotNV out of the numerator and then leave them out of
// the denominator later
float geometryFactor(float dotNL, float dotNV) {
    float tmp = interpolatedRoughness + 1.0;
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
        vec3 displacement = lightVector - interpolatedPosition;
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
    vec2 textureCoordinates = vec2(dotNV, interpolatedRoughness);
    vec4 textureColor = texture2D(ambientLookupTexture, textureCoordinates);
    float scale = textureColor.r + (1.0 / 255.0) * textureColor.g;
    float offset = textureColor.b + (1.0 / 255.0) * textureColor.a;

    vec3 specularColor = specularBaseColor * scale + vec3(1.0, 1.0, 1.0) * offset;
    return (diffuseBaseColor + specularColor) * ambientLightColor;
}
#endif

void main() {
    vec3 normalDirection = normalize(interpolatedNormal);
    vec3 viewDirection = normalize(eyePoint - interpolatedPosition);
    float dotNV = clamp(dot(normalDirection, viewDirection), 0.0, 1.0);

    float nonmetallic = 1.0 - interpolatedMetallic;
    vec3 diffuseBaseColor = nonmetallic * 0.96 * interpolatedBaseColor;
    vec3 specularBaseColor = nonmetallic * 0.04 * vec3(1.0, 1.0, 1.0) + interpolatedMetallic * interpolatedBaseColor;

    vec3 linearColor = vec3(0.0, 0.0, 0.0);

#ifdef AMBIENT
    linearColor += ambientLitColor(dotNV, specularBaseColor, diffuseBaseColor);
#endif

#ifdef LIGHT1
    float alpha = interpolatedRoughness * interpolatedRoughness;
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

#ifdef LIGHT5
    linearColor += litColor(lightType5, lightColor5, lightVector5, lightRadius5, normalDirection, viewDirection, dotNV, diffuseBaseColor, specularBaseColor, alphaSquared);
#endif

#ifdef LIGHT6
    linearColor += litColor(lightType6, lightColor6, lightVector6, lightRadius6, normalDirection, viewDirection, dotNV, diffuseBaseColor, specularBaseColor, alphaSquared);
#endif

#ifdef LIGHT7
    linearColor += litColor(lightType7, lightColor7, lightVector7, lightRadius7, normalDirection, viewDirection, dotNV, diffuseBaseColor, specularBaseColor, alphaSquared);
#endif

#ifdef LIGHT8
    linearColor += litColor(lightType8, lightColor8, lightVector8, lightRadius8, normalDirection, viewDirection, dotNV, diffuseBaseColor, specularBaseColor, alphaSquared);
#endif

    // Apply gamma correction
    float red = pow(linearColor.r, gammaCorrection);
    float green = pow(linearColor.g, gammaCorrection);
    float blue = pow(linearColor.b, gammaCorrection);
    gl_FragColor = vec4(red, green, blue, 1.0);
}
"""


physicalFragment : { ambientLighting : Bool, numLights : Int } -> WebGL.Shader {} a PhysicalVaryings
physicalFragment { ambientLighting, numLights } =
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
    WebGL.unsafeShader (definitions ++ "\n" ++ physicalFragmentSource)


type alias BaseUniforms a =
    { a | eyePoint : Vec3, gammaCorrection : Float }


type alias AmbientUniforms a =
    { a | ambientLightColor : Vec3, ambientLookupTexture : WebGL.Texture }


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


type alias Light5Uniforms a =
    Light4Uniforms
        { a
            | lightType5 : Int
            , lightColor5 : Vec3
            , lightVector5 : Vec3
            , lightRadius5 : Float
        }


type alias Light6Uniforms a =
    Light5Uniforms
        { a
            | lightType6 : Int
            , lightColor6 : Vec3
            , lightVector6 : Vec3
            , lightRadius6 : Float
        }


type alias Light7Uniforms a =
    Light6Uniforms
        { a
            | lightType7 : Int
            , lightColor7 : Vec3
            , lightVector7 : Vec3
            , lightRadius7 : Float
        }


type alias Light8Uniforms a =
    Light7Uniforms
        { a
            | lightType8 : Int
            , lightColor8 : Vec3
            , lightVector8 : Vec3
            , lightRadius8 : Float
        }


ambientOnlyFragment : WebGL.Shader {} (AmbientUniforms (BaseUniforms a)) PhysicalVaryings
ambientOnlyFragment =
    physicalFragment { ambientLighting = True, numLights = 0 }


ambient1Fragment : WebGL.Shader {} (AmbientUniforms (Light1Uniforms (BaseUniforms a))) PhysicalVaryings
ambient1Fragment =
    physicalFragment { ambientLighting = True, numLights = 1 }


ambient2Fragment : WebGL.Shader {} (AmbientUniforms (Light2Uniforms (BaseUniforms a))) PhysicalVaryings
ambient2Fragment =
    physicalFragment { ambientLighting = True, numLights = 2 }


ambient3Fragment : WebGL.Shader {} (AmbientUniforms (Light3Uniforms (BaseUniforms a))) PhysicalVaryings
ambient3Fragment =
    physicalFragment { ambientLighting = True, numLights = 3 }


ambient4Fragment : WebGL.Shader {} (AmbientUniforms (Light4Uniforms (BaseUniforms a))) PhysicalVaryings
ambient4Fragment =
    physicalFragment { ambientLighting = True, numLights = 4 }


ambient5Fragment : WebGL.Shader {} (AmbientUniforms (Light5Uniforms (BaseUniforms a))) PhysicalVaryings
ambient5Fragment =
    physicalFragment { ambientLighting = True, numLights = 5 }


ambient6Fragment : WebGL.Shader {} (AmbientUniforms (Light6Uniforms (BaseUniforms a))) PhysicalVaryings
ambient6Fragment =
    physicalFragment { ambientLighting = True, numLights = 6 }


ambient7Fragment : WebGL.Shader {} (AmbientUniforms (Light7Uniforms (BaseUniforms a))) PhysicalVaryings
ambient7Fragment =
    physicalFragment { ambientLighting = True, numLights = 7 }


ambient8Fragment : WebGL.Shader {} (AmbientUniforms (Light8Uniforms (BaseUniforms a))) PhysicalVaryings
ambient8Fragment =
    physicalFragment { ambientLighting = True, numLights = 8 }


noAmbient1Fragment : WebGL.Shader {} (Light1Uniforms (BaseUniforms a)) PhysicalVaryings
noAmbient1Fragment =
    physicalFragment { ambientLighting = False, numLights = 1 }


noAmbient2Fragment : WebGL.Shader {} (Light2Uniforms (BaseUniforms a)) PhysicalVaryings
noAmbient2Fragment =
    physicalFragment { ambientLighting = False, numLights = 2 }


noAmbient3Fragment : WebGL.Shader {} (Light3Uniforms (BaseUniforms a)) PhysicalVaryings
noAmbient3Fragment =
    physicalFragment { ambientLighting = False, numLights = 3 }


noAmbient4Fragment : WebGL.Shader {} (Light4Uniforms (BaseUniforms a)) PhysicalVaryings
noAmbient4Fragment =
    physicalFragment { ambientLighting = False, numLights = 4 }


noAmbient5Fragment : WebGL.Shader {} (Light5Uniforms (BaseUniforms a)) PhysicalVaryings
noAmbient5Fragment =
    physicalFragment { ambientLighting = False, numLights = 5 }


noAmbient6Fragment : WebGL.Shader {} (Light6Uniforms (BaseUniforms a)) PhysicalVaryings
noAmbient6Fragment =
    physicalFragment { ambientLighting = False, numLights = 6 }


noAmbient7Fragment : WebGL.Shader {} (Light7Uniforms (BaseUniforms a)) PhysicalVaryings
noAmbient7Fragment =
    physicalFragment { ambientLighting = False, numLights = 7 }


noAmbient8Fragment : WebGL.Shader {} (Light8Uniforms (BaseUniforms a)) PhysicalVaryings
noAmbient8Fragment =
    physicalFragment { ambientLighting = False, numLights = 8 }


dummyFragment : WebGL.Shader {} { a | gammaCorrection : Float } PhysicalVaryings
dummyFragment =
    [glsl|
        precision mediump float;

        varying vec3 interpolatedPosition;
        varying vec3 interpolatedNormal;
        varying vec3 interpolatedBaseColor;
        varying float interpolatedRoughness;
        varying float interpolatedMetallic;

        uniform float gammaCorrection;

        void main() {
            // Apply gamma correction
            float red = pow(interpolatedBaseColor.r, gammaCorrection);
            float green = pow(interpolatedBaseColor.g, gammaCorrection);
            float blue = pow(interpolatedBaseColor.b, gammaCorrection);
            gl_FragColor = vec4(red, green, blue, 1.0);
        }
    |]
