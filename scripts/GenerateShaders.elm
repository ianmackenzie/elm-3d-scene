module GenerateShaders exposing (main)

import Common
import Glsl
import Json.Decode as Decode
import Regex
import Script exposing (Script, UserPrivileges)
import Script.Directory as Directory exposing (Directory)
import Script.File as File exposing (File, Writable)



---------- ATTRIBUTES ----------


position : Glsl.Attribute
position =
    Glsl.attribute Glsl.vec3 "position"


normal : Glsl.Attribute
normal =
    Glsl.attribute Glsl.vec3 "normal"


uv : Glsl.Attribute
uv =
    Glsl.attribute Glsl.vec2 "uv"


tangent : Glsl.Attribute
tangent =
    Glsl.attribute Glsl.vec3 "tangent"


quadVertex : Glsl.Attribute
quadVertex =
    Glsl.attribute Glsl.vec3 "quadVertex"


quadShadowVertex : Glsl.Attribute
quadShadowVertex =
    Glsl.attribute Glsl.vec2 "quadShadowVertex"


angle : Glsl.Attribute
angle =
    Glsl.attribute Glsl.float "angle"


offsetScale : Glsl.Attribute
offsetScale =
    Glsl.attribute Glsl.float "offsetScale"



---------- UNIFORMS ----------


modelScale : Glsl.Uniform
modelScale =
    Glsl.uniform Glsl.vec3 "modelScale"


modelMatrix : Glsl.Uniform
modelMatrix =
    Glsl.uniform Glsl.mat4 "modelMatrix"


viewMatrix : Glsl.Uniform
viewMatrix =
    Glsl.uniform Glsl.mat4 "viewMatrix"


sceneProperties : Glsl.Uniform
sceneProperties =
    Glsl.uniform Glsl.mat4 "sceneProperties"


shadowLightSource : Glsl.Uniform
shadowLightSource =
    Glsl.uniform Glsl.mat4 "shadowLightSource"


pointRadius : Glsl.Uniform
pointRadius =
    Glsl.uniform Glsl.float "pointRadius"


constantColor : Glsl.Uniform
constantColor =
    Glsl.uniform Glsl.vec3 "constantColor"


colorTexture : Glsl.Uniform
colorTexture =
    Glsl.uniform Glsl.sampler2D "colorTexture"


emissiveColor : Glsl.Uniform
emissiveColor =
    Glsl.uniform Glsl.vec3 "emissiveColor"


backlight : Glsl.Uniform
backlight =
    Glsl.uniform Glsl.float "backlight"


baseColor : Glsl.Uniform
baseColor =
    Glsl.uniform Glsl.vec3 "baseColor"


baseColorTexture : Glsl.Uniform
baseColorTexture =
    Glsl.uniform Glsl.sampler2D "baseColorTexture"


constantBaseColor : Glsl.Uniform
constantBaseColor =
    Glsl.uniform Glsl.vec4 "constantBaseColor"


roughness : Glsl.Uniform
roughness =
    Glsl.uniform Glsl.float "roughness"


roughnessTexture : Glsl.Uniform
roughnessTexture =
    Glsl.uniform Glsl.sampler2D "roughnessTexture"


roughnessChannel : Glsl.Uniform
roughnessChannel =
    Glsl.uniform Glsl.vec4 "roughnessChannel"


metallic : Glsl.Uniform
metallic =
    Glsl.uniform Glsl.float "metallic"


metallicTexture : Glsl.Uniform
metallicTexture =
    Glsl.uniform Glsl.sampler2D "metallicTexture"


metallicChannel : Glsl.Uniform
metallicChannel =
    Glsl.uniform Glsl.vec4 "metallicChannel"


normalMapTexture : Glsl.Uniform
normalMapTexture =
    Glsl.uniform Glsl.sampler2D "normalMapTexture"


useNormalMap : Glsl.Uniform
useNormalMap =
    Glsl.uniform Glsl.float "useNormalMap"


environmentalLighting : Glsl.Uniform
environmentalLighting =
    Glsl.uniform Glsl.mat4 "environmentalLighting"


lightSources12 : Glsl.Uniform
lightSources12 =
    Glsl.uniform Glsl.mat4 "lightSources12"


lightSources34 : Glsl.Uniform
lightSources34 =
    Glsl.uniform Glsl.mat4 "lightSources34"


lightSources56 : Glsl.Uniform
lightSources56 =
    Glsl.uniform Glsl.mat4 "lightSources56"


lightSources78 : Glsl.Uniform
lightSources78 =
    Glsl.uniform Glsl.mat4 "lightSources78"


materialColor : Glsl.Uniform
materialColor =
    Glsl.uniform Glsl.vec3 "materialColor"


materialColorTexture : Glsl.Uniform
materialColorTexture =
    Glsl.uniform Glsl.sampler2D "materialColorTexture"


constantMaterialColor : Glsl.Uniform
constantMaterialColor =
    Glsl.uniform Glsl.vec2 "constantMaterialColor"


quadVertexPositions : Glsl.Uniform
quadVertexPositions =
    Glsl.uniform Glsl.mat4 "quadVertexPositions"


planarMap : Glsl.Uniform
planarMap =
    Glsl.uniform Glsl.mat4 "planarMap"


sphericalMap : Glsl.Uniform
sphericalMap =
    Glsl.uniform Glsl.mat4 "sphericalMap"



---------- VARYINGS ----------


interpolatedPosition : Glsl.Varying
interpolatedPosition =
    Glsl.varying Glsl.vec3 "interpolatedPosition"


interpolatedNormal : Glsl.Varying
interpolatedNormal =
    Glsl.varying Glsl.vec3 "interpolatedNormal"


interpolatedUv : Glsl.Varying
interpolatedUv =
    Glsl.varying Glsl.vec2 "interpolatedUv"


interpolatedTangent : Glsl.Varying
interpolatedTangent =
    Glsl.varying Glsl.vec3 "interpolatedTangent"



---------- CONSTANTS ----------


kPerspectiveProjection : Glsl.Constant
kPerspectiveProjection =
    Glsl.constant Glsl.float "kPerspectiveProjection" "0.0"


kOrthographicProjection : Glsl.Constant
kOrthographicProjection =
    Glsl.constant Glsl.float "kOrthographicProjection" "1.0"


kDisabledLightSource : Glsl.Constant
kDisabledLightSource =
    Glsl.constant Glsl.float "kDisabledLightSource" "0.0"


kDirectionalLightSource : Glsl.Constant
kDirectionalLightSource =
    Glsl.constant Glsl.float "kDirectionalLightSource" "1.0"


kPointLightSource : Glsl.Constant
kPointLightSource =
    Glsl.constant Glsl.float "kPointLightSource" "2.0"


kNoEnvironmentalLighting : Glsl.Constant
kNoEnvironmentalLighting =
    Glsl.constant Glsl.float "kNoEnvironmentalLighting" "0.0"


kSoftLighting : Glsl.Constant
kSoftLighting =
    Glsl.constant Glsl.float "kSoftLighting" "1.0"


kPi : Glsl.Constant
kPi =
    Glsl.constant Glsl.float "kPi" "3.14159265359"


kMediumpFloatMax : Glsl.Constant
kMediumpFloatMax =
    Glsl.constant Glsl.float "kMediumpFloatMax" "65504.0"



---------- FUNCTIONS ----------


project : Glsl.Function
project =
    Glsl.function { dependencies = [], constants = [] }
        """
        vec4 project(vec4 position, vec4 projectionProperties) {
            float n = projectionProperties[0];
            float a = projectionProperties[1];
            float kc = projectionProperties[2];
            float kz = projectionProperties[3];
            return vec4(
                (kc + kz * position.z) * (position.x / a),
                (kc + kz * position.z) * position.y,
                (-position.z - 2.0 * n),
                -position.z
            );
        }
        """


gammaCorrect : Glsl.Function
gammaCorrect =
    Glsl.function { dependencies = [], constants = [] }
        """
        float gammaCorrect(float u) {
            if (u <= 0.0031308) {
                return 12.92 * u;
            } else {
                return 1.055 * pow(u, 1.0 / 2.4) - 0.055;
            }
        }
        """


toneMap : Glsl.Function
toneMap =
    Glsl.function { dependencies = [], constants = [] }
        """
        float toneMap(float y, float yMax) {
            return y * (1.0 + (y / (yMax * yMax))) / (1.0 + y);
        }
        """


toSrgb : Glsl.Function
toSrgb =
    Glsl.function { dependencies = [ toneMap, gammaCorrect ], constants = [] }
        """
        vec4 toSrgb(vec3 linearColor, mat4 sceneProperties) {
            vec3 referenceWhite = sceneProperties[2].rgb;
            float linearR = linearColor.r / referenceWhite.r;
            float linearG = linearColor.g / referenceWhite.g;
            float linearB = linearColor.b / referenceWhite.b;
            float luminance = 0.2126 * linearR + 0.7152 * linearG + 0.0722 * linearB;
            float dynamicRange = sceneProperties[2].a;
            float toneMappedLuminance = toneMap(luminance, dynamicRange);
            float toneMapScale = toneMappedLuminance / luminance;
            float red = gammaCorrect(linearR * toneMapScale);
            float green = gammaCorrect(linearG * toneMapScale);
            float blue = gammaCorrect(linearB * toneMapScale);
            return vec4(red, green, blue, 1.0);
        }
        """


inverseGamma : Glsl.Function
inverseGamma =
    Glsl.function { dependencies = [], constants = [] }
        """
        float inverseGamma(float u) {
            if (u <= 0.04045) {
                return clamp(u / 12.92, 0.0, 1.0);
            } else {
                return clamp(pow((u + 0.055) / 1.055, 2.4), 0.0, 1.0);
            }
        }
        """


fromSrgb : Glsl.Function
fromSrgb =
    Glsl.function { dependencies = [ inverseGamma ], constants = [] }
        """
        vec3 fromSrgb(vec3 srgbColor) {
            return vec3(
                inverseGamma(srgbColor.r),
                inverseGamma(srgbColor.g),
                inverseGamma(srgbColor.b)
            );
        }
        """


pointAlpha : Glsl.Function
pointAlpha =
    Glsl.function { dependencies = [], constants = [] }
        """
        float pointAlpha(float pointRadius, vec2 pointCoord) {
            float pointSize = 2.0 * pointRadius;
            float x = (pointSize + 2.0) * (pointCoord.s - 0.5);
            float y = (pointSize + 2.0) * (pointCoord.t - 0.5);
            float r = sqrt(x * x + y * y);
            float innerRadius = pointRadius;
            float outerRadius = pointRadius + 1.0;
            if (r > outerRadius) {
                return 0.0;
            } else if (r > innerRadius) {
                return outerRadius - r;
            } else {
                return 1.0;
            }
        }
        """


getWorldPosition : Glsl.Function
getWorldPosition =
    Glsl.function { dependencies = [], constants = [] }
        """
        vec4 getWorldPosition(vec3 modelPosition, vec3 modelScale, mat4 modelMatrix) {
            vec4 scaledPosition = vec4(modelScale * modelPosition, 1.0);
            return modelMatrix * scaledPosition;
        }
        """


getWorldDirection : Glsl.Function
getWorldDirection =
    Glsl.function { dependencies = [], constants = [] }
        """
        vec3 getWorldDirection(vec3 modelDirection, mat4 modelMatrix) {
            return (modelMatrix * vec4(modelDirection, 0.0)).xyz;
        }
        """


getDirectionToLight : Glsl.Function
getDirectionToLight =
    Glsl.function
        { dependencies = []
        , constants = [ kDirectionalLightSource, kPointLightSource ]
        }
        """
        vec3 getDirectionToLight(vec3 surfacePosition, vec4 xyz_type, vec4 rgb_radius) {
            float lightSourceType = xyz_type.w;
            if (lightSourceType == kDirectionalLightSource) {
                return xyz_type.xyz;
            } else if (lightSourceType == kPointLightSource) {
                vec3 lightPosition = xyz_type.xyz;
                return normalize(lightPosition - surfacePosition);
            } else {
                return vec3(0.0, 0.0, 0.0);
            }
        }
        """


positiveDotProduct : Glsl.Function
positiveDotProduct =
    Glsl.function { dependencies = [], constants = [] }
        """
        float positiveDotProduct(vec3 v1, vec3 v2) {
            return clamp(dot(v1, v2), 0.0, 1.0);
        }
        """


softLightingLuminance : Glsl.Function
softLightingLuminance =
    Glsl.function { dependencies = [], constants = [] }
        """
        vec3 softLightingLuminance(
            vec3 aboveLuminance,
            vec3 belowLuminance,
            vec3 localUpDirection,
            vec3 localLightDirection
        ) {
            float sinElevation = dot(localLightDirection, localUpDirection);
            float t = (sinElevation + 1.0) / 2.0;
            return aboveLuminance * t + belowLuminance * (1.0 - t);
        }
        """


getDirectionToLightAndNormalIlluminance : Glsl.Function
getDirectionToLightAndNormalIlluminance =
    Glsl.function
        { dependencies = []
        , constants = [ kDirectionalLightSource, kPointLightSource, kPi ]
        }
        """
        void getDirectionToLightAndNormalIlluminance(
            vec4 xyz_type,
            vec4 rgb_radius,
            vec3 surfacePosition,
            out vec3 directionToLight,
            out vec3 normalIlluminance
        ) {
            float lightSourceType = xyz_type.w;
            if (lightSourceType == kDirectionalLightSource) {
                directionToLight = xyz_type.xyz;
                normalIlluminance = rgb_radius.rgb;
            } else if (lightSourceType == kPointLightSource) {
                vec3 lightSourcePosition = xyz_type.xyz;
                vec3 displacement = lightSourcePosition - surfacePosition;
                float distance = length(displacement);
                directionToLight = displacement / distance;
                normalIlluminance = rgb_radius.rgb / (4.0 * kPi * distance * distance);
            }
        }
        """


getDirectionToCamera : Glsl.Function
getDirectionToCamera =
    Glsl.function { dependencies = [], constants = [ kPerspectiveProjection ] }
        """
        vec3 getDirectionToCamera(vec3 surfacePosition, mat4 sceneProperties) {
            float projectionType = sceneProperties[1].w;
            if (projectionType == kPerspectiveProjection) {
                vec3 cameraPoint = sceneProperties[1].xyz;
                return normalize(cameraPoint - surfacePosition);
            } else {
                return sceneProperties[1].xyz;
            }
        }
        """


getChannelValue : Glsl.Function
getChannelValue =
    Glsl.function { dependencies = [], constants = [] }
        """
        float getChannelValue(sampler2D texture, vec2 uv, vec4 channel) {
            float constantValue = channel.a;
            float useConstant = float(channel.rgb == vec3(0.0, 0.0, 0.0));
            float useTexture = 1.0 - useConstant;
            float textureValue = dot(texture2D(texture, uv).rgb, channel.rgb);
            return clamp(textureValue * useTexture + constantValue * useConstant, 0.0, 1.0);
        }
        """


getLocalNormal : Glsl.Function
getLocalNormal =
    Glsl.function { dependencies = [], constants = [] }
        """
        vec3 getLocalNormal(sampler2D normalMap, float useNormalMap, vec2 uv) {
            vec3 rgb = useNormalMap * texture2D(normalMap, uv).rgb + (1.0 - useNormalMap) * vec3(0.5, 0.5, 1.0);
            float x = 2.0 * (rgb.r - 0.5);
            float y = 2.0 * (rgb.g - 0.5);
            float z = 2.0 * (rgb.b - 0.5);
            return normalize(vec3(-x, -y, z));
        }
        """


getNormalSign : Glsl.Function
getNormalSign =
    Glsl.function { dependencies = [], constants = [] }
        """
        float getNormalSign() {
            return 2.0 * float(gl_FrontFacing) - 1.0;
        }
        """


getMappedNormal : Glsl.Function
getMappedNormal =
    Glsl.function { dependencies = [], constants = [] }
        """
        vec3 getMappedNormal(vec3 normal, vec3 tangent, float normalSign, vec3 localNormal) {
            vec3 bitangent = cross(normal, tangent) * normalSign;
            return normalize(localNormal.x * tangent + localNormal.y * bitangent + localNormal.z * normal);
        }
        """


lambertianEnvironmentalLighting : Glsl.Function
lambertianEnvironmentalLighting =
    Glsl.function
        { dependencies = [ softLightingLuminance ]
        , constants = [ kPerspectiveProjection, kNoEnvironmentalLighting, kSoftLighting ]
        }
        """
        vec3 lambertianEnvironmentalLighting(
            vec3 normalDirection,
            vec3 materialColor,
            vec3 directionToCamera,
            mat4 viewMatrix,
            mat4 environmentalLighting
        ) {
            float environmentalLightingType = environmentalLighting[0][3];
            if (environmentalLightingType == kNoEnvironmentalLighting) {
                return vec3(0.0, 0.0, 0.0);
            } else if (environmentalLightingType == kSoftLighting) {
                vec3 upDirection = environmentalLighting[0].xyz;
                vec3 aboveLuminance = environmentalLighting[1].rgb;
                vec3 belowLuminance = environmentalLighting[2].rgb;
                vec3 luminance = softLightingLuminance(aboveLuminance, belowLuminance, upDirection, normalDirection);
                return luminance * materialColor;
            } else {
                return vec3(0.0, 0.0, 0.0); 
            }
        }
        """


lambertianLightSource : Glsl.Function
lambertianLightSource =
    Glsl.function
        { dependencies = [ getDirectionToLightAndNormalIlluminance, positiveDotProduct ]
        , constants = [ kDisabledLightSource, kPi ]
        }
        """
        vec3 lambertianLightSource(
            vec3 surfacePosition,
            vec3 surfaceNormal,
            vec3 materialColor,
            vec4 xyz_type,
            vec4 rgb_radius
        ) {
            float lightSourceType = xyz_type.w;
            if (lightSourceType == kDisabledLightSource) {
                return vec3(0.0, 0.0, 0.0);
            } 

            vec3 directionToLight = vec3(0.0, 0.0, 0.0);
            vec3 normalIlluminance = vec3(0.0, 0.0, 0.0);
            getDirectionToLightAndNormalIlluminance(
                xyz_type,
                rgb_radius,
                surfacePosition,
                directionToLight,
                normalIlluminance
            );

            float dotNL = positiveDotProduct(directionToLight, surfaceNormal);
            return (normalIlluminance * dotNL) * (materialColor / kPi);
        }
        """


lambertianDirectLighting : Glsl.Function
lambertianDirectLighting =
    Glsl.function
        { dependencies = [ lambertianLightSource ]
        , constants = []
        }
        """
        vec3 lambertianDirectLighting(
            vec3 surfacePosition,
            vec3 surfaceNormal,
            vec3 materialColor,
            mat4 lightSources12,
            mat4 lightSources34,
            mat4 lightSources56,
            mat4 lightSources78
        ) {
            vec3 litColor1 = lambertianLightSource(surfacePosition, surfaceNormal, materialColor, lightSources12[0], lightSources12[1]);
            vec3 litColor2 = lambertianLightSource(surfacePosition, surfaceNormal, materialColor, lightSources12[2], lightSources12[3]);
            vec3 litColor3 = lambertianLightSource(surfacePosition, surfaceNormal, materialColor, lightSources34[0], lightSources34[1]);
            vec3 litColor4 = lambertianLightSource(surfacePosition, surfaceNormal, materialColor, lightSources34[2], lightSources34[3]);
            vec3 litColor5 = lambertianLightSource(surfacePosition, surfaceNormal, materialColor, lightSources56[0], lightSources56[1]);
            vec3 litColor6 = lambertianLightSource(surfacePosition, surfaceNormal, materialColor, lightSources56[2], lightSources56[3]);
            vec3 litColor7 = lambertianLightSource(surfacePosition, surfaceNormal, materialColor, lightSources78[0], lightSources78[1]);
            vec3 litColor8 = lambertianLightSource(surfacePosition, surfaceNormal, materialColor, lightSources78[2], lightSources78[3]);
            return litColor1 + litColor2 + litColor3 + litColor4 + litColor5 + litColor6 + litColor7 + litColor8;
        }
        """


lambertianLighting : Glsl.Function
lambertianLighting =
    Glsl.function
        { dependencies = [ lambertianEnvironmentalLighting, lambertianDirectLighting ]
        , constants = []
        }
        """
        vec3 lambertianLighting(
            vec3 position,
            vec3 normalDirection,
            vec3 materialColor,
            vec3 directionToCamera,
            mat4 viewMatrix,
            mat4 environmentalLighting,
            mat4 lightSources12,
            mat4 lightSources34,
            mat4 lightSources56,
            mat4 lightSources78
        ) {
            vec3 environmentalContribution = lambertianEnvironmentalLighting(
                normalDirection,
                materialColor,
                directionToCamera,
                viewMatrix,
                environmentalLighting
            );
            vec3 directContribution = lambertianDirectLighting(
                position,
                normalDirection,
                materialColor,
                lightSources12,
                lightSources34,
                lightSources56,
                lightSources78
            );
            return environmentalContribution + directContribution;
        }
        """


safeQuotient : Glsl.Function
safeQuotient =
    Glsl.function { dependencies = [], constants = [] }
        """
        float safeQuotient(float numerator, float denominator) {
            if (denominator == 0.0) {
                return 0.0;
            } else {
                return numerator / denominator;
            }
        }
        """


specularD : Glsl.Function
specularD =
    Glsl.function { dependencies = [], constants = [ kPi, kMediumpFloatMax ] }
        """
        // Adapted from https://google.github.io/filament/Filament.md.html#materialsystem/specularbrdf/normaldistributionfunction(speculard)
        float specularD(float alpha, float dotNH, vec3 normalDirection, vec3 halfDirection) {
            vec3 crossNH = cross(normalDirection, halfDirection);
            float a = dotNH * alpha;
            float k = alpha / (dot(crossNH, crossNH) + a * a);
            float d = k * k * (1.0 / kPi);
            return min(d, kMediumpFloatMax);
        }
        """


g1 : Glsl.Function
g1 =
    Glsl.function { dependencies = [ safeQuotient ], constants = [] }
        """
        float g1(float dotNV, float alphaSquared) {
            return safeQuotient(2.0 * dotNV, dotNV + sqrt(alphaSquared + (1.0 - alphaSquared) * dotNV * dotNV));
        }
        """


specularG : Glsl.Function
specularG =
    Glsl.function { dependencies = [ g1 ], constants = [] }
        """
        float specularG(float dotNL, float dotNV, float alphaSquared) {
            return g1(dotNV, alphaSquared) * g1(dotNL, alphaSquared);
        }
        """


fresnelColor : Glsl.Function
fresnelColor =
    Glsl.function { dependencies = [], constants = [] }
        """
        vec3 fresnelColor(vec3 specularBaseColor, float dotVH) {
            vec3 one = vec3(1.0, 1.0, 1.0);
            float scale = exp2((-5.55473 * dotVH - 6.98316) * dotVH);
            return specularBaseColor + (one - specularBaseColor) * scale;
        }
        """


brdf : Glsl.Function
brdf =
    Glsl.function
        { dependencies = [ specularD, specularG, fresnelColor, safeQuotient ]
        , constants = []
        }
        """
        vec3 brdf(vec3 normalDirection, vec3 directionToCamera, vec3 directionToLight, float alpha, float dotNV, float dotNL, vec3 specularBaseColor, vec3 normalIlluminance) {
            vec3 halfDirection = normalize(directionToCamera + directionToLight);
            float dotVH = positiveDotProduct(directionToCamera, halfDirection);
            float dotNH = positiveDotProduct(normalDirection, halfDirection);
            float dotNHSquared = dotNH * dotNH;

            float d = specularD(alpha, dotNH, normalDirection, halfDirection);
            float g = specularG(dotNL, dotNV, alpha * alpha);
            vec3 f = fresnelColor(specularBaseColor, dotVH);
            return safeQuotient(d * g, 4.0 * dotNL * dotNV) * f;
        }
        """


sampleFacetNormal : Glsl.Function
sampleFacetNormal =
    Glsl.function { dependencies = [], constants = [] }
        """
        vec3 sampleFacetNormal(vec3 vH, vec3 vT1, vec3 vT2, float s, float alpha) {
            float t2 = (1.0 - s);
            vec3 vNh = t2 * vT2 + sqrt(max(0.0, 1.0 - t2 * t2)) * vH;
            return normalize(vec3(alpha * vNh.x, alpha * vNh.y, max(0.0, vNh.z)));
        }
        """


softLightingSpecularSample : Glsl.Function
softLightingSpecularSample =
    Glsl.function
        { dependencies = [ softLightingLuminance, positiveDotProduct, fresnelColor, g1 ]
        , constants = []
        }
        """
        vec3 softLightingSpecularSample(
            vec3 aboveLuminance,
            vec3 belowLuminance,
            vec3 localUpDirection,
            vec3 localViewDirection,
            vec3 localLightDirection,
            vec3 localHalfDirection,
            float alphaSquared,
            vec3 specularBaseColor
        ) {
            vec3 luminance = softLightingLuminance(aboveLuminance, belowLuminance, localUpDirection, localLightDirection);
            float dotVH = positiveDotProduct(localViewDirection, localHalfDirection);
            float dotNL = localLightDirection.z;
            return luminance * (fresnelColor(specularBaseColor, dotVH) * g1(dotNL, alphaSquared));
        }
        """


specularLightDirection : Glsl.Function
specularLightDirection =
    Glsl.function { dependencies = [], constants = [] }
        """
        vec3 specularLightDirection(vec3 v, vec3 h) {
            return (2.0 * dot(v, h)) * h - v;
        }
        """


physicalEnvironmentalLighting : Glsl.Function
physicalEnvironmentalLighting =
    Glsl.function
        { dependencies =
            [ sampleFacetNormal
            , specularLightDirection
            , softLightingSpecularSample
            , softLightingLuminance
            ]
        , constants = [ kNoEnvironmentalLighting, kSoftLighting ]
        }
        """
        vec3 physicalEnvironmentalLighting(
            vec3 normalDirection,
            vec3 diffuseBaseColor,
            vec3 specularBaseColor,
            float alpha,
            vec3 directionToCamera,
            mat4 viewMatrix,
            mat4 environmentalLighting
        ) {
            float environmentalLightingType = environmentalLighting[0].w;
            float alphaSquared = alpha * alpha;

            if (environmentalLightingType == kNoEnvironmentalLighting) {
                return vec3(0.0, 0.0, 0.0);
            } else if (environmentalLightingType == kSoftLighting) {
                vec3 upDirection = environmentalLighting[0].xyz;
                vec3 aboveLuminance = environmentalLighting[1].rgb;
                vec3 belowLuminance = environmentalLighting[2].rgb;
                vec3 crossProduct = cross(normalDirection, directionToCamera);
                float crossMagnitude = length(crossProduct);
                vec3 xDirection = vec3(0.0, 0.0, 0.0);
                vec3 yDirection = vec3(0.0, 0.0, 0.0);
                if (crossMagnitude > 1.0e-6) {
                    yDirection = (1.0 / crossMagnitude) * crossProduct;
                    xDirection = cross(yDirection, normalDirection);
                } else {
                    vec3 viewY = vec3(viewMatrix[0][1], viewMatrix[1][1], viewMatrix[2][1]);
                    xDirection = normalize(cross(viewY, normalDirection));
                    yDirection = cross(normalDirection, xDirection);
                }
                float localViewX = dot(directionToCamera, xDirection);
                float localViewZ = dot(directionToCamera, normalDirection);
                vec3 localViewDirection = vec3(localViewX, 0, localViewZ);
                float localUpX = dot(upDirection, xDirection);
                float localUpY = dot(upDirection, yDirection);
                float localUpZ = dot(upDirection, normalDirection);
                vec3 localUpDirection = vec3(localUpX, localUpY, localUpZ);

                vec3 vH = normalize(vec3(alpha * localViewX, 0.0, localViewZ));
                vec3 vT1 = vec3(0.0, 1.0, 0.0);
                vec3 vT2 = cross(vH, vT1);
                float s = 0.5 * (1.0 + vH.z);
                
                vec3 localHalfDirection = vec3(0.0, 0.0, 0.0);
                vec3 localLightDirection = vec3(0.0, 0.0, 0.0);
                
                localHalfDirection = sampleFacetNormal(vH, vT1, vT2, s, alpha);
                localLightDirection = specularLightDirection(localViewDirection, localHalfDirection);
                vec3 specular = softLightingSpecularSample(aboveLuminance, belowLuminance, localUpDirection, localViewDirection, localLightDirection, localHalfDirection, alphaSquared, specularBaseColor);
                
                localLightDirection = vec3(0.000000, 0.000000, 1.000000);
                vec3 diffuse = softLightingLuminance(aboveLuminance, belowLuminance, localUpDirection, localLightDirection) * localLightDirection.z;
                
                return specular + diffuse * diffuseBaseColor;
            } else {
                return vec3(0.0, 0.0, 0.0); 
            }
        }
        """


physicalLightSource : Glsl.Function
physicalLightSource =
    Glsl.function
        { dependencies =
            [ getDirectionToLightAndNormalIlluminance
            , positiveDotProduct
            , brdf
            ]
        , constants = [ kDisabledLightSource, kPi ]
        }
        """
        vec3 physicalLightSource(
            vec4 xyz_type,
            vec4 rgb_radius,
            vec3 surfacePosition,
            vec3 normalDirection,
            vec3 directionToCamera,
            float dotNV,
            vec3 diffuseBaseColor,
            vec3 specularBaseColor,
            float alpha
        ) {
            float lightSourceType = xyz_type.w;
            if (lightSourceType == kDisabledLightSource) {
                return vec3(0.0, 0.0, 0.0);
            }

            vec3 directionToLight = vec3(0.0, 0.0, 0.0);
            vec3 normalIlluminance = vec3(0.0, 0.0, 0.0);
            getDirectionToLightAndNormalIlluminance(xyz_type, rgb_radius, surfacePosition, directionToLight, normalIlluminance);

            float dotNL = positiveDotProduct(normalDirection, directionToLight);
            vec3 specularColor = brdf(normalDirection, directionToCamera, directionToLight, alpha, dotNV, dotNL, specularBaseColor, normalIlluminance);
            return (normalIlluminance * dotNL) * ((diffuseBaseColor / kPi) + specularColor);
        }
        """


physicalDirectLighting : Glsl.Function
physicalDirectLighting =
    Glsl.function { dependencies = [ physicalLightSource ], constants = [] }
        """
        vec3 physicalDirectLighting(
            vec3 surfacePosition,
            vec3 surfaceNormal,
            vec3 directionToCamera,
            vec3 diffuseBaseColor,
            vec3 specularBaseColor,
            float alpha,
            mat4 lightSources12,
            mat4 lightSources34,
            mat4 lightSources56,
            mat4 lightSources78
        ) {
            float dotNV = positiveDotProduct(surfaceNormal, directionToCamera);
            vec3 litColor1 = physicalLightSource(lightSources12[0], lightSources12[1], surfacePosition, surfaceNormal, directionToCamera, dotNV, diffuseBaseColor, specularBaseColor, alpha);
            vec3 litColor2 = physicalLightSource(lightSources12[2], lightSources12[3], surfacePosition, surfaceNormal, directionToCamera, dotNV, diffuseBaseColor, specularBaseColor, alpha);
            vec3 litColor3 = physicalLightSource(lightSources34[0], lightSources34[1], surfacePosition, surfaceNormal, directionToCamera, dotNV, diffuseBaseColor, specularBaseColor, alpha);
            vec3 litColor4 = physicalLightSource(lightSources34[2], lightSources34[3], surfacePosition, surfaceNormal, directionToCamera, dotNV, diffuseBaseColor, specularBaseColor, alpha);
            vec3 litColor5 = physicalLightSource(lightSources56[0], lightSources56[1], surfacePosition, surfaceNormal, directionToCamera, dotNV, diffuseBaseColor, specularBaseColor, alpha);
            vec3 litColor6 = physicalLightSource(lightSources56[2], lightSources56[3], surfacePosition, surfaceNormal, directionToCamera, dotNV, diffuseBaseColor, specularBaseColor, alpha);
            vec3 litColor7 = physicalLightSource(lightSources78[0], lightSources78[1], surfacePosition, surfaceNormal, directionToCamera, dotNV, diffuseBaseColor, specularBaseColor, alpha);
            vec3 litColor8 = physicalLightSource(lightSources78[2], lightSources78[3], surfacePosition, surfaceNormal, directionToCamera, dotNV, diffuseBaseColor, specularBaseColor, alpha);
            return litColor1 + litColor2 + litColor3 + litColor4 + litColor5 + litColor6 + litColor7 + litColor8;
        }
        """


physicalLighting : Glsl.Function
physicalLighting =
    Glsl.function
        { dependencies = [ physicalEnvironmentalLighting, physicalDirectLighting ]
        , constants = []
        }
        """
        vec3 physicalLighting(
            vec3 position,
            vec3 normalDirection,
            vec3 baseColor,
            vec3 directionToCamera,
            mat4 viewMatrix,
            float roughness,
            float metallic,
            mat4 environmentalLighting,
            mat4 lightSources12,
            mat4 lightSources34,
            mat4 lightSources56,
            mat4 lightSources78
        ) {
            float alpha = roughness * roughness;
            float nonmetallic = 1.0 - metallic;
            vec3 diffuseBaseColor = nonmetallic * 0.96 * baseColor;
            vec3 specularBaseColor = nonmetallic * 0.04 * vec3(1.0, 1.0, 1.0) + metallic * baseColor;

            vec3 environmentalContribution = physicalEnvironmentalLighting(
                normalDirection,
                diffuseBaseColor,
                specularBaseColor,
                alpha,
                directionToCamera,
                viewMatrix,
                environmentalLighting
            );

            vec3 directContribution = physicalDirectLighting(
                interpolatedPosition,
                normalDirection,
                directionToCamera,
                diffuseBaseColor,
                specularBaseColor,
                alpha,
                lightSources12,
                lightSources34,
                lightSources56,
                lightSources78
            );

            return environmentalContribution + directContribution;
        }
        """


getQuadVertex : Glsl.Function
getQuadVertex =
    Glsl.function { dependencies = [], constants = [] }
        """
        void getQuadVertex(int quadVertexIndex, mat4 quadVertexPositions, out vec3 position, out vec3 normal, out vec3 tangent) {
            vec3 next = vec3(0.0, 0.0, 0.0);
            vec3 prev = vec3(0.0, 0.0, 0.0);
            if (quadVertexIndex == 0) {
                prev = quadVertexPositions[3].xyz;
                position = quadVertexPositions[0].xyz;
                next = quadVertexPositions[1].xyz;
                tangent = normalize(next - position);
            } else if (quadVertexIndex == 1) {
                prev = quadVertexPositions[0].xyz;
                position = quadVertexPositions[1].xyz;
                next = quadVertexPositions[2].xyz;
                tangent = normalize(position - prev);
            } else if (quadVertexIndex == 2) {
                prev = quadVertexPositions[1].xyz;
                position = quadVertexPositions[2].xyz;
                next = quadVertexPositions[3].xyz;
                tangent = normalize(position - next);
            } else {
                prev = quadVertexPositions[2].xyz;
                position = quadVertexPositions[3].xyz;
                next = quadVertexPositions[0].xyz;
                tangent = normalize(prev - position);
            }
            normal = normalize(cross(next - position, prev - position));
        }
        """


shadowVertexPosition : Glsl.Function
shadowVertexPosition =
    Glsl.function
        { dependencies =
            [ getWorldPosition
            , getWorldDirection
            , getDirectionToLight
            , project
            ]
        , constants = []
        }
        """
        vec4 shadowVertexPosition(vec3 position, vec3 normal, mat4 shadowLightSource, vec3 modelScale, mat4 modelMatrix, mat4 viewMatrix, mat4 sceneProperties) {
            vec4 worldPosition = getWorldPosition(position, modelScale, modelMatrix);
            vec3 worldNormal = getWorldDirection(normal, modelMatrix);
            vec4 xyz_type = shadowLightSource[0];
            vec4 rgb_radius = shadowLightSource[1];
            vec3 directionToLight = getDirectionToLight(worldPosition.xyz, xyz_type, rgb_radius);
            vec3 offset = vec3(0.0, 0.0, 0.0);
            if (dot(directionToLight, worldNormal) <= 0.0) {
                offset = -1.0e9 * directionToLight;
            }
            vec4 offsetPosition = worldPosition + vec4(offset, 0.0);
            return project(viewMatrix * offsetPosition, sceneProperties[0]);
        }
        """


perpendicularTo : Glsl.Function
perpendicularTo =
    Glsl.function { dependencies = [], constants = [] }
        """
        vec3 perpendicularTo(vec3 d) {
            float absX = abs(d.x);
            float absY = abs(d.y);
            float absZ = abs(d.z);
            if (absX <= absY) {
                if (absX <= absZ) {
                    float scale = 1.0 / length(d.zy);
                    return vec3(0.0, -d.z * scale, d.y * scale);
                } else {
                    float scale = 1.0 / length(d.xy);
                    return vec3(-d.y * scale, d.x * scale, 0.0);
                }
            } else {
                if (absY <= absZ) {
                    float scale = 1.0 / length(d.xz);
                    return vec3(d.z * scale, 0.0, -d.x * scale);
                } else {
                    float scale = 1.0 / length(d.xy);
                    return vec3(-d.y * scale, d.x * scale, 0.0);
                }
            }
        }
        """


getSphericalUv : Glsl.Function
getSphericalUv =
    Glsl.function { dependencies = [], constants = [ kPi ] }
        """
        vec2 getSphericalUv(vec3 position, mat4 sphericalMap) {
            vec4 relative = sphericalMap * vec4(position, 1.0);
            float theta = atan(relative.y, relative.x);
            float u = clamp((theta + kPi) / (2 * kPi), 0.0, 1.0);
            float phi = atan(relative.z, length(relative.xy));
            float v = clamp((theta + kPi / 2.0) / kPi, 0.0, 1.0);
            return vec2(u, v);
        }
        """



---------- VERTEX SHADERS ----------


plainVertexShader : Glsl.Shader
plainVertexShader =
    Glsl.vertexShader "plainVertex"
        { attributes = [ position ]
        , uniforms = [ modelScale, modelMatrix, viewMatrix, sceneProperties ]
        , varyings = []
        , constants = []
        , functions = [ getWorldPosition, project ]
        }
        """
        void main () {
            vec4 worldPosition = getWorldPosition(position, modelScale, modelMatrix);
            gl_Position = project(viewMatrix * worldPosition, sceneProperties[0]);
        }
        """


unlitVertexShader : Glsl.Shader
unlitVertexShader =
    Glsl.vertexShader "unlitVertex"
        { attributes = [ position, uv ]
        , uniforms = [ modelScale, modelMatrix, viewMatrix, sceneProperties ]
        , varyings = [ interpolatedUv ]
        , constants = []
        , functions = [ getWorldPosition, project ]
        }
        """
        void main() {
            vec4 worldPosition = getWorldPosition(position, modelScale, modelMatrix);
            gl_Position = project(viewMatrix * worldPosition, sceneProperties[0]);
            interpolatedUv = uv;
        }
        """


planarMappedUnlitVertexShader : Glsl.Shader
planarMappedUnlitVertexShader =
    Glsl.vertexShader "planarMappedUnlitVertex"
        { attributes = [ position ]
        , uniforms = [ modelScale, modelMatrix, viewMatrix, sceneProperties, planarMap ]
        , varyings = [ interpolatedPosition, interpolatedUv ]
        , constants = []
        , functions = [ getWorldPosition, project ]
        }
        """
        void main() {
            vec4 worldPosition = getWorldPosition(position, modelScale, modelMatrix);
            gl_Position = project(viewMatrix * worldPosition, sceneProperties[0]);
            interpolatedPosition = worldPosition.xyz;
            interpolatedUv = planarMap * vec4(position, 1.0);
        }
        """


sphericalMappedUnlitVertexShader : Glsl.Shader
sphericalMappedUnlitVertexShader =
    Glsl.vertexShader "sphericalMappedUnlitVertex"
        { attributes = [ position ]
        , uniforms = [ modelScale, modelMatrix, viewMatrix, sceneProperties, sphericalMap ]
        , varyings = [ interpolatedPosition, interpolatedUv ]
        , constants = []
        , functions = [ getWorldPosition, project, getSphericalUv ]
        }
        """
        void main() {
            vec4 worldPosition = getWorldPosition(position, modelScale, modelMatrix);
            gl_Position = project(viewMatrix * worldPosition, sceneProperties[0]);
            interpolatedPosition = worldPosition.xyz;
            interpolatedUv = getSphericalUv(position, sphericalMap);
        }
        """


uniformVertexShader : Glsl.Shader
uniformVertexShader =
    Glsl.vertexShader "uniformVertex"
        { attributes = [ position, normal ]
        , uniforms = [ modelScale, modelMatrix, viewMatrix, sceneProperties ]
        , varyings = [ interpolatedPosition, interpolatedNormal ]
        , constants = []
        , functions = [ getWorldPosition, getWorldDirection, project ]
        }
        """
        void main () {
            vec4 worldPosition = getWorldPosition(position, modelScale, modelMatrix);
            gl_Position = project(viewMatrix * worldPosition, sceneProperties[0]);
            interpolatedPosition = worldPosition.xyz;
            interpolatedNormal = getWorldDirection(normal, modelMatrix);
        }
        """


texturedVertexShader : Glsl.Shader
texturedVertexShader =
    Glsl.vertexShader "texturedVertex"
        { attributes = [ position, normal, uv ]
        , uniforms = [ modelScale, modelMatrix, viewMatrix, sceneProperties ]
        , varyings =
            [ interpolatedPosition
            , interpolatedNormal
            , interpolatedUv
            , interpolatedTangent
            ]
        , constants = []
        , functions = [ getWorldPosition, getWorldDirection, project ]
        }
        """
        void main () {
            vec4 worldPosition = getWorldPosition(position, modelScale, modelMatrix);
            gl_Position = project(viewMatrix * worldPosition, sceneProperties[0]);
            interpolatedPosition = worldPosition.xyz;
            interpolatedNormal = getWorldDirection(normal, modelMatrix);
            interpolatedUv = uv;
            interpolatedTangent = vec3(0.0, 0.0, 0.0);
        }
        """


normalMappedVertexShader : Glsl.Shader
normalMappedVertexShader =
    Glsl.vertexShader "normalMappedVertex"
        { attributes = [ position, normal, uv, tangent ]
        , uniforms = [ modelScale, modelMatrix, viewMatrix, sceneProperties ]
        , varyings =
            [ interpolatedPosition
            , interpolatedNormal
            , interpolatedUv
            , interpolatedTangent
            ]
        , constants = []
        , functions = [ getWorldPosition, getWorldDirection, project ]
        }
        """
        void main () {
            vec4 worldPosition = getWorldPosition(position, modelScale, modelMatrix);
            gl_Position = project(viewMatrix * worldPosition, sceneProperties[0]);
            interpolatedPosition = worldPosition.xyz;
            interpolatedNormal = getWorldDirection(normal, modelMatrix);
            interpolatedUv = uv;
            interpolatedTangent = getWorldDirection(tangent, modelMatrix);
        }
        """


plainQuadVertexShader : Glsl.Shader
plainQuadVertexShader =
    Glsl.vertexShader "plainQuadVertex"
        { attributes = [ quadVertex ]
        , uniforms =
            [ modelScale
            , modelMatrix
            , viewMatrix
            , sceneProperties
            , quadVertexPositions
            ]
        , varyings = []
        , constants = []
        , functions = [ getQuadVertex, getWorldPosition, getWorldDirection, project ]
        }
        """
        void main() {
            vec3 position = vec3(0.0, 0.0, 0.0);
            vec3 normal = vec3(0.0, 0.0, 0.0);
            vec3 tangent = vec3(0.0, 0.0, 0.0);
            getQuadVertex(int(quadVertex.z), quadVertexPositions, position, normal, tangent);
            vec4 worldPosition = getWorldPosition(position, modelScale, modelMatrix);
            gl_Position = project(viewMatrix * worldPosition, sceneProperties[0]);
        }
        """


unlitQuadVertexShader : Glsl.Shader
unlitQuadVertexShader =
    Glsl.vertexShader "unlitQuadVertex"
        { attributes = [ quadVertex ]
        , uniforms =
            [ modelScale
            , modelMatrix
            , viewMatrix
            , sceneProperties
            , quadVertexPositions
            ]
        , varyings = [ interpolatedUv ]
        , constants = []
        , functions = [ getQuadVertex, getWorldPosition, getWorldDirection, project ]
        }
        """
        void main() {
            vec3 position = vec3(0.0, 0.0, 0.0);
            vec3 normal = vec3(0.0, 0.0, 0.0);
            vec3 tangent = vec3(0.0, 0.0, 0.0);
            getQuadVertex(int(quadVertex.z), quadVertexPositions, position, normal, tangent);
            vec4 worldPosition = getWorldPosition(position, modelScale, modelMatrix);
            gl_Position = project(viewMatrix * worldPosition, sceneProperties[0]);
            interpolatedUv = quadVertex.xy;
        }
        """


smoothQuadVertexShader : Glsl.Shader
smoothQuadVertexShader =
    Glsl.vertexShader "smoothQuadVertex"
        { attributes = [ quadVertex ]
        , uniforms =
            [ modelScale
            , modelMatrix
            , viewMatrix
            , sceneProperties
            , quadVertexPositions
            ]
        , varyings = [ interpolatedPosition, interpolatedNormal ]
        , constants = []
        , functions = [ getQuadVertex, getWorldPosition, getWorldDirection, project ]
        }
        """
        void main() {
            vec3 position = vec3(0.0, 0.0, 0.0);
            vec3 normal = vec3(0.0, 0.0, 0.0);
            vec3 tangent = vec3(0.0, 0.0, 0.0);
            getQuadVertex(int(quadVertex.z), quadVertexPositions, position, normal, tangent);
            vec4 worldPosition = getWorldPosition(position, modelScale, modelMatrix);
            gl_Position = project(viewMatrix * worldPosition, sceneProperties[0]);
            interpolatedPosition = worldPosition.xyz;
            interpolatedNormal = getWorldDirection(normal, modelMatrix);
        }
        """


texturedQuadVertexShader : Glsl.Shader
texturedQuadVertexShader =
    Glsl.vertexShader "texturedQuadVertex"
        { attributes = [ quadVertex ]
        , uniforms =
            [ modelScale
            , modelMatrix
            , viewMatrix
            , sceneProperties
            , quadVertexPositions
            ]
        , varyings = [ interpolatedPosition, interpolatedNormal, interpolatedUv, interpolatedTangent ]
        , constants = []
        , functions = [ getQuadVertex, getWorldPosition, getWorldDirection, project ]
        }
        """
        void main() {
            vec3 position = vec3(0.0, 0.0, 0.0);
            vec3 normal = vec3(0.0, 0.0, 0.0);
            vec3 tangent = vec3(0.0, 0.0, 0.0);
            getQuadVertex(int(quadVertex.z), quadVertexPositions, position, normal, tangent);
            vec4 worldPosition = getWorldPosition(position, modelScale, modelMatrix);
            gl_Position = project(viewMatrix * worldPosition, sceneProperties[0]);
            interpolatedPosition = worldPosition.xyz;
            interpolatedNormal = getWorldDirection(normal, modelMatrix);
            interpolatedUv = quadVertex.xy;
            interpolatedTangent = tangent;
        }
        """


pointVertexShader : Glsl.Shader
pointVertexShader =
    Glsl.vertexShader "pointVertex"
        { attributes = [ position ]
        , uniforms = [ modelScale, modelMatrix, pointRadius, viewMatrix, sceneProperties ]
        , varyings = []
        , constants = []
        , functions = [ getWorldPosition, project ]
        }
        """
        void main () {
            vec4 worldPosition = getWorldPosition(position, modelScale, modelMatrix);
            gl_Position = project(viewMatrix * worldPosition, sceneProperties[0]);
            float supersampling = sceneProperties[3][0];
            gl_PointSize = 2.0 * pointRadius * superSampling + 2.0;
        }
        """


shadowVertexShader : Glsl.Shader
shadowVertexShader =
    Glsl.vertexShader "shadowVertex"
        { attributes = [ position, normal ]
        , uniforms = [ modelScale, modelMatrix, viewMatrix, sceneProperties, shadowLightSource ]
        , varyings = []
        , constants = []
        , functions = [ shadowVertexPosition ]
        }
        """
        void main () {
            gl_Position = shadowVertexPosition(
                position,
                normal,
                shadowLightSource,
                modelScale,
                modelMatrix,
                viewMatrix,
                sceneProperties
            );
        }
        """


quadShadowVertexShader : Glsl.Shader
quadShadowVertexShader =
    Glsl.vertexShader "quadShadowVertex"
        { attributes = [ quadShadowVertex ]
        , uniforms =
            [ modelScale
            , modelMatrix
            , viewMatrix
            , sceneProperties
            , shadowLightSource
            , quadVertexPositions
            ]
        , varyings = []
        , constants = []
        , functions = [ getQuadVertex, shadowVertexPosition ]
        }
        """
        void main () {
            vec3 position = vec3(0.0, 0.0, 0.0);
            vec3 normal = vec3(0.0, 0.0, 0.0);
            vec3 tangent = vec3(0.0, 0.0, 0.0);
            getQuadVertex(int(quadShadowVertex.x), quadVertexPositions, position, normal, tangent);
            normal *= quadShadowVertex.y;
            gl_Position = shadowVertexPosition(
                position,
                normal,
                shadowLightSource,
                modelScale,
                modelMatrix,
                viewMatrix,
                sceneProperties
            );
        }
        """


sphereShadowVertexShader : Glsl.Shader
sphereShadowVertexShader =
    Glsl.vertexShader "sphereShadowVertex"
        { attributes = [ angle, offsetScale ]
        , uniforms =
            [ modelScale
            , modelMatrix
            , viewMatrix
            , sceneProperties
            , shadowLightSource
            ]
        , functions =
            [ getWorldPosition
            , getDirectionToLight
            , perpendicularTo
            , project
            ]
        , constants = [ kPerspectiveProjection ]
        , varyings = []
        }
        """
        void main () {
            vec4 worldCenter = getWorldPosition(vec3(0.0, 0.0, 0.0), modelScale, modelMatrix);
            vec4 xyz_type = shadowLightSource[0];
            vec4 rgb_radius = shadowLightSource[1];
            vec3 zDirection = getDirectionToLight(worldCenter.xyz, xyz_type, rgb_radius);
            vec3 xDirection = perpendicularTo(zDirection);
            vec3 yDirection = cross(zDirection, xDirection);
            float r = modelScale.x;
            float adjustedRadius = r;
            float zOffset = 0.0;
            if (xyz_type.w == kPointLightSource) {
                float distanceToLight = length(xyz_type.xyz - worldCenter.xyz);
                float rSquared = r * r;
                zOffset = rSquared / distanceToLight;
                float zSquared = zOffset * zOffset;
                adjustedRadius = sqrt(rSquared - zSquared);
            }
            vec3 worldPosition =
                worldCenter.xyz
                    + zDirection * zOffset
                    + xDirection * adjustedRadius * cos(angle)
                    + yDirection * adjustedRadius * sin(angle);
            vec3 directionToLight = getDirectionToLight(worldPosition, xyz_type, rgb_radius);
            vec3 offset = -1.0e9 * offsetScale * directionToLight;
            vec4 offsetPosition = vec4(worldPosition + offset, 1.0);
            gl_Position = project(viewMatrix * offsetPosition, sceneProperties[0]);
        }
        """



---------- FRAGMENT SHADERS ----------


shadowFragmentShader : Glsl.Shader
shadowFragmentShader =
    Glsl.fragmentShader "shadowFragment"
        { uniforms = []
        , varyings = []
        , constants = []
        , functions = []
        }
        """
        void main () {
            gl_FragColor = vec4(0.0, 0.0, 0.0, 1.0);
        }
        """


constantFragmentShader : Glsl.Shader
constantFragmentShader =
    Glsl.fragmentShader "constantFragment"
        { uniforms = [ constantColor ]
        , constants = []
        , varyings = []
        , functions = []
        }
        """
        void main () {
            gl_FragColor = vec4(constantColor, 1.0);
        }
        """


colorTextureFragmentShader : Glsl.Shader
colorTextureFragmentShader =
    Glsl.fragmentShader "colorTextureFragment"
        { uniforms = [ colorTexture ]
        , constants = []
        , varyings = [ interpolatedUv ]
        , functions = []
        }
        """
        void main () {
            gl_FragColor = texture2D(colorTexture, interpolatedUv);
        }
        """


constantPointFragmentShader : Glsl.Shader
constantPointFragmentShader =
    Glsl.fragmentShader "constantPointFragment"
        { uniforms = [ constantColor, pointRadius, sceneProperties ]
        , constants = []
        , varyings = []
        , functions = [ pointAlpha ]
        }
        """
        void main () {
            float supersampling = sceneProperties[3][0];
            float alpha = pointAlpha(pointRadius * supersampling, gl_PointCoord);
            gl_FragColor = vec4(constantColor, alpha);
        }
        """


emissiveFragmentShader : Glsl.Shader
emissiveFragmentShader =
    Glsl.fragmentShader "emissiveFragment"
        { uniforms = [ emissiveColor, sceneProperties ]
        , constants = []
        , varyings = []
        , functions = [ toSrgb ]
        }
        """
        void main () {
            gl_FragColor = toSrgb(emissiveColor, sceneProperties);
        }
        """


emissiveTextureFragmentShader : Glsl.Shader
emissiveTextureFragmentShader =
    Glsl.fragmentShader "emissiveTextureFragment"
        { uniforms = [ colorTexture, backlight, sceneProperties ]
        , varyings = [ interpolatedUv ]
        , constants = []
        , functions = [ fromSrgb, toSrgb ]
        }
        """
        void main () {
            vec3 emissiveColor = fromSrgb(texture2D(colorTexture, interpolatedUv).rgb) * backlight;
            gl_FragColor = toSrgb(emissiveColor, sceneProperties);
        }
        """


emissivePointFragmentShader : Glsl.Shader
emissivePointFragmentShader =
    Glsl.fragmentShader "emissivePointFragment"
        { uniforms = [ emissiveColor, pointRadius, sceneProperties ]
        , varyings = []
        , functions = [ toSrgb, pointAlpha ]
        , constants = []
        }
        """
        void main () {
            vec4 color = toSrgb(emissiveColor, sceneProperties);
            float supersampling = sceneProperties[3][0];
            float alpha = pointAlpha(pointRadius * supersampling, gl_PointCoord);
            gl_FragColor = vec4(color.rgb, alpha);
        }
        """


lambertianFragmentShader : Glsl.Shader
lambertianFragmentShader =
    Glsl.fragmentShader "lambertianFragment"
        { uniforms =
            [ sceneProperties
            , environmentalLighting
            , lightSources12
            , lightSources34
            , lightSources56
            , lightSources78
            , materialColor
            , viewMatrix
            ]
        , varyings = [ interpolatedPosition, interpolatedNormal ]
        , constants = []
        , functions =
            [ getNormalSign
            , getDirectionToCamera
            , lambertianLighting
            , toSrgb
            ]
        }
        """
        void main() {
            vec3 normalDirection = normalize(interpolatedNormal) * getNormalSign();
            vec3 directionToCamera = getDirectionToCamera(interpolatedPosition, sceneProperties);

            vec3 linearColor = lambertianLighting(
                interpolatedPosition,
                normalDirection,
                materialColor,
                directionToCamera,
                viewMatrix,
                environmentalLighting,
                lightSources12,
                lightSources34,
                lightSources56,
                lightSources78
            );

            gl_FragColor = toSrgb(linearColor, sceneProperties);
        }
        """


lambertianTextureFragmentShader : Glsl.Shader
lambertianTextureFragmentShader =
    Glsl.fragmentShader "lambertianTextureFragment"
        { uniforms =
            [ sceneProperties
            , environmentalLighting
            , lightSources12
            , lightSources34
            , lightSources56
            , lightSources78
            , materialColorTexture
            , normalMapTexture
            , useNormalMap
            , viewMatrix
            ]
        , varyings = [ interpolatedPosition, interpolatedNormal, interpolatedUv, interpolatedTangent ]
        , constants = []
        , functions =
            [ getLocalNormal
            , getNormalSign
            , getMappedNormal
            , getDirectionToCamera
            , lambertianLighting
            , fromSrgb
            , toSrgb
            ]
        }
        """
        void main() {
            vec3 localNormal = getLocalNormal(normalMapTexture, useNormalMap, interpolatedUv);
            float normalSign = getNormalSign();
            vec3 originalNormal = normalize(interpolatedNormal) * normalSign;
            vec3 normalDirection = getMappedNormal(originalNormal, interpolatedTangent, normalSign, localNormal);
            vec3 directionToCamera = getDirectionToCamera(interpolatedPosition, sceneProperties);
            vec3 materialColor = fromSrgb(texture2D(materialColorTexture, interpolatedUv).rgb);

            vec3 linearColor = lambertianLighting(
                interpolatedPosition,
                normalDirection,
                materialColor,
                directionToCamera,
                viewMatrix,
                environmentalLighting,
                lightSources12,
                lightSources34,
                lightSources56,
                lightSources78
            );

            gl_FragColor = toSrgb(linearColor, sceneProperties);
        }
        """


physicalFragmentShader : Glsl.Shader
physicalFragmentShader =
    Glsl.fragmentShader "physicalFragment"
        { uniforms =
            [ sceneProperties
            , environmentalLighting
            , viewMatrix
            , lightSources12
            , lightSources34
            , lightSources56
            , lightSources78
            , baseColor
            , roughness
            , metallic
            ]
        , varyings = [ interpolatedPosition, interpolatedNormal ]
        , functions =
            [ getNormalSign
            , getDirectionToCamera
            , physicalLighting
            , toSrgb
            ]
        , constants = []
        }
        """
        void main() {
            vec3 normalDirection = normalize(interpolatedNormal) * getNormalSign();
            vec3 directionToCamera = getDirectionToCamera(interpolatedPosition, sceneProperties);

            vec3 linearColor = physicalLighting(
                interpolatedPosition,
                normalDirection,
                baseColor,
                directionToCamera,
                viewMatrix,
                roughness,
                metallic,
                environmentalLighting,
                lightSources12,
                lightSources34,
                lightSources56,
                lightSources78
            );

            gl_FragColor = toSrgb(linearColor, sceneProperties);
        }
        """


physicalTexturesFragmentShader : Glsl.Shader
physicalTexturesFragmentShader =
    Glsl.fragmentShader "physicalTexturesFragment"
        { uniforms =
            [ sceneProperties
            , environmentalLighting
            , viewMatrix
            , lightSources12
            , lightSources34
            , lightSources56
            , lightSources78
            , baseColorTexture
            , constantBaseColor
            , roughnessTexture
            , roughnessChannel
            , metallicTexture
            , metallicChannel
            , normalMapTexture
            , useNormalMap
            ]
        , varyings =
            [ interpolatedPosition
            , interpolatedNormal
            , interpolatedUv
            , interpolatedTangent
            ]
        , functions =
            [ getChannelValue
            , getLocalNormal
            , getNormalSign
            , getMappedNormal
            , getDirectionToCamera
            , physicalLighting
            , fromSrgb
            , toSrgb
            ]
        , constants = []
        }
        """
        void main() {
            vec3 baseColor = fromSrgb(texture2D(baseColorTexture, interpolatedUv).rgb) * (1.0 - constantBaseColor.w) + constantBaseColor.rgb * constantBaseColor.w;
            float roughness = getChannelValue(roughnessTexture, interpolatedUv, roughnessChannel);
            float metallic = getChannelValue(metallicTexture, interpolatedUv, metallicChannel);

            vec3 localNormal = getLocalNormal(normalMapTexture, useNormalMap, interpolatedUv);
            float normalSign = getNormalSign();
            vec3 originalNormal = normalize(interpolatedNormal) * normalSign;
            vec3 normalDirection = getMappedNormal(originalNormal, interpolatedTangent, normalSign, localNormal);
            vec3 directionToCamera = getDirectionToCamera(interpolatedPosition, sceneProperties);

            vec3 linearColor = physicalLighting(
                interpolatedPosition,
                normalDirection,
                baseColor,
                directionToCamera,
                viewMatrix,
                roughness,
                metallic,
                environmentalLighting,
                lightSources12,
                lightSources34,
                lightSources56,
                lightSources78
            );

            gl_FragColor = toSrgb(linearColor, sceneProperties);
        }
        """



---------- SCRIPT ----------


fixSource : String -> String
fixSource input =
    let
        doRegex =
            Regex.fromString "\\bdo\\b"
                |> Maybe.withDefault Regex.never

        ifRegex =
            Regex.fromString "\\bif\\b(?!\\()"
                |> Maybe.withDefault Regex.never

        inRegex =
            Regex.fromString "\\bin\\b"
                |> Maybe.withDefault Regex.never
    in
    input
        |> Regex.replace doRegex (always "do_")
        |> Regex.replace ifRegex (always "if_")
        |> Regex.replace inRegex (always "in_")


optimizeShader : Script.UserPrivileges -> Glsl.Shader -> Script String Glsl.Shader
optimizeShader userPrivileges givenShader =
    Directory.createTemporary
        |> Script.thenWith
            (\tempDirectory ->
                let
                    inputFile =
                        File.in_ tempDirectory "input.glsl"

                    outputFile =
                        File.in_ tempDirectory "output.glsl"

                    shaderTypeString =
                        case Glsl.shaderType givenShader of
                            Glsl.VertexShader ->
                                "vertex"

                            Glsl.FragmentShader ->
                                "fragment"
                in
                Script.printLine ("Optimizing " ++ Glsl.shaderName givenShader)
                    |> Script.andThen (File.writeTo inputFile (Glsl.shaderSource givenShader))
                    |> Script.andThen
                        (Script.executeWith userPrivileges
                            { command = "glsl-minifier.cmd"
                            , arguments =
                                [ "-i"
                                , File.name inputFile
                                , "-o"
                                , File.name outputFile
                                , "-sT"
                                , shaderTypeString
                                , "-sV"
                                , "2" -- WebGL 1
                                ]
                            , workingDirectory = tempDirectory
                            }
                            |> Script.ignoreResult
                        )
                    |> Script.andThen (File.read outputFile)
                    |> Script.map fixSource
                    |> Script.thenWith
                        (\optimizedSource ->
                            Script.succeed (givenShader |> Glsl.setShaderSource optimizedSource)
                        )
            )


writeToFile : File Writable -> String -> Directory Writable -> UserPrivileges -> List Glsl.Shader -> Script String ()
writeToFile outputFile moduleName workingDirectory userPrivileges shaders =
    let
        contents =
            "-- Generated by scripts/GenerateShaders.elm, please do not edit by hand\n"
                ++ Glsl.generateModule moduleName shaders
    in
    Script.printLine ("Writing " ++ File.name outputFile ++ "...")
        |> Script.andThen (File.write contents outputFile)
        |> Script.andThen (Script.printLine "Formatting output file with elm-format...")
        |> Script.andThen
            (Script.executeWith userPrivileges
                { command = "elm-format"
                , arguments = [ "--yes", File.path outputFile ]
                , workingDirectory = workingDirectory
                }
                |> Script.ignoreResult
                |> Script.onError (\_ -> Script.fail "Error when running elm-format")
            )
        |> Script.andThen (Script.printLine "Test compiling output file...")
        |> Script.andThen
            (Script.executeWith userPrivileges
                { command = "elm"
                , arguments = [ "make", "--output=/dev/null", File.path outputFile ]
                , workingDirectory = workingDirectory
                }
                |> Script.ignoreResult
                |> Script.onError (\_ -> Script.fail "Error when running 'elm make'")
            )
        |> Script.andThen (Script.printLine "Success!")


script : Script.Init -> Script String ()
script { workingDirectory, userPrivileges } =
    let
        optimizedOutputFile =
            File.writable userPrivileges "../src/Scene3d/OptimizedShaders.elm"

        unoptimizedOutputFile =
            File.writable userPrivileges "../src/Scene3d/UnoptimizedShaders.elm"

        shaders =
            [ plainVertexShader
            , unlitVertexShader
            , uniformVertexShader
            , texturedVertexShader
            , normalMappedVertexShader
            , plainQuadVertexShader
            , unlitQuadVertexShader
            , smoothQuadVertexShader
            , texturedQuadVertexShader
            , pointVertexShader
            , shadowVertexShader
            , quadShadowVertexShader
            , sphereShadowVertexShader
            , shadowFragmentShader
            , constantFragmentShader
            , colorTextureFragmentShader
            , constantPointFragmentShader
            , emissiveFragmentShader
            , emissiveTextureFragmentShader
            , emissivePointFragmentShader
            , lambertianFragmentShader
            , lambertianTextureFragmentShader
            , physicalFragmentShader
            , physicalTexturesFragmentShader
            ]
    in
    Script.printLine "Optimizing shaders..."
        |> Script.andThen (Script.collect (optimizeShader userPrivileges) shaders)
        |> Script.thenWith
            (\optimizedShaders ->
                Script.do
                    [ writeToFile optimizedOutputFile "Scene3d.OptimizedShaders" workingDirectory userPrivileges optimizedShaders
                    , writeToFile unoptimizedOutputFile "Scene3d.UnoptimizedShaders" workingDirectory userPrivileges shaders
                    ]
            )


main : Script.Program
main =
    Common.program script
