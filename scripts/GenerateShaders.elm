module GenerateShaders exposing (main)

import Common exposing (handleError)
import Glsl
import Script exposing (Script)
import Script.Directory as Directory
import Script.File as File



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


quadVertex : Glsl.Attribute
quadVertex =
    Glsl.attribute Glsl.vec3 "quadVertex"


quadShadowVertex : Glsl.Attribute
quadShadowVertex =
    Glsl.attribute Glsl.vec2 "quadShadowVertex"



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


constantRoughness : Glsl.Uniform
constantRoughness =
    Glsl.uniform Glsl.vec2 "constantRoughness"


metallic : Glsl.Uniform
metallic =
    Glsl.uniform Glsl.float "metallic"


metallicTexture : Glsl.Uniform
metallicTexture =
    Glsl.uniform Glsl.sampler2D "metallicTexture"


constantMetallic : Glsl.Uniform
constantMetallic =
    Glsl.uniform Glsl.vec2 "constantMetallic"


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


toSrgb : Glsl.Function
toSrgb =
    Glsl.function { dependencies = [ gammaCorrect ], constants = [] }
        """
        vec4 toSrgb(vec3 linearColor, mat4 sceneProperties) {
            vec3 referenceWhite = sceneProperties[2].rgb;
            float red = gammaCorrect(linearColor.r / referenceWhite.r);
            float green = gammaCorrect(linearColor.g / referenceWhite.g);
            float blue = gammaCorrect(linearColor.b / referenceWhite.b);
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


getWorldNormal : Glsl.Function
getWorldNormal =
    Glsl.function { dependencies = [], constants = [] }
        """
        vec3 getWorldNormal(vec3 modelNormal, mat4 modelMatrix) {
            return (modelMatrix * vec4(modelNormal, 0.0)).xyz;
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
                float localUpX = dot(upDirection, xDirection);
                float localUpY = dot(upDirection, yDirection);
                float localUpZ = dot(upDirection, normalDirection);
                vec3 localUpDirection = vec3(localUpX, localUpY, localUpZ);
                
                float numSamples = 13.0;
                vec3 sum = vec3(0.0, 0.0, 0.0);

                vec3 localLightDirection = vec3(0.000000, 0.000000, 1.000000);
                sum += softLightingLuminance(aboveLuminance, belowLuminance, localUpDirection, localLightDirection) * localLightDirection.z;

                localLightDirection = vec3(0.606266, 0.000000, 0.795262);
                sum += softLightingLuminance(aboveLuminance, belowLuminance, localUpDirection, localLightDirection) * localLightDirection.z;
                
                localLightDirection = vec3(0.000000, 0.606266, 0.795262);
                sum += softLightingLuminance(aboveLuminance, belowLuminance, localUpDirection, localLightDirection) * localLightDirection.z;
                
                localLightDirection = vec3(-0.606266, 0.000000, 0.795262);
                sum += softLightingLuminance(aboveLuminance, belowLuminance, localUpDirection, localLightDirection) * localLightDirection.z;
                
                localLightDirection = vec3(0.000000, -0.606266, 0.795262);
                sum += softLightingLuminance(aboveLuminance, belowLuminance, localUpDirection, localLightDirection) * localLightDirection.z;
                
                localLightDirection = vec3(0.873598, 0.361856, 0.325402);
                sum += softLightingLuminance(aboveLuminance, belowLuminance, localUpDirection, localLightDirection) * localLightDirection.z;
                
                localLightDirection = vec3(0.361856, 0.873598, 0.325402);
                sum += softLightingLuminance(aboveLuminance, belowLuminance, localUpDirection, localLightDirection) * localLightDirection.z;
                
                localLightDirection = vec3(-0.361856, 0.873598, 0.325402);
                sum += softLightingLuminance(aboveLuminance, belowLuminance, localUpDirection, localLightDirection) * localLightDirection.z;
                
                localLightDirection = vec3(-0.873598, 0.361856, 0.325402);
                sum += softLightingLuminance(aboveLuminance, belowLuminance, localUpDirection, localLightDirection) * localLightDirection.z;
                
                localLightDirection = vec3(-0.873598, -0.361856, 0.325402);
                sum += softLightingLuminance(aboveLuminance, belowLuminance, localUpDirection, localLightDirection) * localLightDirection.z;
                
                localLightDirection = vec3(-0.361856, -0.873598, 0.325402);
                sum += softLightingLuminance(aboveLuminance, belowLuminance, localUpDirection, localLightDirection) * localLightDirection.z;
                
                localLightDirection = vec3(0.361856, -0.873598, 0.325402);
                sum += softLightingLuminance(aboveLuminance, belowLuminance, localUpDirection, localLightDirection) * localLightDirection.z;

                localLightDirection = vec3(0.873598, -0.361856, 0.325402);
                sum += softLightingLuminance(aboveLuminance, belowLuminance, localUpDirection, localLightDirection) * localLightDirection.z;
                
                return (2.0 * sum * materialColor) / numSamples;
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
        vec3 sampleFacetNormal(float t1, float t2, vec3 vH, vec3 vT1, vec3 vT2, float s, float alpha) {
            t2 = (1.0 - s) * sqrt(1.0 - t1 * t1) + s * t2;
            vec3 vNh = t1 * vT1 + t2 * vT2 + sqrt(max(0.0, 1.0 - t1 * t1 - t2 * t2)) * vH;
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
            }

            if (environmentalLightingType == kSoftLighting) {
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
                float numSamples = 13.0;
                
                vec3 specularSum = vec3(0.0, 0.0, 0.0);

                localHalfDirection = sampleFacetNormal(0.000000, 0.000000, vH, vT1, vT2, s, alpha);
                localLightDirection = specularLightDirection(localViewDirection, localHalfDirection);
                specularSum += softLightingSpecularSample(aboveLuminance, belowLuminance, localUpDirection, localViewDirection, localLightDirection, localHalfDirection, alphaSquared, specularBaseColor);
                
                localHalfDirection = sampleFacetNormal(0.448762, 0.000000, vH, vT1, vT2, s, alpha);
                localLightDirection = specularLightDirection(localViewDirection, localHalfDirection);
                specularSum += softLightingSpecularSample(aboveLuminance, belowLuminance, localUpDirection, localViewDirection, localLightDirection, localHalfDirection, alphaSquared, specularBaseColor);

                localHalfDirection = sampleFacetNormal(0.000000, 0.448762, vH, vT1, vT2, s, alpha);
                localLightDirection = specularLightDirection(localViewDirection, localHalfDirection);
                specularSum += softLightingSpecularSample(aboveLuminance, belowLuminance, localUpDirection, localViewDirection, localLightDirection, localHalfDirection, alphaSquared, specularBaseColor);
                
                localHalfDirection = sampleFacetNormal(-0.448762, 0.000000, vH, vT1, vT2, s, alpha);
                localLightDirection = specularLightDirection(localViewDirection, localHalfDirection);
                specularSum += softLightingSpecularSample(aboveLuminance, belowLuminance, localUpDirection, localViewDirection, localLightDirection, localHalfDirection, alphaSquared, specularBaseColor);
                
                localHalfDirection = sampleFacetNormal(0.000000, -0.448762, vH, vT1, vT2, s, alpha);
                localLightDirection = specularLightDirection(localViewDirection, localHalfDirection);
                specularSum += softLightingSpecularSample(aboveLuminance, belowLuminance, localUpDirection, localViewDirection, localLightDirection, localHalfDirection, alphaSquared, specularBaseColor);
                
                localHalfDirection = sampleFacetNormal(0.748423, 0.310007, vH, vT1, vT2, s, alpha);
                localLightDirection = specularLightDirection(localViewDirection, localHalfDirection);
                specularSum += softLightingSpecularSample(aboveLuminance, belowLuminance, localUpDirection, localViewDirection, localLightDirection, localHalfDirection, alphaSquared, specularBaseColor);
                
                localHalfDirection = sampleFacetNormal(0.310007, 0.748423, vH, vT1, vT2, s, alpha);
                localLightDirection = specularLightDirection(localViewDirection, localHalfDirection);
                specularSum += softLightingSpecularSample(aboveLuminance, belowLuminance, localUpDirection, localViewDirection, localLightDirection, localHalfDirection, alphaSquared, specularBaseColor);
                
                localHalfDirection = sampleFacetNormal(-0.310007, 0.748423, vH, vT1, vT2, s, alpha);
                localLightDirection = specularLightDirection(localViewDirection, localHalfDirection);
                specularSum += softLightingSpecularSample(aboveLuminance, belowLuminance, localUpDirection, localViewDirection, localLightDirection, localHalfDirection, alphaSquared, specularBaseColor);
                
                localHalfDirection = sampleFacetNormal(-0.748423, 0.310007, vH, vT1, vT2, s, alpha);
                localLightDirection = specularLightDirection(localViewDirection, localHalfDirection);
                specularSum += softLightingSpecularSample(aboveLuminance, belowLuminance, localUpDirection, localViewDirection, localLightDirection, localHalfDirection, alphaSquared, specularBaseColor);
                
                localHalfDirection = sampleFacetNormal(-0.748423, -0.310007, vH, vT1, vT2, s, alpha);
                localLightDirection = specularLightDirection(localViewDirection, localHalfDirection);
                specularSum += softLightingSpecularSample(aboveLuminance, belowLuminance, localUpDirection, localViewDirection, localLightDirection, localHalfDirection, alphaSquared, specularBaseColor);
                
                localHalfDirection = sampleFacetNormal(-0.310007, -0.748423, vH, vT1, vT2, s, alpha);
                localLightDirection = specularLightDirection(localViewDirection, localHalfDirection);
                specularSum += softLightingSpecularSample(aboveLuminance, belowLuminance, localUpDirection, localViewDirection, localLightDirection, localHalfDirection, alphaSquared, specularBaseColor);
                
                localHalfDirection = sampleFacetNormal(0.310007, -0.748423, vH, vT1, vT2, s, alpha);
                localLightDirection = specularLightDirection(localViewDirection, localHalfDirection);
                specularSum += softLightingSpecularSample(aboveLuminance, belowLuminance, localUpDirection, localViewDirection, localLightDirection, localHalfDirection, alphaSquared, specularBaseColor);
                
                localHalfDirection = sampleFacetNormal(0.748423, -0.310007, vH, vT1, vT2, s, alpha);
                localLightDirection = specularLightDirection(localViewDirection, localHalfDirection);
                specularSum += softLightingSpecularSample(aboveLuminance, belowLuminance, localUpDirection, localViewDirection, localLightDirection, localHalfDirection, alphaSquared, specularBaseColor);
                
                vec3 diffuseSum = vec3(0.0, 0.0, 0.0);

                localLightDirection = vec3(0.000000, 0.000000, 1.000000);
                diffuseSum += softLightingLuminance(aboveLuminance, belowLuminance, localUpDirection, localLightDirection) * localLightDirection.z;

                localLightDirection = vec3(0.606266, 0.000000, 0.795262);
                diffuseSum += softLightingLuminance(aboveLuminance, belowLuminance, localUpDirection, localLightDirection) * localLightDirection.z;
                
                localLightDirection = vec3(0.000000, 0.606266, 0.795262);
                diffuseSum += softLightingLuminance(aboveLuminance, belowLuminance, localUpDirection, localLightDirection) * localLightDirection.z;
                
                localLightDirection = vec3(-0.606266, 0.000000, 0.795262);
                diffuseSum += softLightingLuminance(aboveLuminance, belowLuminance, localUpDirection, localLightDirection) * localLightDirection.z;
                
                localLightDirection = vec3(0.000000, -0.606266, 0.795262);
                diffuseSum += softLightingLuminance(aboveLuminance, belowLuminance, localUpDirection, localLightDirection) * localLightDirection.z;
                
                localLightDirection = vec3(0.873598, 0.361856, 0.325402);
                diffuseSum += softLightingLuminance(aboveLuminance, belowLuminance, localUpDirection, localLightDirection) * localLightDirection.z;
                
                localLightDirection = vec3(0.361856, 0.873598, 0.325402);
                diffuseSum += softLightingLuminance(aboveLuminance, belowLuminance, localUpDirection, localLightDirection) * localLightDirection.z;
                
                localLightDirection = vec3(-0.361856, 0.873598, 0.325402);
                diffuseSum += softLightingLuminance(aboveLuminance, belowLuminance, localUpDirection, localLightDirection) * localLightDirection.z;
                
                localLightDirection = vec3(-0.873598, 0.361856, 0.325402);
                diffuseSum += softLightingLuminance(aboveLuminance, belowLuminance, localUpDirection, localLightDirection) * localLightDirection.z;
                
                localLightDirection = vec3(-0.873598, -0.361856, 0.325402);
                diffuseSum += softLightingLuminance(aboveLuminance, belowLuminance, localUpDirection, localLightDirection) * localLightDirection.z;
                
                localLightDirection = vec3(-0.361856, -0.873598, 0.325402);
                diffuseSum += softLightingLuminance(aboveLuminance, belowLuminance, localUpDirection, localLightDirection) * localLightDirection.z;
                
                localLightDirection = vec3(0.361856, -0.873598, 0.325402);
                diffuseSum += softLightingLuminance(aboveLuminance, belowLuminance, localUpDirection, localLightDirection) * localLightDirection.z;

                localLightDirection = vec3(0.873598, -0.361856, 0.325402);
                diffuseSum += softLightingLuminance(aboveLuminance, belowLuminance, localUpDirection, localLightDirection) * localLightDirection.z;
                
                return (specularSum + 2.0 * diffuseSum * diffuseBaseColor) / numSamples;
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
        void getQuadVertex(int quadVertexIndex, mat4 quadVertexPositions, out vec3 position, out vec3 normal) {
            vec3 next = vec3(0.0, 0.0, 0.0);
            vec3 prev = vec3(0.0, 0.0, 0.0);
            if (quadVertexIndex == 0) {
                prev = quadVertexPositions[3].xyz;
                position = quadVertexPositions[0].xyz;
                next = quadVertexPositions[1].xyz;
            } else if (quadVertexIndex == 1) {
                prev = quadVertexPositions[0].xyz;
                position = quadVertexPositions[1].xyz;
                next = quadVertexPositions[2].xyz;
            } else if (quadVertexIndex == 2) {
                prev = quadVertexPositions[1].xyz;
                position = quadVertexPositions[2].xyz;
                next = quadVertexPositions[3].xyz;
            } else {
                prev = quadVertexPositions[2].xyz;
                position = quadVertexPositions[3].xyz;
                next = quadVertexPositions[0].xyz;
            }
            normal = normalize(cross(next - position, prev - position));
        }
        """


shadowVertexPosition : Glsl.Function
shadowVertexPosition =
    Glsl.function
        { dependencies =
            [ getWorldPosition
            , getWorldNormal
            , getDirectionToLight
            , project
            ]
        , constants = []
        }
        """
        vec4 shadowVertexPosition(vec3 position, vec3 normal, mat4 shadowLightSource, vec3 modelScale, mat4 modelMatrix, mat4 viewMatrix, mat4 sceneProperties) {
            vec4 worldPosition = getWorldPosition(position, modelScale, modelMatrix);
            vec3 worldNormal = getWorldNormal(normal, modelMatrix);
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



---------- VERTEX SHADERS ----------


plainVertexShader : Glsl.Shader
plainVertexShader =
    Glsl.vertexShader "plainVertex"
        { attributes = [ position ]
        , uniforms = [ modelScale, modelMatrix, viewMatrix, sceneProperties ]
        , varyings = [ interpolatedPosition ]
        , constants = []
        , functions = [ getWorldPosition, project ]
        }
        """
        void main () {
            vec4 worldPosition = getWorldPosition(position, modelScale, modelMatrix);
            gl_Position = project(viewMatrix * worldPosition, sceneProperties[0]);
            interpolatedPosition = worldPosition.xyz;
        }
        """


texturedVertexShader : Glsl.Shader
texturedVertexShader =
    Glsl.vertexShader "texturedVertex"
        { attributes = [ position, uv ]
        , uniforms = [ modelScale, modelMatrix, viewMatrix, sceneProperties ]
        , varyings = [ interpolatedPosition, interpolatedUv ]
        , constants = []
        , functions = [ getWorldPosition, project ]
        }
        """
        void main() {
            vec4 worldPosition = getWorldPosition(position, modelSale, modelMatrix);
            gl_Position = project(viewMatrix * worldPosition, sceneProperties[0]);
            interpolatedPosition = worldPosition.xyz;
            interpolatedUv = uv;
        }
        """


quadVertexShader : Glsl.Shader
quadVertexShader =
    Glsl.vertexShader "quadVertex"
        { attributes = [ quadVertex ]
        , uniforms =
            [ modelScale
            , modelMatrix
            , viewMatrix
            , sceneProperties
            , quadVertexPositions
            ]
        , varyings = [ interpolatedPosition ]
        , constants = []
        , functions = [ getQuadVertex, getWorldPosition, project ]
        }
        """
        void main() {
            vec3 position = vec3(0.0, 0.0, 0.0);
            vec3 normal = vec3(0.0, 0.0, 0.0);
            getQuadVertex(int(quadVertex.z), quadVertexPositions, position, normal);
            vec4 worldPosition = getWorldPosition(position, modelScale, modelMatrix);
            gl_Position = project(viewMatrix * worldPosition, sceneProperties[0]);
            interpolatedPosition = worldPosition.xyz;
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
        , functions = [ getQuadVertex, getWorldPosition, getWorldNormal, project ]
        }
        """
        void main() {
            vec3 position = vec3(0.0, 0.0, 0.0);
            vec3 normal = vec3(0.0, 0.0, 0.0);
            getQuadVertex(int(quadVertex.z), quadVertexPositions, position, normal);
            vec4 worldPosition = getWorldPosition(position, modelScale, modelMatrix);
            gl_Position = project(viewMatrix * worldPosition, sceneProperties[0]);
            interpolatedPosition = worldPosition.xyz;
            interpolatedNormal = getWorldNormal(normal, modelMatrix);
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
        , varyings = [ interpolatedPosition, interpolatedUv ]
        , constants = []
        , functions = [ getQuadVertex, getWorldPosition, project ]
        }
        """
        void main() {
            vec3 position = vec3(0.0, 0.0, 0.0);
            vec3 normal = vec3(0.0, 0.0, 0.0);
            getQuadVertex(int(quadVertex.z), quadVertexPositions, position, normal);
            vec4 worldPosition = getWorldPosition(position, modelScale, modelMatrix);
            gl_Position = project(viewMatrix * worldPosition, sceneProperties[0]);
            interpolatedPosition = worldPosition.xyz;
            interpolatedUv = quadVertex.xy;
        }
        """


smoothTexturedQuadVertexShader : Glsl.Shader
smoothTexturedQuadVertexShader =
    Glsl.vertexShader "smoothTexturedQuadVertex"
        { attributes = [ quadVertex ]
        , uniforms =
            [ modelScale
            , modelMatrix
            , viewMatrix
            , sceneProperties
            , quadVertexPositions
            ]
        , varyings = [ interpolatedPosition, interpolatedNormal, interpolatedUv ]
        , constants = []
        , functions = [ getQuadVertex, getWorldPosition, getWorldNormal, project ]
        }
        """
        void main() {
            vec3 position = vec3(0.0, 0.0, 0.0);
            vec3 normal = vec3(0.0, 0.0, 0.0);
            getQuadVertex(int(quadVertex.z), quadVertexPositions, position, normal);
            vec4 worldPosition = getWorldPosition(position, modelScale, modelMatrix);
            gl_Position = project(viewMatrix * worldPosition, sceneProperties[0]);
            interpolatedPosition = worldPosition.xyz;
            interpolatedNormal = getWorldNormal(normal, modelMatrix);
            interpolatedUv = quadVertex.xy;
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
            gl_PointSize = 2.0 * pointRadius + 2.0;
        }
        """


smoothVertexShader : Glsl.Shader
smoothVertexShader =
    Glsl.vertexShader "smoothVertex"
        { attributes = [ position, normal ]
        , uniforms = [ modelScale, modelMatrix, viewMatrix, sceneProperties ]
        , varyings = [ interpolatedPosition, interpolatedNormal ]
        , constants = []
        , functions = [ getWorldPosition, getWorldNormal, project ]
        }
        """
        void main () {
            vec4 worldPosition = getWorldPosition(position, modelScale, modelMatrix);
            gl_Position = project(viewMatrix * worldPosition, sceneProperties[0]);
            interpolatedPosition = worldPosition.xyz;
            interpolatedNormal = getWorldNormal(normal, modelMatrix);
        }
        """


smoothTexturedVertexShader : Glsl.Shader
smoothTexturedVertexShader =
    Glsl.vertexShader "smoothTexturedVertex"
        { attributes = [ position, normal, uv ]
        , uniforms = [ modelScale, modelMatrix, viewMatrix, sceneProperties ]
        , varyings = [ interpolatedPosition, interpolatedNormal, interpolatedUv ]
        , constants = []
        , functions = [ getWorldPosition, getWorldNormal, project ]
        }
        """
        void main () {
            vec4 worldPosition = getWorldPosition(position, modelScale, modelMatrix);
            gl_Position = project(viewMatrix * worldPosition, sceneProperties[0]);
            interpolatedPosition = worldPosition.xyz;
            interpolatedNormal = getWorldNormal(normal, modelMatrix);
            interpolatedUv = uv;
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
        , functions = [ project, getDirectionToLight ]
        }
        """
        void main () {
            vec3 position = vec3(0.0, 0.0, 0.0);
            vec3 normal = vec3(0.0, 0.0, 0.0);
            getQuadVertex(int(quadShadowVertex.x), quadVertexPositions, position, normal);
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
        , varyings = [ interpolatedPosition ]
        , constants = []
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
        , varyings = [ interpolatedPosition, interpolatedUv ]
        , constants = []
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
        , varyings = []
        , constants = []
        , functions = [ pointAlpha ]
        }
        """
        void main () {
            float alpha = pointAlpha(pointRadius, gl_PointCoord);
            gl_FragColor = vec4(constantColor, alpha);
        }
        """


emissiveFragmentShader : Glsl.Shader
emissiveFragmentShader =
    Glsl.fragmentShader "emissiveFragment"
        { uniforms = [ emissiveColor, sceneProperties ]
        , varyings = [ interpolatedPosition ]
        , constants = []
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
        , varyings = [ interpolatedPosition, interpolatedUv ]
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
        , functions = [ toSrgb ]
        , constants = []
        }
        """
        void main () {
            vec3 color = toSrgb(emissiveColor, sceneProperties);
            float alpha = pointAlpha(pointRadius, gl_PointCoord);
            gl_FragColor = vec4(color, alpha);
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
        , varyings =
            [ interpolatedPosition
            , interpolatedNormal
            ]
        , constants = []
        , functions =
            [ getDirectionToCamera
            , lambertianLighting
            , toSrgb
            ]
        }
        """
        void main() {
            vec3 normalDirection = normalize(interpolatedNormal);
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
            , viewMatrix
            ]
        , varyings =
            [ interpolatedPosition
            , interpolatedNormal
            , interpolatedUv
            ]
        , constants = []
        , functions =
            [ getDirectionToCamera
            , lambertianEnvironmentalLighting
            , lambertianDirectLighting
            , fromSrgb
            , toSrgb
            ]
        }
        """
        void main() {
            vec3 normalDirection = normalize(interpolatedNormal);
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
        , varyings =
            [ interpolatedPosition
            , interpolatedNormal
            ]
        , functions =
            [ getDirectionToCamera
            , physicalLighting
            , toSrgb
            ]
        , constants = []
        }
        """
        void main() {
            vec3 normalDirection = normalize(interpolatedNormal);
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
            , constantRoughness
            , metallicTexture
            , constantMetallic
            ]
        , varyings =
            [ interpolatedPosition
            , interpolatedNormal
            , interpolatedUv
            ]
        , functions =
            [ getDirectionToCamera
            , physicalLighting
            , fromSrgb
            , toSrgb
            ]
        , constants = []
        }
        """
        void main() {
            vec3 baseColor = fromSrgb(texture2D(baseColorTexture, interpolatedUv).rgb) * (1.0 - constantBaseColor.w) + constantBaseColor.rgb * constantBaseColor.w;
            float roughness = texture2D(roughnessTexture, interpolatedUv).r * (1.0 - constantRoughness.y) + constantRoughness.x * constantRoughness.y;
            float metallic = texture2D(metallicTexture, interpolatedUv).r * (1.0 - constantMetallic.y) + constantMetallic.x * constantMetallic.y;

            vec3 normalDirection = normalize(interpolatedNormal);
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


script : Script.Init -> Script Int ()
script { workingDirectory, userPrivileges } =
    let
        outputFile =
            File.writable userPrivileges "../src/Scene3d/Shaders.elm"

        contents =
            "-- Generated by scripts/GenerateShaders.elm, please do not edit by hand\n"
                ++ Glsl.generateModule "Scene3d.Shaders"
                    [ plainVertexShader
                    , texturedVertexShader
                    , quadVertexShader
                    , smoothQuadVertexShader
                    , texturedQuadVertexShader
                    , smoothTexturedQuadVertexShader
                    , pointVertexShader
                    , smoothVertexShader
                    , smoothTexturedVertexShader
                    , shadowVertexShader
                    , quadShadowVertexShader
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
    Script.printLine "Writing output file..."
        |> Script.andThen (File.write contents outputFile)
        |> Script.onError (handleError .message)
        |> Script.andThen (Script.printLine "Formatting output file with elm-format...")
        |> Script.andThen
            (Script.executeWith userPrivileges
                { command = "elm-format"
                , arguments = [ "--yes", File.path outputFile ]
                , workingDirectory = workingDirectory
                }
                |> Script.ignoreResult
                |> Script.onError (\_ -> Script.fail 1)
            )
        |> Script.andThen (Script.printLine "Test compiling output file...")
        |> Script.andThen
            (Script.executeWith userPrivileges
                { command = "elm"
                , arguments = [ "make", "--output=/dev/null", File.path outputFile ]
                , workingDirectory = workingDirectory
                }
                |> Script.ignoreResult
                |> Script.onError (\_ -> Script.fail 1)
            )
        |> Script.andThen (Script.printLine "Success!")


main : Script.Program
main =
    Common.program script
