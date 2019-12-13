module Scene3d.Shader exposing
    ( constantFragment
    , constantPointFragment
    , emissiveFragment
    , emissivePointFragment
    , lambertianFragment
    , physicalFragment
    , plainVertex
    , pointVertex
    , shadowFragment
    , shadowVertex
    , smoothVertex
    )

import Math.Matrix4 as Matrix4 exposing (Mat4)
import Math.Vector3 as Vector3 exposing (Vec3)
import Math.Vector4 as Vector4 exposing (Vec4)
import WebGL
import WebGL.Texture exposing (Texture)



-- # PACKED MATRIX UNIFORMS
--
--
-- ## Overall scene Properties
--
-- projectionType:
--   0: perspective (camera XYZ is eye position)
--   1: orthographic (camera XYZ is direction to screen)
--
-- [ clipDistance  cameraX         whiteR  * ]
-- [ aspectRatio   cameraY         whiteG  * ]
-- [ kc            cameraZ         whiteB  * ]
-- [ kz            projectionType  *       * ]
--
--
-- ## Lights
--
-- type:
--   0 : disabled
--   1 : directional (XYZ is direction to light, i.e. reversed light direction)
--   2 : point (XYZ is light position)
--
-- radius is unused for now (will hopefully add sphere lights in the future)
--
-- [ x_i     r_i       x_j     r_j      ]
-- [ y_i     g_i       y_j     g_j      ]
-- [ z_i     b_i       z_j     b_j      ]
-- [ type_i  radius_i  type_j  radius_j ]


type alias SmoothVaryings =
    { interpolatedPosition : Vec3
    , interpolatedNormal : Vec3
    }


plainVertex :
    WebGL.Shader
        { attributes
            | position : Vec3
        }
        { uniforms
            | modelScale : Float
            , modelMatrix : Mat4
            , viewMatrix : Mat4
            , sceneProperties : Mat4
        }
        { interpolatedPosition : Vec3
        }
plainVertex =
    [glsl|
        precision mediump float;
        
        attribute vec3 position;

        uniform float modelScale;
        uniform mat4 modelMatrix;
        uniform mat4 viewMatrix;
        uniform mat4 sceneProperties;

        varying vec3 interpolatedPosition;

        vec4 project(vec4 position) {
            float n = sceneProperties[0][0];
            float a = sceneProperties[0][1];
            float kc = sceneProperties[0][2];
            float kz = sceneProperties[0][3];
            return vec4(
                (kc + kz * position.z) * (position.x / a),
                (kc + kz * position.z) * position.y,
                (-position.z - 2.0 * n),
                -position.z
            );
        }

        void main () {
            vec4 scaledPosition = vec4(modelScale * position, 1.0);
            vec4 transformedPosition = modelMatrix * scaledPosition;
            gl_Position = project(viewMatrix * transformedPosition);
            interpolatedPosition = transformedPosition.xyz;
        }
    |]


pointVertex :
    WebGL.Shader
        { attributes
            | position : Vec3
        }
        { uniforms
            | modelScale : Float
            , modelMatrix : Mat4
            , pointRadius : Float
            , viewMatrix : Mat4
            , sceneProperties : Mat4
        }
        {}
pointVertex =
    [glsl|
        precision mediump float;
        
        attribute vec3 position;

        uniform float modelScale;
        uniform mat4 modelMatrix;
        uniform mat4 viewMatrix;
        uniform mat4 sceneProperties;
        uniform float pointRadius;

        vec4 project(vec4 position) {
            float n = sceneProperties[0][0];
            float a = sceneProperties[0][1];
            float kc = sceneProperties[0][2];
            float kz = sceneProperties[0][3];
            return vec4(
                (kc + kz * position.z) * (position.x / a),
                (kc + kz * position.z) * position.y,
                (-position.z - 2.0 * n),
                -position.z
            );
        }

        void main () {
            vec4 scaledPosition = vec4(modelScale * position, 1.0);
            vec4 transformedPosition = modelMatrix * scaledPosition;
            gl_Position = project(viewMatrix * transformedPosition);
            gl_PointSize = 2.0 * pointRadius + 2.0;
        }
    |]


smoothVertex :
    WebGL.Shader
        { attributes
            | position : Vec3
            , normal : Vec3
        }
        { uniforms
            | modelScale : Float
            , modelMatrix : Mat4
            , viewMatrix : Mat4
            , sceneProperties : Mat4
        }
        { interpolatedPosition : Vec3
        , interpolatedNormal : Vec3
        }
smoothVertex =
    [glsl|
        precision mediump float;

        attribute vec3 position;
        attribute vec3 normal;

        uniform float modelScale;
        uniform mat4 modelMatrix;
        uniform mat4 viewMatrix;
        uniform mat4 sceneProperties;

        varying vec3 interpolatedPosition;
        varying vec3 interpolatedNormal;

        vec4 project(vec4 position) {
            float n = sceneProperties[0][0];
            float a = sceneProperties[0][1];
            float kc = sceneProperties[0][2];
            float kz = sceneProperties[0][3];
            return vec4(
                (kc + kz * position.z) * (position.x / a),
                (kc + kz * position.z) * position.y,
                (-position.z - 2.0 * n),
                -position.z
            );
        }

        void main () {
            vec4 scaledPosition = vec4(modelScale * position, 1.0);
            vec4 transformedPosition = modelMatrix * scaledPosition;
            gl_Position = project(viewMatrix * transformedPosition);
            interpolatedPosition = transformedPosition.xyz;
            interpolatedNormal = (modelMatrix * vec4(normal, 0.0)).xyz;
        }
    |]


shadowVertex :
    WebGL.Shader
        { attributes
            | position : Vec3
            , normal : Vec3
        }
        { uniforms
            | modelScale : Float
            , modelMatrix : Mat4
            , viewMatrix : Mat4
            , sceneProperties : Mat4
            , lightSource : Mat4
        }
        {}
shadowVertex =
    [glsl|
        precision mediump float;

        const float DISABLED_LIGHT_SOURCE = 0.0;
        const float DIRECTIONAL_LIGHT_SOURCE = 1.0;
        const float POINT_LIGHT_SOURCE = 2.0;

        attribute vec3 position;
        attribute vec3 normal;

        uniform float modelScale;
        uniform mat4 modelMatrix;
        uniform mat4 viewMatrix;
        uniform mat4 sceneProperties;
        uniform mat4 lightSource;

        vec4 project(vec4 position) {
            float n = sceneProperties[0][0];
            float a = sceneProperties[0][1];
            float kc = sceneProperties[0][2];
            float kz = sceneProperties[0][3];
            return vec4(
                (kc + kz * position.z) * (position.x / a),
                (kc + kz * position.z) * position.y,
                (-position.z - 2.0 * n),
                -position.z
            );
        }
        
        vec3 getDirectionToLight(vec4 xyz_type, vec4 rgb_radius) {
            float lightSourceType = xyz_type.w;
            if (lightSourceType == DIRECTIONAL_LIGHT_SOURCE) {
                return xyz_type.xyz;
            } else if (lightSourceType == POINT_LIGHT_SOURCE) {
                vec3 lightPosition = xyz_type.xyz;
                return normalize(lightPosition - position);
            }
        }

        void main () {
            vec4 scaledPosition = vec4(modelScale * position, 1.0);
            vec4 transformedPosition = modelMatrix * scaledPosition;
            vec3 transformedNormal = (modelMatrix * vec4(normal, 0.0)).xyz;
            vec4 xyz_type = lightSource[0];
            vec4 rgb_radius = lightSource[1];
            vec3 directionToLight = getDirectionToLight(xyz_type, rgb_radius);
            vec3 offset = vec3(0.0, 0.0, 0.0);
            if (dot(directionToLight, transformedNormal) <= 0.0) {
                offset = -1.0e9 * directionToLight;
            }
            vec4 offsetPosition = transformedPosition + vec4(offset, 0.0);
            gl_Position = project(viewMatrix * offsetPosition);
        }
    |]


shadowFragment : WebGL.Shader {} uniforms {}
shadowFragment =
    [glsl|
        precision mediump float;
        
        void main () {
            gl_FragColor = vec4(0.0, 0.0, 0.0, 1.0);
        }
    |]


constantFragment :
    WebGL.Shader {}
        { uniforms
            | color : Vec3
        }
        { interpolatedPosition : Vec3
        }
constantFragment =
    [glsl|
        precision mediump float;

        uniform vec3 color;

        varying vec3 interpolatedPosition;

        void main () {
            gl_FragColor = vec4(color, 1.0);
        }
    |]


constantPointFragment :
    WebGL.Shader {}
        { uniforms
            | color : Vec3
            , pointRadius : Float
            , sceneProperties : Mat4
        }
        {}
constantPointFragment =
    [glsl|
        precision mediump float;

        uniform vec3 color;
        uniform mat4 sceneProperties;

        void main () {
            float pointSize = 2.0 * pointRadius;
            float x = (pointSize + 2.0) * (gl_PointCoord.s - 0.5);
            float y = (pointSize + 2.0) * (gl_PointCoord.t - 0.5);
            float r = sqrt(x * x + y * y);
            float alpha = 1.0;
            float innerRadius = pointRadius;
            float outerRadius = pointRadius + 1.0;
            if (r > outerRadius) {
                alpha = 0.0;
            } else if (r > innerRadius) {
                alpha = outerRadius - r;
            }
            gl_FragColor = vec4(color, alpha);
        }
    |]


emissiveFragment :
    WebGL.Shader {}
        { uniforms
            | emissiveColor : Vec3
            , sceneProperties : Mat4
        }
        { interpolatedPosition : Vec3
        }
emissiveFragment =
    [glsl|
        precision mediump float;

        uniform vec3 emissiveColor;
        uniform mat4 sceneProperties;

        varying vec3 interpolatedPosition;

        float gammaCorrect(float u) {
            if (u <= 0.0031308) {
                return 12.92 * u;
            } else {
                return 1.055 * pow(u, 1.0 / 2.4) - 0.055;
            }
        }

        vec4 toSrgb(vec3 linearColor) {
            vec3 referenceWhite = sceneProperties[2].rgb;
            float red = gammaCorrect(linearColor.r / referenceWhite.r);
            float green = gammaCorrect(linearColor.g / referenceWhite.g);
            float blue = gammaCorrect(linearColor.b / referenceWhite.b);
            return vec4(red, green, blue, 1.0);
        }

        void main () {
            gl_FragColor = toSrgb(emissiveColor);
        }
    |]


emissivePointFragment :
    WebGL.Shader {}
        { uniforms
            | emissiveColor : Vec3
            , pointRadius : Float
            , sceneProperties : Mat4
        }
        {}
emissivePointFragment =
    [glsl|
        precision mediump float;

        uniform vec3 emissiveColor;
        uniform mat4 sceneProperties;
        uniform float pointRadius;

        float gammaCorrect(float u) {
            if (u <= 0.0031308) {
                return 12.92 * u;
            } else {
                return 1.055 * pow(u, 1.0 / 2.4) - 0.055;
            }
        }

        vec3 toSrgb(vec3 linearColor) {
            vec3 referenceWhite = sceneProperties[2].rgb;
            float red = gammaCorrect(linearColor.r / referenceWhite.r);
            float green = gammaCorrect(linearColor.g / referenceWhite.g);
            float blue = gammaCorrect(linearColor.b / referenceWhite.b);
            return vec3(red, green, blue);
        }

        void main () {
            float pointSize = 2.0 * pointRadius;
            float x = (pointSize + 2.0) * (gl_PointCoord.s - 0.5);
            float y = (pointSize + 2.0) * (gl_PointCoord.t - 0.5);
            float r = sqrt(x * x + y * y);
            float alpha = 1.0;
            float innerRadius = pointRadius;
            float outerRadius = pointRadius + 1.0;
            if (r > outerRadius) {
                alpha = 0.0;
            } else if (r > innerRadius) {
                alpha = outerRadius - r;
            }
            gl_FragColor = vec4(toSrgb(emissiveColor), alpha);
        }
    |]


lambertianFragment :
    WebGL.Shader {}
        { uniforms
            | sceneProperties : Mat4
            , environmentalLighting : Mat4
            , lightSources12 : Mat4
            , lightSources34 : Mat4
            , lightSources56 : Mat4
            , lightSources78 : Mat4
            , materialColor : Vec3
            , viewMatrix : Mat4
        }
        { interpolatedPosition : Vec3
        , interpolatedNormal : Vec3
        }
lambertianFragment =
    [glsl|
        precision mediump float;

        const float PERSPECTIVE_PROJECTION = 0.0;
        const float ORTHOGRAPHIC_PROJECTION = 1.0;

        const float DISABLED_LIGHT_SOURCE = 0.0;
        const float DIRECTIONAL_LIGHT_SOURCE = 1.0;
        const float POINT_LIGHT_SOURCE = 2.0;

        const float NO_ENVIRONMENTAL_LIGHTING = 0.0;
        const float SOFT_LIGHTING = 1.0;

        uniform mat4 sceneProperties;
        uniform mat4 environmentalLighting;
        uniform mat4 lightSources12;
        uniform mat4 lightSources34;
        uniform mat4 lightSources56;
        uniform mat4 lightSources78;
        uniform vec3 materialColor;
        uniform mat4 viewMatrix;

        varying vec3 interpolatedPosition;
        varying vec3 interpolatedNormal;

        float positiveDotProduct(vec3 v1, vec3 v2) {
            return clamp(dot(v1, v2), 0.0, 1.0);
        }

        float gammaCorrect(float u) {
            if (u <= 0.0031308) {
                return 12.92 * u;
            } else {
                return 1.055 * pow(u, 1.0 / 2.4) - 0.055;
            }
        }

        vec4 toSrgb(vec3 linearColor) {
            vec3 referenceWhite = sceneProperties[2].rgb;
            float red = gammaCorrect(linearColor.r / referenceWhite.r);
            float green = gammaCorrect(linearColor.g / referenceWhite.g);
            float blue = gammaCorrect(linearColor.b / referenceWhite.b);
            return vec4(red, green, blue, 1.0);
        }

        vec3 softLightingLuminance(vec3 aboveLuminance, vec3 belowLuminance, vec3 localUpDirection, vec3 localLightDirection) {
            float sinElevation = dot(localLightDirection, localUpDirection);
            float t = (sinElevation + 1.0) / 2.0;
            return aboveLuminance * t + belowLuminance * (1.0 - t);
        }

        void getDirectionToLightAndNormalIlluminance(vec4 xyz_type, vec4 rgb_radius, out vec3 directionToLight, out vec3 normalIlluminance) {
            float lightSourceType = xyz_type.w;
            if (lightSourceType == DIRECTIONAL_LIGHT_SOURCE) {
                directionToLight = xyz_type.xyz;
                normalIlluminance = rgb_radius.rgb;
            } else if (lightSourceType == POINT_LIGHT_SOURCE) {
                vec3 lightSourcePosition = xyz_type.xyz;
                vec3 displacement = lightSourcePosition - interpolatedPosition;
                float distance = length(displacement);
                directionToLight = displacement / distance;
                normalIlluminance = rgb_radius.rgb / (4.0 * 3.14159265359 * distance * distance);
            }
        }

        vec3 environmentalLightingColor(vec3 normalDirection, vec3 directionToCamera) {
            float enviromentalLightingType = environmentalLighting[0][3];
            if (enviromentalLightingType == NO_ENVIRONMENTAL_LIGHTING) {
                return vec3(0.0, 0.0, 0.0);
            } else if (enviromentalLightingType == SOFT_LIGHTING) {
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

        vec3 litColor(vec4 xyz_type, vec4 rgb_radius, vec3 normalDirection) {
            float lightSourceType = xyz_type.w;
            if (lightSourceType == DISABLED_LIGHT_SOURCE) {
                return vec3(0.0, 0.0, 0.0);
            } 

            vec3 directionToLight = vec3(0.0, 0.0, 0.0);
            vec3 normalIlluminance = vec3(0.0, 0.0, 0.0);
            getDirectionToLightAndNormalIlluminance(xyz_type, rgb_radius, directionToLight, normalIlluminance);

            float dotNL = positiveDotProduct(directionToLight, interpolatedNormal);
            return (normalIlluminance * dotNL) * (materialColor / 3.14159265359);
        }

        void main() {
            float projectionType = sceneProperties[1][3];
            vec3 directionToCamera = vec3(0.0, 0.0, 0.0);
            if (projectionType == PERSPECTIVE_PROJECTION) {
                vec3 cameraPoint = sceneProperties[1].xyz;
                directionToCamera = normalize(cameraPoint - interpolatedPosition);
            } else {
                directionToCamera = sceneProperties[1].xyz;
            }
            vec3 normalDirection = normalize(interpolatedNormal);
            vec3 litColor0 = environmentalLightingColor(normalDirection, directionToCamera);
            vec3 litColor1 = litColor(lightSources12[0], lightSources12[1], normalDirection);
            vec3 litColor2 = litColor(lightSources12[2], lightSources12[3], normalDirection);
            vec3 litColor3 = litColor(lightSources34[0], lightSources34[1], normalDirection);
            vec3 litColor4 = litColor(lightSources34[2], lightSources34[3], normalDirection);
            vec3 litColor5 = litColor(lightSources56[0], lightSources56[1], normalDirection);
            vec3 litColor6 = litColor(lightSources56[2], lightSources56[3], normalDirection);
            vec3 litColor7 = litColor(lightSources78[0], lightSources78[1], normalDirection);
            vec3 litColor8 = litColor(lightSources78[2], lightSources78[3], normalDirection);
            
            gl_FragColor = toSrgb(litColor0 + litColor1 + litColor2 + litColor3 + litColor4 + litColor5 + litColor6 + litColor7 + litColor8);
        }
    |]


physicalFragment :
    WebGL.Shader {}
        { uniforms
            | sceneProperties : Mat4
            , environmentalLighting : Mat4
            , viewMatrix : Mat4
            , lightSources12 : Mat4
            , lightSources34 : Mat4
            , lightSources56 : Mat4
            , lightSources78 : Mat4
            , baseColor : Vec3
            , roughness : Float
            , metallic : Float
        }
        SmoothVaryings
physicalFragment =
    [glsl|
        precision mediump float;

        const float PERSPECTIVE_PROJECTION = 0.0;
        const float ORTHOGRAPHIC_PROJECTION = 1.0;

        const float DISABLED_LIGHT_SOURCE = 0.0;
        const float DIRECTIONAL_LIGHT_SOURCE = 1.0;
        const float POINT_LIGHT_SOURCE = 2.0;

        const float NO_ENVIRONMENTAL_LIGHTING = 0.0;
        const float SOFT_LIGHTING = 1.0;

        uniform mat4 sceneProperties;
        uniform mat4 environmentalLighting;
        uniform mat4 viewMatrix;
        uniform mat4 lightSources12;
        uniform mat4 lightSources34;
        uniform mat4 lightSources56;
        uniform mat4 lightSources78;
        uniform vec3 baseColor;
        uniform float roughness;
        uniform float metallic;

        varying vec3 interpolatedPosition;
        varying vec3 interpolatedNormal;

        float positiveDotProduct(vec3 v1, vec3 v2) {
            return clamp(dot(v1, v2), 0.0, 1.0);
        }

        float gammaCorrect(float u) {
            if (u <= 0.0031308) {
                return 12.92 * u;
            } else {
                return 1.055 * pow(u, 1.0 / 2.4) - 0.055;
            }
        }

        vec4 toSrgb(vec3 linearColor) {
            vec3 referenceWhite = sceneProperties[2].rgb;
            float red = gammaCorrect(linearColor.r / referenceWhite.r);
            float green = gammaCorrect(linearColor.g / referenceWhite.g);
            float blue = gammaCorrect(linearColor.b / referenceWhite.b);
            return vec4(red, green, blue, 1.0);
        }

        void getDirectionToLightAndNormalIlluminance(vec4 xyz_type, vec4 rgb_radius, out vec3 directionToLight, out vec3 normalIlluminance) {
            float lightSourceType = xyz_type.w;
            if (lightSourceType == DIRECTIONAL_LIGHT_SOURCE) {
                directionToLight = xyz_type.xyz;
                normalIlluminance = rgb_radius.rgb;
            } else if (lightSourceType == POINT_LIGHT_SOURCE) {
                vec3 lightPosition = xyz_type.xyz;
                vec3 displacement = lightPosition - interpolatedPosition;
                float distance = length(displacement);
                directionToLight = displacement / distance;
                normalIlluminance = rgb_radius.rgb / (4.0 * 3.14159265359 * distance * distance);
            }
        }

        float safeQuotient(float numerator, float denominator) {
            if (denominator == 0.0) {
                return 0.0;
            } else {
                return numerator / denominator;
            }
        }

        // Adapted from https://google.github.io/filament/Filament.md.html#materialsystem/specularbrdf/normaldistributionfunction(speculard)
        float specularD(float alpha, float dotNH, vec3 normalDirection, vec3 halfDirection) {
            vec3 crossNH = cross(normalDirection, halfDirection);
            float a = dotNH * alpha;
            float k = alpha / (dot(crossNH, crossNH) + a * a);
            float d = k * k * (1.0 / 3.14159265359);
            return min(d, 65504.0);
        }

        float g1(float dotNV, float alphaSquared) {
            return safeQuotient(2.0 * dotNV, dotNV + sqrt(alphaSquared + (1.0 - alphaSquared) * dotNV * dotNV));
        }

        float specularG(float dotNL, float dotNV, float alphaSquared) {
            return g1(dotNV, alphaSquared) * g1(dotNL, alphaSquared);
        }

        vec3 fresnelColor(vec3 specularBaseColor, float dotVH) {
            vec3 one = vec3(1.0, 1.0, 1.0);
            float scale = exp2((-5.55473 * dotVH - 6.98316) * dotVH);
            return specularBaseColor + (one - specularBaseColor) * scale;
        }

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

        vec3 sampleFacetNormal(float t1, float t2, vec3 vH, vec3 vT1, vec3 vT2, float s, float alpha) {
            t2 = (1.0 - s) * sqrt(1.0 - t1 * t1) + s * t2;
            vec3 vNh = t1 * vT1 + t2 * vT2 + sqrt(max(0.0, 1.0 - t1 * t1 - t2 * t2)) * vH;
            return normalize(vec3(alpha * vNh.x, alpha * vNh.y, max(0.0, vNh.z)));
        }

        vec3 softLightingLuminance(vec3 aboveLuminance, vec3 belowLuminance, vec3 localUpDirection, vec3 localLightDirection) {
            float sinElevation = dot(localLightDirection, localUpDirection);
            float t = (1.0 + sinElevation) / 2.0;
            return aboveLuminance * t + belowLuminance * (1.0 - t);
        }

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

        vec3 specularLightDirection(vec3 v, vec3 h) {
            return (2.0 * dot(v, h)) * h - v;
        }

        vec3 environmentalLightingColor(
            vec3 normalDirection,
            vec3 directionToCamera,
            float dotNV,
            vec3 diffuseBaseColor,
            vec3 specularBaseColor,
            float alpha
        ) {
            float enviromentalLightingType = environmentalLighting[0][3];
            float alphaSquared = alpha * alpha;

            if (enviromentalLightingType == NO_ENVIRONMENTAL_LIGHTING) {
                return vec3(0.0, 0.0, 0.0);
            }

            if (enviromentalLightingType == SOFT_LIGHTING) {
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

        vec3 litColor(vec4 xyz_type, vec4 rgb_radius, vec3 normalDirection, vec3 directionToCamera, float dotNV, vec3 diffuseBaseColor, vec3 specularBaseColor, float alpha) {
            float lightSourceType = xyz_type.w;
            if (lightSourceType == DISABLED_LIGHT_SOURCE) {
                return vec3(0.0, 0.0, 0.0);
            }

            vec3 directionToLight = vec3(0.0, 0.0, 0.0);
            vec3 normalIlluminance = vec3(0.0, 0.0, 0.0);
            getDirectionToLightAndNormalIlluminance(xyz_type, rgb_radius, directionToLight, normalIlluminance);

            float dotNL = positiveDotProduct(normalDirection, directionToLight);
            vec3 specularColor = brdf(normalDirection, directionToCamera, directionToLight, alpha, dotNV, dotNL, specularBaseColor, normalIlluminance);
            return (normalIlluminance * dotNL) * ((diffuseBaseColor / 3.14159265359) + specularColor);
        }

        void main() {
            vec3 normalDirection = normalize(interpolatedNormal);
            float projectionType = sceneProperties[1][3];
            vec3 directionToCamera = vec3(0.0, 0.0, 0.0);
            if (projectionType == PERSPECTIVE_PROJECTION) {
                vec3 cameraPoint = sceneProperties[1].xyz;
                directionToCamera = normalize(cameraPoint - interpolatedPosition);
            } else {
                directionToCamera = sceneProperties[1].xyz;
            }

            float dotNV = positiveDotProduct(normalDirection, directionToCamera);

            float nonmetallic = 1.0 - metallic;
            vec3 diffuseBaseColor = nonmetallic * 0.96 * baseColor;
            vec3 specularBaseColor = nonmetallic * 0.04 * vec3(1.0, 1.0, 1.0) + metallic * baseColor;

            vec3 linearColor = vec3(0.0, 0.0, 0.0);

            float alpha = roughness * roughness;

            vec3 litColor0 = environmentalLightingColor(normalDirection, directionToCamera, dotNV, diffuseBaseColor, specularBaseColor, alpha);
            vec3 litColor1 = litColor(lightSources12[0], lightSources12[1], normalDirection, directionToCamera, dotNV, diffuseBaseColor, specularBaseColor, alpha);
            vec3 litColor2 = litColor(lightSources12[2], lightSources12[3], normalDirection, directionToCamera, dotNV, diffuseBaseColor, specularBaseColor, alpha);
            vec3 litColor3 = litColor(lightSources34[0], lightSources34[1], normalDirection, directionToCamera, dotNV, diffuseBaseColor, specularBaseColor, alpha);
            vec3 litColor4 = litColor(lightSources34[2], lightSources34[3], normalDirection, directionToCamera, dotNV, diffuseBaseColor, specularBaseColor, alpha);
            vec3 litColor5 = litColor(lightSources56[0], lightSources56[1], normalDirection, directionToCamera, dotNV, diffuseBaseColor, specularBaseColor, alpha);
            vec3 litColor6 = litColor(lightSources56[2], lightSources56[3], normalDirection, directionToCamera, dotNV, diffuseBaseColor, specularBaseColor, alpha);
            vec3 litColor7 = litColor(lightSources78[0], lightSources78[1], normalDirection, directionToCamera, dotNV, diffuseBaseColor, specularBaseColor, alpha);
            vec3 litColor8 = litColor(lightSources78[2], lightSources78[3], normalDirection, directionToCamera, dotNV, diffuseBaseColor, specularBaseColor, alpha);

            gl_FragColor = toSrgb(litColor0 + litColor1 + litColor2 + litColor3 + litColor4 + litColor5 + litColor6 + litColor7 + litColor8);
        }
    |]
