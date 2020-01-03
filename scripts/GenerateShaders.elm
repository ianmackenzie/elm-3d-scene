module GenerateShaders exposing (main)

import Common exposing (handleError)
import Glsl
import Script exposing (Script)
import Script.Directory as Directory
import Script.File as File


position : Glsl.Attribute
position =
    Glsl.attribute Glsl.vec3 "position"


normal : Glsl.Attribute
normal =
    Glsl.attribute Glsl.vec3 "normal"


modelScale : Glsl.Uniform
modelScale =
    Glsl.uniform Glsl.float "modelScale"


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


emissiveColor : Glsl.Uniform
emissiveColor =
    Glsl.uniform Glsl.vec3 "emissiveColor"


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


interpolatedPosition : Glsl.Varying
interpolatedPosition =
    Glsl.varying Glsl.vec3 "interpolatedPosition"


interpolatedNormal : Glsl.Varying
interpolatedNormal =
    Glsl.varying Glsl.vec3 "interpolatedNormal"


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


lambertianEnvironmentalLighting : Glsl.Function
lambertianEnvironmentalLighting =
    Glsl.function
        { dependencies = [ softLightingLuminance ]
        , constants = [ kPerspectiveProjection, kNoEnvironmentalLighting, kSoftLighting ]
        }
        """
        vec3 lambertianEnvironmentalLighting(
            vec3 surfacePosition,
            vec3 normalDirection,
            vec3 materialColor,
            mat4 sceneProperties,
            mat4 viewMatrix,
            mat4 environmentalLighting
        ) {
            vec3 directionToCamera = vec3(0.0, 0.0, 0.0);
            float projectionType = sceneProperties[1].w;
            if (projectionType == kPerspectiveProjection) {
                vec3 cameraPoint = sceneProperties[1].xyz;
                directionToCamera = normalize(cameraPoint - surfacePosition);
            } else {
                directionToCamera = sceneProperties[1].xyz;
            }

            float enviromentalLightingType = environmentalLighting[0][3];
            if (enviromentalLightingType == kNoEnvironmentalLighting) {
                return vec3(0.0, 0.0, 0.0);
            } else if (enviromentalLightingType == kSoftLighting) {
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


lambertianLighting : Glsl.Function
lambertianLighting =
    Glsl.function
        { dependencies = [ getDirectionToLightAndNormalIlluminance, positiveDotProduct ]
        , constants = [ kDisabledLightSource, kPi ]
        }
        """
        vec3 lambertianLighting(
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


plainVertexShader : Glsl.Shader
plainVertexShader =
    Glsl.vertexShader "plainVertex"
        { attributes = [ position ]
        , uniforms = [ modelScale, modelMatrix, viewMatrix, sceneProperties ]
        , varyings = [ interpolatedPosition ]
        , constants = []
        , functions = [ project ]
        }
        """
        void main () {
            vec4 scaledPosition = vec4(modelScale * position, 1.0);
            vec4 transformedPosition = modelMatrix * scaledPosition;
            gl_Position = project(viewMatrix * transformedPosition, sceneProperties[0]);
            interpolatedPosition = transformedPosition.xyz;
        }
        """


pointVertexShader : Glsl.Shader
pointVertexShader =
    Glsl.vertexShader "pointVertex"
        { attributes = [ position ]
        , uniforms = [ modelScale, modelMatrix, pointRadius, viewMatrix, sceneProperties ]
        , varyings = []
        , constants = []
        , functions = [ project ]
        }
        """
        void main () {
            vec4 scaledPosition = vec4(modelScale * position, 1.0);
            vec4 transformedPosition = modelMatrix * scaledPosition;
            gl_Position = project(viewMatrix * transformedPosition, sceneProperties[0]);
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
        , functions = [ project ]
        }
        """
        void main () {
            vec4 scaledPosition = vec4(modelScale * position, 1.0);
            vec4 transformedPosition = modelMatrix * scaledPosition;
            gl_Position = project(viewMatrix * transformedPosition, sceneProperties[0]);
            interpolatedPosition = transformedPosition.xyz;
            interpolatedNormal = (modelMatrix * vec4(normal, 0.0)).xyz;
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


shadowVertexShader : Glsl.Shader
shadowVertexShader =
    Glsl.vertexShader "shadowVertex"
        { attributes = [ position, normal ]
        , uniforms = [ modelScale, modelMatrix, viewMatrix, sceneProperties, shadowLightSource ]
        , varyings = []
        , constants = []
        , functions = [ project, getDirectionToLight ]
        }
        """
        void main () {
            vec4 scaledPosition = vec4(modelScale * position, 1.0);
            vec4 transformedPosition = modelMatrix * scaledPosition;
            vec3 transformedNormal = (modelMatrix * vec4(normal, 0.0)).xyz;
            vec4 xyz_type = shadowLightSource[0];
            vec4 rgb_radius = shadowLightSource[1];
            vec3 directionToLight = getDirectionToLight(position, xyz_type, rgb_radius);
            vec3 offset = vec3(0.0, 0.0, 0.0);
            if (dot(directionToLight, transformedNormal) <= 0.0) {
                offset = -1.0e9 * directionToLight;
            }
            vec4 offsetPosition = transformedPosition + vec4(offset, 0.0);
            gl_Position = project(viewMatrix * offsetPosition, sceneProperties[0]);
        }
        """


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
        , varyings = [ interpolatedPosition, interpolatedNormal ]
        , constants = []
        , functions = [ lambertianEnvironmentalLighting, lambertianLighting, toSrgb ]
        }
        """
        void main() {
            vec3 normalDirection = normalize(interpolatedNormal);
            vec3 litColor0 = lambertianEnvironmentalLighting(interpolatedPosition, normalDirection, materialColor, sceneProperties, viewMatrix, environmentalLighting);
            vec3 litColor1 = lambertianDirectLighting(interpolatedPosition, normalDirection, materialColor, lightSources12[0], lightSources12[1]);
            vec3 litColor2 = lambertianDirectLighting(interpolatedPosition, normalDirection, materialColor, lightSources12[2], lightSources12[3]);
            vec3 litColor3 = lambertianDirectLighting(interpolatedPosition, normalDirection, materialColor, lightSources34[0], lightSources34[1]);
            vec3 litColor4 = lambertianDirectLighting(interpolatedPosition, normalDirection, materialColor, lightSources34[2], lightSources34[3]);
            vec3 litColor5 = lambertianDirectLighting(interpolatedPosition, normalDirection, materialColor, lightSources56[0], lightSources56[1]);
            vec3 litColor6 = lambertianDirectLighting(interpolatedPosition, normalDirection, materialColor, lightSources56[2], lightSources56[3]);
            vec3 litColor7 = lambertianDirectLighting(interpolatedPosition, normalDirection, materialColor, lightSources78[0], lightSources78[1]);
            vec3 litColor8 = lambertianDirectLighting(interpolatedPosition, normalDirection, materialColor, lightSources78[2], lightSources78[3]);
            vec3 litColorSum = litColor0 + litColor1 + litColor2 + litColor3 + litColor4 + litColor5 + litColor6 + litColor7 + litColor8;
            gl_FragColor = toSrgb(litColorSum, sceneProperties);
        }
        """


script : Script.Init -> Script Int ()
script { workingDirectory, userPrivileges } =
    let
        outputFile =
            File.in_ workingDirectory "Shaders.elm"

        contents =
            "-- Generated by scripts/GenerateShaders.elm, please do not edit by hand\n"
                ++ Glsl.generateModule "Shaders"
                    [ plainVertexShader
                    , pointVertexShader
                    , smoothVertexShader
                    , shadowVertexShader
                    , shadowFragmentShader
                    , constantFragmentShader
                    , constantPointFragmentShader
                    , emissiveFragmentShader
                    , emissivePointFragmentShader
                    , lambertianFragmentShader
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
