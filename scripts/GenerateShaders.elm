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


lightSource : Glsl.Uniform
lightSource =
    Glsl.uniform Glsl.mat4 "lightSource"


pointRadius : Glsl.Uniform
pointRadius =
    Glsl.uniform Glsl.float "pointRadius"


color : Glsl.Uniform
color =
    Glsl.uniform Glsl.vec3 "color"


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
    Glsl.uniform Glsl.mat4 "lightSources12"


lightSources56 : Glsl.Uniform
lightSources56 =
    Glsl.uniform Glsl.mat4 "lightSources12"


lightSources78 : Glsl.Uniform
lightSources78 =
    Glsl.uniform Glsl.mat4 "lightSources12"


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
        , uniforms = [ modelScale, modelMatrix, viewMatrix, sceneProperties, lightSource ]
        , varyings = []
        , constants = []
        , functions = [ project, getDirectionToLight ]
        }
        """
        void main () {
            vec4 scaledPosition = vec4(modelScale * position, 1.0);
            vec4 transformedPosition = modelMatrix * scaledPosition;
            vec3 transformedNormal = (modelMatrix * vec4(normal, 0.0)).xyz;
            vec4 xyz_type = lightSource[0];
            vec4 rgb_radius = lightSource[1];
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
        { uniforms = [ color ]
        , varyings = [ interpolatedPosition ]
        , constants = []
        , functions = []
        }
        """
        void main () {
            gl_FragColor = vec4(color, 1.0);
        }
        """


constantPointFragmentShader : Glsl.Shader
constantPointFragmentShader =
    Glsl.fragmentShader "constantPointFragment"
        { uniforms = [ color, pointRadius, sceneProperties ]
        , varyings = []
        , constants = []
        , functions = [ pointAlpha ]
        }
        """
        void main () {
            float alpha = pointAlpha(pointRadius, gl_PointCoord);
            gl_FragColor = vec4(color, alpha);
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


script : Script.Init -> Script Int ()
script { workingDirectory, userPrivileges } =
    let
        outputFile =
            File.in_ workingDirectory "Shaders.elm"

        contents =
            Glsl.generateModule "Shaders"
                [ plainVertexShader
                , pointVertexShader
                , smoothVertexShader
                , shadowVertexShader
                , shadowFragmentShader
                , constantFragmentShader
                , constantPointFragmentShader
                , emissiveFragmentShader
                , emissivePointFragmentShader
                ]
    in
    File.write contents outputFile
        |> Script.onError (handleError .message)
        |> Script.andThen
            (Script.executeWith userPrivileges
                { command = "elm-format"
                , arguments = [ "--yes", File.path outputFile ]
                , workingDirectory = workingDirectory
                }
                |> Script.ignoreResult
                |> Script.ignoreError
            )


main : Script.Program
main =
    Common.program script
