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


interpolatedPosition : Glsl.Varying
interpolatedPosition =
    Glsl.varying Glsl.vec3 "interpolatedPosition"


interpolatedNormal : Glsl.Varying
interpolatedNormal =
    Glsl.varying Glsl.vec3 "interpolatedNormal"


disabledLightSource : Glsl.Constant
disabledLightSource =
    Glsl.constant Glsl.float "DISABLED_LIGHT_SOURCE" "0.0"


directionalLightSource : Glsl.Constant
directionalLightSource =
    Glsl.constant Glsl.float "DIRECTIONAL_LIGHT_SOURCE" "1.0"


pointLightSource : Glsl.Constant
pointLightSource =
    Glsl.constant Glsl.float "POINT_LIGHT_SOURCE" "2.0"


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


plainVertexShader : String
plainVertexShader =
    Glsl.program
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


pointVertexShader : String
pointVertexShader =
    Glsl.program
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


smoothVertexShader : String
smoothVertexShader =
    Glsl.program
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
        , constants = [ directionalLightSource, pointLightSource ]
        }
        """
        vec3 getDirectionToLight(vec3 surfacePosition, vec4 xyz_type, vec4 rgb_radius) {
            float lightSourceType = xyz_type.w;
            if (lightSourceType == DIRECTIONAL_LIGHT_SOURCE) {
                return xyz_type.xyz;
            } else if (lightSourceType == POINT_LIGHT_SOURCE) {
                vec3 lightPosition = xyz_type.xyz;
                return normalize(lightPosition - surfacePosition);
            }
        }
        """


shadowVertexShader : String
shadowVertexShader =
    Glsl.program
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


script : Script.Init -> Script Int ()
script { workingDirectory } =
    let
        dump shader fileName =
            File.write shader (File.in_ workingDirectory fileName)
    in
    Script.do
        [ dump plainVertexShader "plainVertexShader.glsl"
        , dump pointVertexShader "pointVertexShader.glsl"
        , dump smoothVertexShader "smoothVertexShader.glsl"
        , dump shadowVertexShader "shadowVertexShader.glsl"
        ]
        |> Script.onError (handleError .message)


main : Script.Program
main =
    Common.program script
