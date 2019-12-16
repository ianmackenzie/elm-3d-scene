module GenerateShaders exposing (main)

import Common
import Glsl
import Script exposing (Script)


position : Glsl.Attribute
position =
    Glsl.attribute "position" Glsl.vec3


modelScale : Glsl.Uniform
modelScale =
    Glsl.uniform "modelScale" Glsl.float


modelMatrix : Glsl.Uniform
modelMatrix =
    Glsl.uniform "modelMatrix" Glsl.mat4


viewMatrix : Glsl.Uniform
viewMatrix =
    Glsl.uniform "viewMatrix" Glsl.mat4


sceneProperties : Glsl.Uniform
sceneProperties =
    Glsl.uniform "sceneProperties" Glsl.mat4


interpolatedPosition : Glsl.Varying
interpolatedPosition =
    Glsl.varying "interpolatedPosition" Glsl.vec3


project : Glsl.Function
project =
    Glsl.function
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
        []


plainVertex : String
plainVertex =
    Glsl.program
        { attributes = [ position ]
        , uniforms = [ modelScale, modelMatrix, viewMatrix, sceneProperties ]
        , varyings = [ interpolatedPosition ]
        }
        """
        void main () {
            vec4 scaledPosition = vec4(modelScale * position, 1.0);
            vec4 transformedPosition = modelMatrix * scaledPosition;
            vec4 projectionProperties = sceneProperties[0];
            gl_Position = project(viewMatrix * transformedPosition, projectionProperties);
            interpolatedPosition = transformedPosition.xyz;
        }
        """
        [ project ]


script : Script.Init -> Script Int ()
script init =
    Script.fail 1


main : Script.Program
main =
    Common.program script
