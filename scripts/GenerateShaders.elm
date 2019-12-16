module GenerateShaders exposing (main)

import Common
import Glsl
import Script exposing (Script)


project : Glsl.Function
project =
    Glsl.function """
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
        }"""
        []


script : Script.Init -> Script Int ()
script init =
    Script.fail 1


main : Script.Program
main =
    Common.program script
