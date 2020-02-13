port module Common exposing (program)

import Json.Encode exposing (Value)
import Script exposing (Script)


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


program : (Script.Init -> Script String ()) -> Script.Program
program script =
    Script.program script requestPort responsePort
