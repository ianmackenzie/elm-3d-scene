module PublishTestApp exposing (main)

import Script exposing (Script)
import Script.Directory as Directory
import Script.File as File


script : Script.Init -> Script String ()
script { userPrivileges, workingDirectory } =
    Script.do
        [ Script.executeWith userPrivileges
            { workingDirectory = workingDirectory
            , command = "elm"
            , arguments =
                [ "make"
                , "--output"
                , "C:/Git/ianmackenzie/ianmackenzie.github.io/elm-3d-scene/portability-testing/main.js"
                , "TestApp.elm"
                ]
            }
            |> Script.ignoreResult
        , File.copyInto
            (Directory.writable userPrivileges "C:/Git/ianmackenzie/ianmackenzie.github.io/elm-3d-scene")
            (File.in_ workingDirectory "portability-testing.html")
            |> Script.ignoreResult
        , File.copyInto
            (Directory.writable userPrivileges "C:/Git/ianmackenzie/ianmackenzie.github.io/elm-3d-scene/portability-testing")
            (File.in_ workingDirectory "portability-testing/test_cases_2.txt")
            |> Script.ignoreResult
        ]


main : Script.Program
main =
    Script.program script
