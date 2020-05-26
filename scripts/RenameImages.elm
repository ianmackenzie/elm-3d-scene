module RenameImages exposing (main)

import Script exposing (Script)
import Script.Directory as Directory
import Script.File as File


script : Script.Init -> Script String ()
script { userPrivileges } =
    let
        imagesDirectory =
            Directory.writable userPrivileges "images"

        testCaseIndexes =
            List.range 1 82
    in
    testCaseIndexes
        |> Script.each
            (\index ->
                let
                    originalFileName =
                        "download (" ++ String.fromInt index ++ ").png"

                    newFileName =
                        "test" ++ String.fromInt index ++ ".png"
                in
                File.move
                    (File.in_ imagesDirectory originalFileName)
                    (File.in_ imagesDirectory newFileName)
            )


main : Script.Program
main =
    Script.program script
