module PublishExamples exposing (main)

import ReCase
import Script exposing (Script)
import Script.Directory as Directory
import Script.File as File


script : Script.Init -> Script String ()
script { userPrivileges, workingDirectory } =
    let
        sourcePath =
            "/home/ian/git/ianmackenzie/elm-3d-scene/examples"

        sourceDirectory =
            Directory.readOnly userPrivileges sourcePath

        outputPath =
            "/home/ian/git/ianmackenzie/ianmackenzie.github.io/elm-3d-scene/examples/1.0.0"

        outputDirectory =
            Directory.writable userPrivileges outputPath
    in
    Directory.listFiles sourceDirectory
        |> Script.thenWith
            (Script.each
                (\sourceFile ->
                    let
                        inputFileName =
                            File.name sourceFile
                    in
                    if String.endsWith ".elm" inputFileName then
                        let
                            outputStem =
                                inputFileName
                                    |> String.dropRight 4
                                    |> ReCase.recase ReCase.ToKebab

                            outputFileName =
                                outputStem ++ ".html"
                        in
                        Script.do
                            [ Script.printLine ("Compiling " ++ inputFileName ++ " to " ++ outputFileName)
                            , Script.executeWith userPrivileges
                                { workingDirectory = workingDirectory
                                , command = "elm"
                                , arguments =
                                    [ "make"
                                    , "--output"
                                    , outputPath ++ "/" ++ outputFileName
                                    , sourcePath ++ "/" ++ inputFileName
                                    ]
                                }
                                |> Script.ignoreResult
                            ]

                    else
                        Script.do []
                )
            )


main : Script.Program
main =
    Script.program script
