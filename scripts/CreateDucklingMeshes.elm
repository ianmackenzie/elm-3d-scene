module CreateDucklingMeshes exposing (main)

import Json.Encode as Encode
import Obj
import Scene3d.Mesh as Mesh
import Scene3d.Mesh.Encode as Encode
import Script exposing (Script)
import Script.Directory as Directory
import Script.File as File


script : Script.Init -> Script String ()
script { userPrivileges } =
    let
        outputDirectory =
            Directory.writable userPrivileges "C:/Git/ianmackenzie/ianmackenzie.github.io/elm-3d-scene/examples"

        objFile =
            File.in_ outputDirectory "duckling-medpoly.obj"

        meshJsonFile =
            File.in_ outputDirectory "duckling.json"

        shadowJsonFile =
            File.in_ outputDirectory "duckling-shadow.json"
    in
    File.read objFile
        |> Script.thenWith
            (\contents ->
                let
                    triangularMesh =
                        Obj.parse contents

                    mesh =
                        Mesh.texturedFaces triangularMesh

                    shadow =
                        Mesh.shadow mesh

                    meshJson =
                        Encode.mesh mesh

                    shadowJson =
                        Encode.shadow shadow
                in
                Script.do
                    [ File.writeTo meshJsonFile (Encode.encode 0 meshJson)
                    , File.writeTo shadowJsonFile (Encode.encode 0 shadowJson)
                    ]
            )


main : Script.Program
main =
    Script.program script
