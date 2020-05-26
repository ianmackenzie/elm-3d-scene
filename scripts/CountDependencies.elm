module CountDependencies exposing (main)

import Json.Decode as Decode exposing (Decoder)
import Script exposing (Script)
import Script.Http as Http


script : Script.Init -> Script String ()
script { networkConnection } =
    Http.get networkConnection
        { url = "https://package.elm-lang.org/search.json"
        , expect = Http.expectJson decodePackageList
        }
        |> Script.onError
            (\error ->
                Script.fail ("search.json request failed: " ++ httpErrorString error)
            )
        |> Script.thenWith
            (Script.each
                (\packageName ->
                    Http.get networkConnection
                        { url = "https://package.elm-lang.org/packages/" ++ packageName ++ "/latest/elm.json"
                        , expect = Http.expectJson decodeDependencies
                        }
                        |> Script.onError
                            (\error ->
                                Script.fail ("elm.json request failed for " ++ packageName ++ ": " ++ httpErrorString error)
                            )
                        |> Script.thenWith
                            (Script.each
                                (\dependency ->
                                    if (dependency == "tesk9/palette") || (dependency == "avh4/elm-color") then
                                        Script.printLine (packageName ++ " depends on " ++ dependency)

                                    else
                                        Script.do []
                                )
                            )
                )
            )


httpErrorString : Http.Error -> String
httpErrorString error =
    case error of
        Http.Timeout ->
            "Request timed out"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus code ->
            "Bad status " ++ String.fromInt code

        Http.BadBody message ->
            message


decodePackageList : Decoder (List String)
decodePackageList =
    Decode.list (Decode.field "name" Decode.string)


decodeDependencies : Decoder (List String)
decodeDependencies =
    Decode.field "dependencies" (Decode.keyValuePairs Decode.value)
        |> Decode.map (List.map Tuple.first)


main : Script.Program
main =
    Script.program script
