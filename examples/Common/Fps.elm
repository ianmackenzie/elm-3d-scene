module Common.Fps exposing
    ( update
    , view
    )

{-| This module is used to show the FPS meter.
We keep the last 50 time deltas and show the
weighted average.
-}

import Html exposing (Html)
import Html.Attributes exposing (style)


update : Float -> List Float -> List Float
update dt fps =
    List.take 50 (dt :: fps)


view : List Float -> Int -> Html a
view fps numBodies =
    let
        average currentWeight sumOfWeights weightedSum list =
            case list of
                [] ->
                    weightedSum / sumOfWeights

                el :: rest ->
                    average
                        (currentWeight * 0.9)
                        (currentWeight + sumOfWeights)
                        (el * currentWeight + weightedSum)
                        rest
    in
    Html.div
        [ style "position" "fixed"
        , style "font-family" "monospaced"
        , style "right" "250px"
        , style "top" "0"
        , style "color" "white"
        ]
        [ Html.span [ style "font" "50px sans-serif" ]
            [ Html.text (String.fromInt (round (1000 / average 1 0 0 fps))) ]
        , Html.text " fps"
        , Html.span
            [ style "font" "50px sans-serif" ]
            [ Html.text (" " ++ String.fromInt numBodies) ]
        , Html.text " bodies"
        ]
