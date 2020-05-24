module Debug.Control exposing
    ( Control
    , value
    , bool, string, stringTextarea, date
    , values, maybe, choice, list, record, field
    , map
    , view, currentValue, allValues
    , lazy
    , float
    )

{-| Create interactive controls for complex data structures.

@docs Control
@docs value
@docs bool, string, stringTextarea, date
@docs values, maybe, choice, list, record, field
@docs map

@docs view, currentValue, allValues
@docs lazy

-}

import DateTimePicker
import Float.Extra as Float
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Styled
import Json.Decode
import Json.Encode
import Round
import String
import Time
import Time.Extra


{-| An interactive control that produces a value `a`.
-}
type Control a
    = Control
        { currentValue : () -> a
        , allValues : () -> List a
        , view : () -> ControlView a
        }


type ControlView a
    = NoView
    | SingleView (Html (Control a))
    | FieldViews (List ( String, Html (Control a) ))


{-| A `Control` that has a static value (and no UI).
-}
value : a -> Control a
value initial =
    Control
        { currentValue = \() -> initial
        , allValues = \() -> [ initial ]
        , view = \() -> NoView
        }


{-| A `Control` that chooses between a list of values with a dropdown UI.

The first value will be the initial value.

-}
values : (a -> String) -> List a -> Control a
values toString choices =
    choice (List.map (\x -> ( toString x, value x )) choices)


{-| A `Control` that wraps another control in a `Maybe`, which a checkbox UI.

The `Bool` parameter is the initial value, where `False` is `Nothing`,
and `True` is `Just` with the value of the nested control.

-}
maybe : Bool -> Control a -> Control (Maybe a)
maybe isJust (Control control) =
    Control
        { currentValue =
            \() ->
                if isJust then
                    Just (control.currentValue ())

                else
                    Nothing
        , allValues =
            \() ->
                Nothing
                    :: List.map Just (control.allValues ())
        , view =
            \() ->
                SingleView <|
                    Html.span
                        [ Html.Attributes.style "white-space" "nowrap"
                        ]
                        [ Html.input
                            [ Html.Attributes.type_ "checkbox"
                            , Html.Events.onCheck (\a -> maybe a (Control control))
                            , Html.Attributes.checked isJust
                            ]
                            []
                        , Html.text " "
                        , if isJust then
                            view_ (maybe isJust) (Control control)

                          else
                            Html.text "Nothing"
                        ]
        }


{-| A `Control` that toggles a `Bool` with a checkbox UI.
-}
bool : Bool -> Control Bool
bool initialValue =
    Control
        { currentValue = \() -> initialValue
        , allValues =
            \() ->
                [ initialValue
                , not initialValue
                ]
        , view =
            \() ->
                SingleView <|
                    Html.span []
                        [ Html.input
                            [ Html.Attributes.type_ "checkbox"
                            , Html.Events.onCheck bool
                            , Html.Attributes.checked initialValue
                            ]
                            []
                        , Html.text " "
                        , case initialValue of
                            True ->
                                Html.text "True"

                            False ->
                                Html.text "False"
                        ]
        }


slider :
    List (Html.Attribute Float)
    -> { min : Float, max : Float, step : Maybe Float }
    -> Float
    -> Html Float
slider attributes { min, max, step } =
    let
        targetValueDecoder current =
            Json.Decode.map (String.toFloat >> Maybe.withDefault current)
                Html.Events.targetValue

        newValueDecoder current =
            targetValueDecoder current
                |> Json.Decode.andThen
                    (\new ->
                        if new /= current then
                            Json.Decode.succeed new

                        else
                            Json.Decode.fail "value did not change"
                    )

        commonAttributes =
            Html.Attributes.property "min" (Json.Encode.float min)
                :: Html.Attributes.property "max" (Json.Encode.float max)
                :: Html.Attributes.property "step"
                    (case step of
                        Nothing ->
                            Json.Encode.string "any"

                        Just stepValue ->
                            Json.Encode.float stepValue
                    )
                :: attributes
    in
    \current ->
        Html.input
            (Html.Attributes.type_ "range"
                :: Html.Attributes.property "value" (Json.Encode.float current)
                :: Html.Events.on "input" (targetValueDecoder current)
                :: Html.Events.on "change" (targetValueDecoder current)
                :: commonAttributes
            )
            []


{-| A `Control` that toggles a `Bool` with a checkbox UI.
-}
float : { min : Float, max : Float } -> Float -> Control Float
float range initialValue =
    Control
        { currentValue = \() -> initialValue
        , allValues =
            \() ->
                Float.range
                    { start = range.min
                    , end = range.max
                    , steps = 5
                    }
        , view =
            \() ->
                SingleView <|
                    Html.div []
                        [ slider [] { min = range.min, max = range.max, step = Nothing } initialValue
                            |> Html.map (float range)
                        , Html.text " "
                        , Html.text (Round.round 2 initialValue)
                        ]
        }


{-| A `Control` that allows text input.
-}
string : String -> Control String
string initialValue =
    Control
        { currentValue = \() -> initialValue
        , allValues =
            \() ->
                [ initialValue
                , ""
                , "short"
                , "Longwordyesverylongwithnospacessupercalifragilisticexpialidocious"
                , "Long text lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
                ]
        , view =
            \() ->
                SingleView <|
                    Html.input
                        [ Html.Attributes.value initialValue
                        , Html.Events.onInput string
                        ]
                        []
        }


{-| A `Control` that allows multiline text input.
-}
stringTextarea : String -> Control String
stringTextarea initialValue =
    Control
        { currentValue = \() -> initialValue
        , allValues =
            \() ->
                [ initialValue
                , ""
                , "short"
                , "Longwordyesverylongwithnospacessupercalifragilisticexpialidocious"
                , """
                    Long text lorem ipsum dolor sit amet, consectetur adipiscing elit,
                    sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.

                    Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris
                    nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in
                    reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
                    Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
                  """
                ]
        , view =
            \() ->
                SingleView <|
                    Html.textarea
                        [ Html.Attributes.value initialValue
                        , Html.Events.onInput stringTextarea
                        ]
                        []
        }


{-| A `Control` that allows a Date (include date and time) input
with a date picker UI.
-}
date : Time.Zone -> Time.Posix -> Control Time.Posix
date zone initialValue =
    let
        initialDateTime =
            { year = Time.toYear zone initialValue
            , month = Time.toMonth zone initialValue
            , day = Time.toDay zone initialValue
            , hour = Time.toHour zone initialValue
            , minute = Time.toMinute zone initialValue
            }

        toPosix { year, month, day, hour, minute } =
            Time.Extra.partsToPosix zone
                { year = year
                , month = month
                , day = day
                , hour = hour
                , minute = minute
                , second = 0
                , millisecond = 0
                }
    in
    date_ (DateTimePicker.initialStateWithToday initialDateTime) initialDateTime
        |> map toPosix


date_ : DateTimePicker.State -> DateTimePicker.DateTime -> Control DateTimePicker.DateTime
date_ state initialValue =
    Control
        { currentValue = \() -> initialValue
        , allValues = \() -> [ initialValue ] -- TODO
        , view =
            \() ->
                SingleView <|
                    Html.span
                        [ Html.Attributes.style "display" "inline-block"
                        ]
                        [ DateTimePicker.dateTimePicker
                            (\newState newDate ->
                                case newDate of
                                    Nothing ->
                                        date_ newState initialValue

                                    Just d ->
                                        date_ newState d
                            )
                            []
                            state
                            (Just initialValue)
                            |> Html.Styled.toUnstyled
                        ]
        }


{-| A `Control` that chooses between a list of nested controls.

This will crash if you provide an empty list.

The first entry will be the initial value.

-}
choice : List ( String, Control a ) -> Control a
choice choices =
    case choices of
        [] ->
            -- Debug.crash "No choices given"
            choice choices

        first :: rest ->
            choice_ [] first rest


choice_ :
    List ( String, Control a )
    -> ( String, Control a )
    -> List ( String, Control a )
    -> Control a
choice_ left current right =
    Control
        { currentValue = \() -> current |> Tuple.second |> currentValue
        , allValues =
            \() ->
                (List.reverse left ++ [ current ] ++ right)
                    |> List.map (Tuple.second >> allValues)
                    |> List.concat
        , view =
            \() ->
                SingleView <|
                    let
                        option selected ( label, _ ) =
                            Html.option
                                [ Html.Attributes.selected selected ]
                                [ Html.text label ]

                        selectNew i =
                            let
                                all =
                                    List.reverse left
                                        ++ [ current ]
                                        ++ right

                                left_ =
                                    all
                                        |> List.take i
                                        |> List.reverse

                                current_ =
                                    all
                                        |> List.drop i
                                        |> List.head
                                        |> Maybe.withDefault current

                                right_ =
                                    all
                                        |> List.drop (i + 1)
                            in
                            choice_ left_ current_ right_

                        updateChild new =
                            choice_ left ( Tuple.first current, new ) right
                    in
                    Html.div []
                        [ Html.map selectNew <|
                            Html.select
                                [ Html.Events.on "change" (Json.Decode.at [ "target", "selectedIndex" ] Json.Decode.int)
                                ]
                            <|
                                List.concat
                                    [ List.map (option False) <| List.reverse left
                                    , [ option True current ]
                                    , List.map (option False) right
                                    ]
                        , view_ updateChild (Tuple.second current)
                        ]
        }


{-| A `Control` that provides a list of selected length.
-}
list : Control a -> Control (List a)
list itemControl =
    list_ itemControl 1 0 10


list_ : Control a -> Int -> Int -> Int -> Control (List a)
list_ itemControl current min max =
    let
        makeList n =
            allValues itemControl
                |> List.repeat n
                |> List.concat
                |> List.take n
    in
    Control
        { currentValue = \() -> makeList current
        , allValues =
            \() ->
                [ 1, 0, 3 ]
                    |> List.filter (\x -> x > min && x < max)
                    |> (\a -> List.append a [ min, max ])
                    |> List.map makeList
        , view =
            \() ->
                SingleView <|
                    let
                        selectNew new =
                            list_ itemControl new min max
                    in
                    Html.map
                        (String.toInt
                            >> Maybe.withDefault current
                            >> selectNew
                        )
                    <|
                        Html.label []
                            [ Html.text ""
                            , Html.input
                                [ Html.Attributes.type_ "range"
                                , Html.Attributes.min <| String.fromInt min
                                , Html.Attributes.max <| String.fromInt max
                                , Html.Attributes.step <| String.fromInt 1
                                , Html.Attributes.attribute "value" <| String.fromInt current
                                , Html.Events.on "input" Html.Events.targetValue
                                ]
                                []
                            ]
        }


{-| Create a `Control` representing a record with multiple fields.

This uses an API similar to [elm-decode-pipeline](http://package.elm-lang.org/packages/NoRedInk/elm-decode-pipeline/latest).

You will use this with `field`.

    import Debug.Control exposing (field, record, string)

    type alias Point =
        { x : String
        , y : String
        }

    pointControl : Control Point
    pointControl =
        record Point
            |> field "x" (string "initial x value")
            |> field "y" (string "initial y value")

-}
record : a -> Control a
record fn =
    Control
        { currentValue = \() -> fn
        , allValues = \() -> [ fn ]
        , view = \() -> FieldViews []
        }


{-| Used with `record` to create a `Control` representing a record.

See [`record`](#record).

-}
field : String -> Control a -> Control (a -> b) -> Control b
field name (Control control) (Control pipeline) =
    Control
        { currentValue = \() -> pipeline.currentValue () (control.currentValue ())
        , allValues =
            \() ->
                control.allValues ()
                    |> List.concatMap
                        (\v ->
                            List.map (\p -> p v)
                                (pipeline.allValues ())
                        )
        , view =
            \() ->
                let
                    otherFields =
                        case pipeline.view () of
                            FieldViews fs ->
                                List.map (Tuple.mapSecond (\x -> Html.map (field name (Control control)) x))
                                    fs

                            _ ->
                                []

                    newView =
                        view_ (\v -> field name v (Control pipeline)) (Control control)
                in
                FieldViews (( name, newView ) :: otherFields)
        }


{-| Transform the value produced by a `Control`.
-}
map : (a -> b) -> Control a -> Control b
map fn (Control a) =
    Control
        { currentValue = \() -> fn (a.currentValue ())
        , allValues = mapAllValues fn a.allValues
        , view = \() -> mapView fn (a.view ())
        }


{-| Use lazy when working with recursive types:

    import Debug.Control as Control exposing (Control)

    type RecursiveType
        = RecursiveType (Maybe RecursiveType)

    recursiveTypeControl : Control RecursiveType
    recursiveTypeControl =
        Control.choice
            [ ( "No child", Control.value Nothing )
            , ( "child", Control.lazy (\() -> recursiveTypeControl) |> Control.map Just )
            ]
            |> Control.map RecursiveType

-}
lazy : (() -> Control a) -> Control a
lazy fn =
    let
        unwrap (Control v) =
            v
    in
    Control
        { currentValue = \() -> (unwrap (fn ())).currentValue ()
        , allValues = \() -> (unwrap (fn ())).allValues ()
        , view = \() -> (unwrap (fn ())).view ()
        }


mapAllValues : (a -> b) -> (() -> List a) -> (() -> List b)
mapAllValues fn allValues_ =
    \() -> List.map fn (allValues_ ())


mapView : (a -> b) -> ControlView a -> ControlView b
mapView fn controlView =
    case controlView of
        NoView ->
            NoView

        SingleView v ->
            SingleView (Html.map (map fn) v)

        FieldViews fs ->
            FieldViews
                (List.map (Tuple.mapSecond (Html.map (map fn))) fs)


{-| Gets the current value of a `Control`.
-}
currentValue : Control a -> a
currentValue (Control c) =
    c.currentValue ()


{-| TODO: revise API
-}
allValues : Control a -> List a
allValues (Control c) =
    c.allValues ()


{-| Renders the interactive UI for a `Control`.
-}
view : (Control a -> msg) -> Control a -> Html msg
view msg (Control c) =
    Html.div []
        [ view_ msg (Control c)
        ]


view_ : (Control a -> msg) -> Control a -> Html msg
view_ msg (Control c) =
    case c.view () of
        NoView ->
            Html.text ""

        SingleView v ->
            Html.map msg v

        FieldViews fs ->
            let
                fieldRow ( name, fieldView ) =
                    Html.label
                        [ Html.Attributes.style "display" "table-row"
                        , Html.Attributes.style "vertical-align" "text-top"
                        ]
                        [ Html.span
                            [ Html.Attributes.style "display" "table-cell"
                            ]
                            [ Html.text "," ]
                        , Html.span
                            [ Html.Attributes.style "display" "table-cell"
                            , Html.Attributes.style "text-align" "right"
                            ]
                            [ Html.text name ]
                        , Html.span
                            [ Html.Attributes.style "display" "table-cell"
                            ]
                            [ Html.text " = " ]
                        , Html.div
                            [ Html.Attributes.style "display" "table-cell"
                            ]
                            [ fieldView ]
                        ]
            in
            List.concat
                [ [ Html.div
                        [ Html.Attributes.style "display" "table-row"
                        , Html.Attributes.style "vertical-align" "text-top"
                        ]
                        [ Html.div
                            [ Html.Attributes.style "display" "table-cell" ]
                            [ Html.text "{" ]
                        ]
                  ]
                , fs
                    |> List.reverse
                    |> List.map fieldRow
                , [ Html.div
                        [ Html.Attributes.style "display" "table-row"
                        ]
                        [ Html.div
                            [ Html.Attributes.style "display" "table-cell"
                            ]
                            [ Html.text "}" ]
                        ]
                  ]
                ]
                |> Html.div
                    [ Html.Attributes.style "display" "table"
                    , Html.Attributes.style "border-spacing" "2px"
                    ]
                |> Html.map msg
