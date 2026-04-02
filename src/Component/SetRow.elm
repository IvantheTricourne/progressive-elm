module Component.SetRow exposing (viewSetInput, viewSetReadOnly)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)



-- ── Read-only set row (last session reference) ────────────────────────────────


viewSetReadOnly : Set -> Html msg
viewSetReadOnly s =
    div [ class "flex items-center gap-2 py-1" ]
        [ span [ class "font-mono text-sm w-14 text-white" ]
            [ text (formatWeight s.weight) ]
        , span [ class "font-mono text-sm text-gray-400" ]
            [ text (formatReps s.reps) ]
        , viewOutcomeBadge s.outcome
        , case s.note of
            Just n ->
                span [ class "text-xs text-gray-600 ml-1" ] [ text n ]

            Nothing ->
                text ""
        ]



-- ── Editable set row ──────────────────────────────────────────────────────────


type alias SetRowConfig msg =
    { index : Int
    , set : Set
    , isLast : Bool
    , isSingle : Bool
    , onWeight : Int -> String -> msg
    , onReps : Int -> String -> msg
    , onOutcome : Int -> String -> msg
    , onRemove : Int -> msg
    , onAdd : msg
    }


viewSetInput : SetRowConfig msg -> Html msg
viewSetInput cfg =
    div [ class "flex items-center gap-2 bg-gray-900 border border-gray-800 rounded-lg px-3 py-2" ]
        [ span [ class "font-mono text-xs text-gray-600 w-4 shrink-0" ]
            [ text (String.fromInt (cfg.index + 1)) ]

        -- Weight input
        , input
            [ type_ "number"
            , attribute "inputmode" "decimal"
            , placeholder "lb"
            , value (Maybe.withDefault "" (Maybe.map String.fromFloat cfg.set.weight))
            , class "bg-gray-800 border border-gray-700 rounded px-2 py-1.5 font-mono text-sm text-white w-16 outline-none focus:border-lime-400"
            , onInput (cfg.onWeight cfg.index)
            ]
            []

        -- Reps input
        , input
            [ type_ "number"
            , attribute "inputmode" "numeric"
            , placeholder "reps"
            , value (Maybe.withDefault "" (Maybe.map String.fromInt cfg.set.reps))
            , class "bg-gray-800 border border-gray-700 rounded px-2 py-1.5 font-mono text-sm text-white w-12 outline-none focus:border-lime-400"
            , onInput (cfg.onReps cfg.index)
            ]
            []

        -- Outcome select
        , select
            [ class "bg-gray-800 border border-gray-700 rounded px-1.5 py-1.5 font-mono text-xs text-gray-300 flex-1 min-w-0 outline-none focus:border-lime-400"
            , onInput (cfg.onOutcome cfg.index)
            , value (outcomeToString cfg.set.outcome)
            ]
            (List.map
                (\( val, label ) ->
                    option
                        [ value val
                        , selected (val == outcomeToString cfg.set.outcome)
                        ]
                        [ text label ]
                )
                outcomeOptions
            )

        -- Delete button (not on last row)
        , if cfg.isLast || cfg.isSingle then
            text ""

          else
            button
                [ class "text-gray-600 hover:text-red-400 text-lg leading-none px-1 shrink-0"
                , onClick (cfg.onRemove cfg.index)
                ]
                [ text "×" ]

        -- Add button (only on last row)
        , if cfg.isLast then
            button
                [ class "w-7 h-7 rounded-full border border-gray-700 bg-gray-800 text-lime-400 text-lg flex items-center justify-center shrink-0 hover:border-lime-400"
                , onClick cfg.onAdd
                ]
                [ text "+" ]

          else
            text ""
        ]



-- ── Outcome options ───────────────────────────────────────────────────────────


outcomeOptions : List ( String, String )
outcomeOptions =
    [ ( "", "Clean — all reps done" )
    , ( "-", "- Almost — failed next rep" )
    , ( "+", "+ Warmup set" )
    , ( "/", "/ Partial — last rep assisted" )
    , ( "++", "++ Too light — skip next time" )
    ]



-- ── Outcome badge ─────────────────────────────────────────────────────────────


viewOutcomeBadge : Outcome -> Html msg
viewOutcomeBadge outcome =
    case outcome of
        Warmup ->
            span [ class "text-xs font-mono rounded px-1 py-0.5 bg-lime-950 text-lime-400 border border-lime-900" ]
                [ text "warmup" ]

        TooLight ->
            span [ class "text-xs font-mono rounded px-1 py-0.5 bg-red-950 text-red-400 border border-red-900" ]
                [ text "too light" ]

        AlmostFailed ->
            span [ class "text-xs font-mono rounded px-1 py-0.5 bg-amber-950 text-amber-400 border border-amber-900" ]
                [ text "almost" ]

        Partial ->
            span [ class "text-xs font-mono rounded px-1 py-0.5 bg-blue-950 text-blue-400 border border-blue-900" ]
                [ text "partial" ]

        Clean ->
            span [ class "text-xs font-mono rounded px-1 py-0.5 bg-gray-800 text-gray-600" ]
                [ text "✓" ]



-- ── Helpers ───────────────────────────────────────────────────────────────────


formatWeight : Maybe Float -> String
formatWeight w =
    case w of
        Just v ->
            String.fromFloat v ++ " lb"

        Nothing ->
            "——"


formatReps : Maybe Int -> String
formatReps r =
    case r of
        Just v ->
            String.fromInt v ++ " reps"

        Nothing ->
            "——"
