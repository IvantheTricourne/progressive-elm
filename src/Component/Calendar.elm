module Component.Calendar exposing
    ( CalendarState
    , initCalendar
    , viewCalStats
    , viewCalendar
    )

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias CalendarState =
    { year : Int
    , month : Int -- 1-indexed
    , selectedDate : Maybe String
    }


initCalendar : Int -> Int -> CalendarState
initCalendar y m =
    { year = y, month = m, selectedDate = Nothing }



-- ── Stats row ─────────────────────────────────────────────────────────────────


viewCalStats :
    { thisMonth : Int
    , thisWeek : Int
    , daysSince : Maybe Int
    }
    -> Html msg
viewCalStats stats =
    div [ class "grid grid-cols-3 gap-2 mb-4" ]
        [ statCard (String.fromInt stats.thisMonth) "this month"
        , statCard (String.fromInt stats.thisWeek) "this week"
        , statCard (formatDaysSince stats.daysSince) "since last"
        ]


statCard : String -> String -> Html msg
statCard val lbl =
    div [ class "bg-gray-900 border border-gray-800 rounded-lg p-3" ]
        [ div [ class "font-mono text-2xl font-medium text-white leading-none" ] [ text val ]
        , div [ class "text-xs text-gray-600 mt-1" ] [ text lbl ]
        ]


formatDaysSince : Maybe Int -> String
formatDaysSince d =
    case d of
        Nothing ->
            "—"

        Just 0 ->
            "today"

        Just 1 ->
            "1d"

        Just n ->
            String.fromInt n ++ "d"



-- ── Calendar grid ─────────────────────────────────────────────────────────────


viewCalendar :
    { state : CalendarState
    , sessionDates : Dict String (List String) -- date → routine names
    , today : String
    , onNav : Int -> msg
    , onSelectDay : String -> msg
    }
    -> Html msg
viewCalendar cfg =
    div []
        [ viewCalNav cfg.state cfg.today cfg.onNav
        , viewCalGrid cfg
        ]


viewCalNav : CalendarState -> String -> (Int -> msg) -> Html msg
viewCalNav state today onNav =
    let
        todayYear =
            String.left 4 today |> String.toInt |> Maybe.withDefault 2026

        todayMonth =
            String.slice 5 7 today |> String.toInt |> Maybe.withDefault 1

        isCurrentMonth =
            state.year == todayYear && state.month == todayMonth
    in
    div [ class "flex items-center justify-between mb-3" ]
        [ button
            [ class "w-8 h-8 rounded-full border border-gray-700 bg-gray-900 text-gray-400 flex items-center justify-center text-base hover:bg-gray-800"
            , onClick (onNav -1)
            ]
            [ text "‹" ]
        , span [ class "font-mono text-sm text-white" ]
            [ text (monthName state.month ++ " " ++ String.fromInt state.year) ]
        , button
            [ class "w-8 h-8 rounded-full border border-gray-700 bg-gray-900 text-gray-400 flex items-center justify-center text-base hover:bg-gray-800"
            , disabled isCurrentMonth
            , classList [ ( "opacity-30 cursor-default", isCurrentMonth ) ]
            , onClick (onNav 1)
            ]
            [ text "›" ]
        ]


viewCalGrid :
    { state : CalendarState
    , sessionDates : Dict String (List String)
    , today : String
    , onNav : Int -> msg
    , onSelectDay : String -> msg
    }
    -> Html msg
viewCalGrid cfg =
    let
        firstDow =
            firstDayOfWeek cfg.state.year cfg.state.month

        -- 0=Sun
        daysInMon =
            daysInMonth cfg.state.year cfg.state.month

        padding =
            List.repeat firstDow ()

        days =
            List.range 1 daysInMon
    in
    div [ class "grid grid-cols-7 gap-1" ]
        (dowHeaders
            ++ List.map (\_ -> div [ class "aspect-square" ] []) padding
            ++ List.map (viewCalDay cfg) days
        )


viewCalDay :
    { state : CalendarState
    , sessionDates : Dict String (List String)
    , today : String
    , onNav : Int -> msg
    , onSelectDay : String -> msg
    }
    -> Int
    -> Html msg
viewCalDay cfg day =
    let
        dateStr =
            String.fromInt cfg.state.year
                ++ "-"
                ++ String.padLeft 2 '0' (String.fromInt cfg.state.month)
                ++ "-"
                ++ String.padLeft 2 '0' (String.fromInt day)

        routineNames =
            Dict.get dateStr cfg.sessionDates |> Maybe.withDefault []

        hasSession =
            not (List.isEmpty routineNames)

        isToday =
            dateStr == cfg.today

        isSelected =
            cfg.state.selectedDate == Just dateStr

        baseClass =
            "aspect-square rounded-lg flex flex-col items-center justify-center font-mono text-xs border "

        dayClass =
            if hasSession then
                baseClass
                    ++ "bg-lime-950 border-lime-900 text-white cursor-pointer "
                    ++ (if isSelected then
                            "border-lime-400 "

                        else
                            ""
                       )
                    ++ (if isToday then
                            "border-lime-400 "

                        else
                            ""
                       )

            else if isToday then
                baseClass ++ "bg-gray-900 border-gray-700 text-gray-400 "

            else
                baseClass ++ "bg-gray-900 border-gray-800 text-gray-600 "

        -- Show last letter of routine name (A, B, A+, etc.)
        routineLabel =
            routineNames
                |> List.head
                |> Maybe.map (\n -> String.right 1 n)
                |> Maybe.withDefault ""
    in
    div
        ([ class dayClass ]
            ++ (if hasSession then
                    [ onClick (cfg.onSelectDay dateStr) ]

                else
                    []
               )
        )
        [ text (String.fromInt day)
        , if hasSession then
            div [ class "text-lime-600 text-xs leading-none mt-0.5" ] [ text routineLabel ]

          else
            text ""
        ]


dowHeaders : List (Html msg)
dowHeaders =
    [ "S", "M", "T", "W", "T", "F", "S" ]
        |> List.map
            (\d ->
                div [ class "text-center text-xs font-mono text-gray-600 pb-1" ]
                    [ text d ]
            )



-- ── Date helpers ──────────────────────────────────────────────────────────────


monthName : Int -> String
monthName m =
    case m of
        1 ->
            "January"

        2 ->
            "February"

        3 ->
            "March"

        4 ->
            "April"

        5 ->
            "May"

        6 ->
            "June"

        7 ->
            "July"

        8 ->
            "August"

        9 ->
            "September"

        10 ->
            "October"

        11 ->
            "November"

        _ ->
            "December"


daysInMonth : Int -> Int -> Int
daysInMonth y m =
    case m of
        2 ->
            if modBy 4 y == 0 && (modBy 100 y /= 0 || modBy 400 y == 0) then
                29

            else
                28

        4 ->
            30

        6 ->
            30

        9 ->
            30

        11 ->
            30

        _ ->
            31



-- Day of week for 1st of month, 0=Sun using Tomohiko Sakamoto's algorithm


firstDayOfWeek : Int -> Int -> Int
firstDayOfWeek year month =
    let
        t =
            [ 0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4 ]

        y =
            if month < 3 then
                year - 1

            else
                year

        m =
            month

        tableVal =
            List.drop (m - 1) t |> List.head |> Maybe.withDefault 0
    in
    modBy 7 (y + y // 4 - y // 100 + y // 400 + tableVal + 1)
