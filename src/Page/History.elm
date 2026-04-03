module Page.History exposing
    ( HistoryModel
    , initHistory
    , updateHistory
    , viewHistory
    )

import Component.Badge exposing (..)
import Component.Calendar exposing (..)
import Component.SetRow exposing (viewSetReadOnly)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)
import Set as S



-- ── Model ─────────────────────────────────────────────────────────────────────


type alias HistoryModel =
    { view : HistoryView
    , selectedEx : String
    , selectedRoutine : Maybe String
    , calendar : CalendarState
    , today : String
    }


initHistory : String -> List String -> HistoryModel
initHistory today exKeys =
    let
        year =
            String.left 4 today |> String.toInt |> Maybe.withDefault 2026

        month =
            String.slice 5 7 today |> String.toInt |> Maybe.withDefault 1
    in
    { view = CalendarView
    , selectedEx = List.head exKeys |> Maybe.withDefault "LP"
    , selectedRoutine = Nothing
    , calendar = initCalendar year month
    , today = today
    }



-- ── Update ────────────────────────────────────────────────────────────────────


updateHistory : HistoryPageMsg -> HistoryModel -> HistoryModel
updateHistory msg model =
    case msg of
        SetHistoryView v ->
            { model | view = v }

        SelectExercise abbr ->
            { model | selectedEx = abbr, view = ByExercise }

        SelectRoutineSummary key ->
            { model | selectedRoutine = Just key }

        CalendarNav dir ->
            let
                cal =
                    model.calendar

                newM =
                    cal.month + dir

                ( y, m ) =
                    if newM > 12 then
                        ( cal.year + 1, 1 )

                    else if newM < 1 then
                        ( cal.year - 1, 12 )

                    else
                        ( cal.year, newM )
            in
            { model | calendar = { cal | year = y, month = m, selectedDate = Nothing } }

        SelectCalDay date ->
            let
                cal =
                    model.calendar
            in
            { model | calendar = { cal | selectedDate = Just date } }



-- ── View ──────────────────────────────────────────────────────────────────────


viewHistory : HistoryModel -> Db -> Routines -> List String -> Html HistoryPageMsg
viewHistory model db routines exKeys =
    div [ class "p-4" ]
        [ p [ class "text-xs font-mono text-gray-600 uppercase tracking-wider mb-3" ]
            [ text "History" ]
        , viewToggle model.view
        , case model.view of
            ByExercise ->
                viewByExercise model db exKeys

            ByRoutine ->
                viewByRoutine model db routines

            CalendarView ->
                viewCalendarView model db routines
        ]


viewToggle : HistoryView -> Html HistoryPageMsg
viewToggle current =
    div [ class "flex gap-1 bg-gray-900 border border-gray-800 rounded-lg p-0.5 mb-4" ]
        [ toggleBtn "By exercise" (current == ByExercise) (SetHistoryView ByExercise)
        , toggleBtn "By routine" (current == ByRoutine) (SetHistoryView ByRoutine)
        , toggleBtn "Calendar" (current == CalendarView) (SetHistoryView CalendarView)
        ]


toggleBtn : String -> Bool -> msg -> Html msg
toggleBtn label isActive msg =
    button
        [ class "flex-1 py-2 text-sm rounded-md transition-all"
        , classList
            [ ( "bg-gray-700 text-white", isActive )
            , ( "text-gray-500 hover:text-gray-300", not isActive )
            ]
        , onClick msg
        ]
        [ text label ]



-- ── By Exercise ───────────────────────────────────────────────────────────────


viewByExercise : HistoryModel -> Db -> List String -> Html HistoryPageMsg
viewByExercise model db exKeys =
    let
        ex =
            Dict.get model.selectedEx db
    in
    div []
        [ -- Exercise picker
          div [ class "flex gap-2 overflow-x-auto pb-1 mb-4 no-scrollbar" ]
            (List.map
                (\a ->
                    div
                        [ class "shrink-0 px-4 py-1.5 rounded-full border text-sm font-mono cursor-pointer"
                        , classList
                            [ ( "bg-lime-400 text-black border-lime-400", a == model.selectedEx )
                            , ( "bg-gray-900 text-gray-400 border-gray-800", a /= model.selectedEx )
                            ]
                        , onClick (SelectExercise a)
                        ]
                        [ text a ]
                )
                exKeys
            )
        , case ex of
            Nothing ->
                text ""

            Just e ->
                div []
                    [ viewSessionHistory e ]
        ]


viewSessionHistory : Exercise -> Html HistoryPageMsg
viewSessionHistory ex =
    let
        reversed =
            List.reverse ex.sessions
    in
    div [ class "flex flex-col gap-3" ]
        (List.map
            (\sess ->
                let
                    badges =
                        computeBadges sess ex.sessions

                    topB =
                        topBadge badges
                in
                div [ class "bg-gray-900 border border-gray-800 rounded-xl p-4" ]
                    [ div [ class "flex items-center justify-between mb-3" ]
                        [ span [ class "font-mono text-xs text-gray-600" ]
                            [ text
                                (sess.date
                                    ++ (case ex.defaultConfig of
                                            Just c ->
                                                " · " ++ c

                                            Nothing ->
                                                ""
                                       )
                                )
                            ]
                        , case topB of
                            Just b ->
                                span [ class ("text-xs font-mono rounded px-1.5 py-0.5 border " ++ badgeClass b) ]
                                    [ text (badgeLabel b) ]

                            Nothing ->
                                text ""
                        ]
                    , div [ class "flex flex-col gap-1" ]
                        (List.map viewSetReadOnly sess.sets)
                    ]
            )
            reversed
        )



-- ── By Routine ────────────────────────────────────────────────────────────────


viewByRoutine : HistoryModel -> Db -> Routines -> Html HistoryPageMsg
viewByRoutine model db routines =
    let
        activeRoutines =
            Dict.filter (\_ r -> not r.archived) routines
                |> Dict.toList

        -- Default to first routine with complete sessions
        effectiveKey =
            case model.selectedRoutine of
                Just k ->
                    k

                Nothing ->
                    activeRoutines
                        |> List.filter
                            (\( _, r ) ->
                                let
                                    ds =
                                        List.map (\a -> Dict.get a db |> Maybe.map (\e -> List.map .date e.sessions) |> Maybe.withDefault []) r.exercises
                                in
                                case List.head ds of
                                    Nothing ->
                                        False

                                    Just first ->
                                        List.any (\d -> List.all (List.member d) ds) first
                            )
                        |> List.head
                        |> Maybe.map Tuple.first
                        |> Maybe.withDefault ""
    in
    div []
        [ -- Routine picker
          div [ class "flex gap-2 overflow-x-auto pb-1 mb-4 no-scrollbar" ]
            (List.map
                (\( key, r ) ->
                    let
                        hasComplete =
                            let
                                ds =
                                    List.map (\a -> Dict.get a db |> Maybe.map (\e -> List.map .date e.sessions) |> Maybe.withDefault []) r.exercises
                            in
                            case List.head ds of
                                Nothing ->
                                    False

                                Just first ->
                                    List.any (\d -> List.all (List.member d) ds) first
                    in
                    div
                        [ class "shrink-0 px-4 py-1.5 rounded-full border text-sm font-mono transition-all"
                        , classList
                            [ ( "bg-lime-400 text-black border-lime-400 cursor-pointer", key == effectiveKey )
                            , ( "bg-gray-900 text-gray-400 border-gray-800 cursor-pointer", key /= effectiveKey && hasComplete )
                            , ( "opacity-30 cursor-default", not hasComplete && key /= effectiveKey )
                            ]
                        , if hasComplete then
                            onClick (SelectRoutineSummary key)

                          else
                            class ""
                        ]
                        [ text r.name ]
                )
                activeRoutines
            )

        -- Summary cards
        , case Dict.get effectiveKey routines of
            Nothing ->
                text ""

            Just r ->
                let
                    completeDates =
                        let
                            ds =
                                List.map (\a -> Dict.get a db |> Maybe.map (\e -> List.map .date e.sessions) |> Maybe.withDefault []) r.exercises
                        in
                        case List.head ds of
                            Nothing ->
                                []

                            Just first ->
                                List.filter (\d -> List.all (List.member d) ds) first

                    completeDateSet =
                        S.fromList completeDates
                in
                div [ class "flex flex-col gap-2" ]
                    (List.map
                        (\abbr ->
                            viewRoutineSummaryCard abbr db completeDateSet model.today
                        )
                        r.exercises
                    )
        ]


viewRoutineSummaryCard : String -> Db -> S.Set String -> String -> Html HistoryPageMsg
viewRoutineSummaryCard abbr db completeDates today =
    let
        ex =
            Dict.get abbr db

        validSessions =
            ex
                |> Maybe.map (\e -> List.filter (\s -> S.member s.date completeDates) e.sessions)
                |> Maybe.withDefault []

        lastSess =
            List.reverse validSessions |> List.head

        hasData =
            lastSess /= Nothing

        lastWeight =
            lastSess |> Maybe.map (\s -> maxWeightFromSets s.sets)

        lastDate =
            lastSess |> Maybe.map .date

        daysSince =
            lastDate |> Maybe.map (daysAgo today)

        dateLabel =
            case daysSince of
                Nothing ->
                    Nothing

                Just 0 ->
                    Just "today"

                Just 1 ->
                    Just "yesterday"

                Just n ->
                    Just (String.fromInt n ++ "d ago")

        trend =
            if List.length validSessions >= 2 then
                let
                    recent =
                        List.reverse validSessions |> List.take 3 |> List.reverse

                    ws =
                        List.map (\s -> maxWeightFromSets s.sets) recent
                in
                case List.reverse ws of
                    last :: prev :: _ ->
                        if last > prev then
                            Just "↑"

                        else if last < prev then
                            Just "↓"

                        else
                            Just "→"

                    _ ->
                        Just "→"

            else
                Nothing

        badges =
            case lastSess of
                Nothing ->
                    []

                Just s ->
                    computeBadges s validSessions

        topB =
            topBadge badges
    in
    div
        [ class "bg-gray-900 border border-gray-800 rounded-xl p-4 flex items-center gap-3 cursor-pointer hover:border-gray-700"
        , onClick (SelectExercise abbr)
        ]
        [ div [ class "font-mono text-sm font-medium text-white w-10 shrink-0" ]
            [ text abbr ]
        , div [ class "flex-1 min-w-0" ]
            [ div [ class "text-xs text-gray-500 mb-1" ]
                [ text (ex |> Maybe.map .fullName |> Maybe.withDefault "") ]
            , div [ class "font-mono text-lg font-medium text-white" ]
                [ text
                    (case lastWeight of
                        Just w ->
                            String.fromFloat w ++ " lb"

                        Nothing ->
                            "—"
                    )
                ]
            , case dateLabel of
                Just d ->
                    div [ class "font-mono text-xs text-gray-600 mt-0.5" ] [ text d ]

                Nothing ->
                    text ""
            ]
        , div [ class "flex flex-col items-end gap-1 shrink-0" ]
            [ case ( trend, hasData ) of
                ( Just t, True ) ->
                    span
                        [ class "font-mono text-lg"
                        , classList
                            [ ( "text-lime-400", t == "↑" )
                            , ( "text-gray-600", t == "→" )
                            , ( "text-amber-400", t == "↓" )
                            ]
                        ]
                        [ text t ]

                _ ->
                    span [ class "text-xs font-mono text-gray-700 bg-gray-800 border border-gray-700 rounded px-1.5 py-0.5" ]
                        [ text "new" ]
            , case topB of
                Just b ->
                    span [ class ("text-xs font-mono rounded px-1.5 py-0.5 border " ++ badgeClass b) ]
                        [ text (badgeLabel b) ]

                Nothing ->
                    text ""
            ]
        ]



-- ── Calendar view ─────────────────────────────────────────────────────────────


viewCalendarView : HistoryModel -> Db -> Routines -> Html HistoryPageMsg
viewCalendarView model db routines =
    let
        sessionDateMap =
            buildSessionDateMap db routines

        allDates =
            Dict.keys sessionDateMap |> List.sort

        now =
            model.today

        thisMonth =
            List.filter (\d -> String.left 7 d == String.left 7 now) allDates

        thisWeek =
            List.filter (\d -> daysAgo now d >= 0 && daysAgo now d < 7) allDates

        daysSinceVal =
            List.reverse allDates |> List.head |> Maybe.map (daysAgo now)
    in
    div []
        [ viewCalStats
            { thisMonth = List.length thisMonth
            , thisWeek = List.length thisWeek
            , daysSince = daysSinceVal
            }
        , viewCalendar
            { state = model.calendar
            , sessionDates = sessionDateMap
            , today = model.today
            , onNav = CalendarNav
            , onSelectDay = SelectCalDay
            }
        , case model.calendar.selectedDate of
            Nothing ->
                text ""

            Just date ->
                viewCalDayDetail date db routines sessionDateMap
        ]


viewCalDayDetail : String -> Db -> Routines -> Dict String (List String) -> Html HistoryPageMsg
viewCalDayDetail date db routines sessionDateMap =
    let
        routineNames =
            Dict.get date sessionDateMap |> Maybe.withDefault []

        completedRoutines =
            routines
                |> Dict.filter (\_ r -> List.member r.name routineNames)
                |> Dict.values

        exercises =
            completedRoutines
                |> List.concatMap .exercises
                |> dedupe
    in
    div [ class "bg-gray-900 border border-gray-800 rounded-xl p-4 mt-3" ]
        [ div [ class "font-mono text-xs text-gray-600 uppercase tracking-wider mb-4" ]
            [ text (date ++ " · " ++ String.join ", " routineNames) ]
        , div [ class "flex flex-col gap-4" ]
            (List.filterMap
                (\abbr ->
                    Dict.get abbr db
                        |> Maybe.andThen (\ex -> List.filter (\s -> s.date == date) ex.sessions |> List.head |> Maybe.map (\sess -> ( abbr, ex, sess )))
                )
                exercises
                |> List.map
                    (\( abbr, ex, sess ) ->
                        div []
                            [ div [ class "font-mono text-xs text-gray-400 mb-2" ]
                                [ text (abbr ++ " — " ++ ex.fullName) ]
                            , div [ class "flex flex-col gap-1" ]
                                (List.map viewSetReadOnly sess.sets)
                            ]
                    )
            )
        ]



-- ── Helpers ───────────────────────────────────────────────────────────────────


buildSessionDateMap : Db -> Routines -> Dict String (List String)
buildSessionDateMap db routines =
    routines
        |> Dict.filter (\_ r -> not r.archived)
        |> Dict.values
        |> List.foldl
            (\r acc ->
                let
                    dateSets =
                        List.map
                            (\a ->
                                Dict.get a db
                                    |> Maybe.map (\e -> S.fromList (List.map .date e.sessions))
                                    |> Maybe.withDefault S.empty
                            )
                            r.exercises

                    completeDates =
                        case dateSets of
                            [] ->
                                S.empty

                            first :: rest ->
                                List.foldl S.intersect first rest
                in
                S.foldl
                    (\d innerAcc ->
                        Dict.update d
                            (\existing ->
                                Just (r.name :: Maybe.withDefault [] existing)
                            )
                            innerAcc
                    )
                    acc
                    completeDates
            )
            Dict.empty


maxWeightFromSets : List Set -> Float
maxWeightFromSets sets =
    sets
        |> List.filter (\s -> s.outcome /= Warmup && s.outcome /= TooLight)
        |> List.filterMap .weight
        |> List.foldl Basics.max 0


daysAgo : String -> String -> Int
daysAgo today dateStr =
    let
        parseDateParts s =
            case String.split "-" s of
                [ y, m, d ] ->
                    Maybe.map3 (\yy mm dd -> yy * 365 + mm * 30 + dd)
                        (String.toInt y)
                        (String.toInt m)
                        (String.toInt d)

                _ ->
                    Nothing
    in
    case ( parseDateParts today, parseDateParts dateStr ) of
        ( Just t, Just d ) ->
            t - d

        _ ->
            0


dedupe : List a -> List a
dedupe list =
    List.foldl
        (\x acc ->
            if List.member x acc then
                acc

            else
                acc ++ [ x ]
        )
        []
        list
