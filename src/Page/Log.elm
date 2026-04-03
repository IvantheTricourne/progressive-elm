module Page.Log exposing
    ( CachedExercise
    , LogEffect(..)
    , LogModel
    , LogStep(..)
    , cacheCurrentState
    , initLog
    , restoreSets
    , updateLog
    , viewLog
    )

import Component.SetRow exposing (viewSetInput, viewSetReadOnly)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)



-- ── Model ─────────────────────────────────────────────────────────────────────


type LogStep
    = PickRoutine
    | LoggingSets
    | FinishConfirm


type alias LogModel =
    { step : LogStep
    , selectedRoutine : Maybe String
    , sessionQueue : List String
    , sessionIdx : Int
    , currentSets : List Set
    , setsCache : Dict String CachedExercise
    , configValue : String
    , draft : Maybe Draft
    }


type alias CachedExercise =
    { sets : List Set
    , config : String
    }


initLog : Maybe Draft -> LogModel
initLog draft =
    { step = PickRoutine
    , selectedRoutine = Nothing
    , sessionQueue = []
    , sessionIdx = 0
    , currentSets = [ emptySet ]
    , setsCache = Dict.empty
    , configValue = ""
    , draft = draft
    }


emptySet : Set
emptySet =
    { weight = Nothing, reps = Nothing, outcome = Clean, note = Nothing }



-- ── Update ────────────────────────────────────────────────────────────────────


updateLog : LogPageMsg -> LogModel -> Db -> Routines -> ( LogModel, Maybe LogEffect )
updateLog msg model db routines =
    case msg of
        SelectRoutine key ->
            ( { model | selectedRoutine = Just key }, Nothing )

        StartSession ->
            case model.selectedRoutine of
                Nothing ->
                    ( model, Nothing )

                Just key ->
                    let
                        queue =
                            Dict.get key routines
                                |> Maybe.map .exercises
                                |> Maybe.withDefault []
                    in
                    ( { model
                        | step = LoggingSets
                        , sessionQueue = queue
                        , sessionIdx = 0
                        , setsCache = Dict.empty
                        , currentSets = [ emptySet ]
                        , configValue =
                            List.head queue
                                |> Maybe.andThen (\a -> Dict.get a db)
                                |> Maybe.andThen .defaultConfig
                                |> Maybe.withDefault ""
                      }
                    , Nothing
                    )

        ResumeSession ->
            case model.draft of
                Nothing ->
                    ( model, Nothing )

                Just draft ->
                    let
                        cached =
                            Dict.get (currentAbbr draft) draft.setsCache

                        sets =
                            cached
                                |> Maybe.map
                                    (\c ->
                                        if List.isEmpty c.sets then
                                            [ emptySet ]

                                        else
                                            c.sets
                                    )
                                |> Maybe.withDefault [ emptySet ]

                        config =
                            cached |> Maybe.map .config |> Maybe.withDefault ""
                    in
                    ( { model
                        | step = LoggingSets
                        , selectedRoutine = Just draft.routine
                        , sessionQueue = draft.sessionQueue
                        , sessionIdx = draft.sessionIdx
                        , setsCache = Dict.map (\_ c -> c) draft.setsCache
                        , currentSets = sets
                        , configValue = config
                      }
                    , Nothing
                    )

        DiscardDraft ->
            ( { model | draft = Nothing }, Just ClearDraftEffect )

        ExitSession ->
            let
                cached =
                    cacheCurrentState model

                newCache =
                    Dict.insert (currentExAbbr model) cached model.setsCache
            in
            ( { model
                | step = PickRoutine
                , selectedRoutine = Nothing
                , setsCache = newCache
              }
            , Just (SaveDraftEffect (buildDraft model newCache))
            )

        NavExercise dir ->
            let
                newIdx =
                    model.sessionIdx + dir

                cached =
                    cacheCurrentState model

                updatedCache =
                    Dict.insert (currentExAbbr model) cached model.setsCache

                newAbbr =
                    List.drop newIdx model.sessionQueue |> List.head |> Maybe.withDefault ""

                restoredSets =
                    Dict.get newAbbr updatedCache
                        |> Maybe.map
                            (\c ->
                                if List.isEmpty c.sets then
                                    [ emptySet ]

                                else
                                    c.sets
                            )
                        |> Maybe.withDefault [ emptySet ]

                restoredConfig =
                    Dict.get newAbbr updatedCache
                        |> Maybe.map .config
                        |> Maybe.withDefault ""
            in
            ( { model
                | sessionIdx = newIdx
                , setsCache = updatedCache
                , currentSets = restoredSets
                , configValue = restoredConfig
              }
            , Nothing
            )

        UpdateWeight i val ->
            ( { model | currentSets = updateSetAt i (\s -> { s | weight = String.toFloat val }) model.currentSets }
            , Nothing
            )

        UpdateReps i val ->
            ( { model | currentSets = updateSetAt i (\s -> { s | reps = String.toInt val }) model.currentSets }
            , Nothing
            )

        UpdateOutcome i val ->
            ( { model | currentSets = updateSetAt i (\s -> { s | outcome = outcomeFromString val }) model.currentSets }
            , Nothing
            )

        UpdateConfig val ->
            ( { model | configValue = val }, Nothing )

        AddSet ->
            let
                lastWeight =
                    List.reverse model.currentSets
                        |> List.head
                        |> Maybe.andThen .weight

                newSet =
                    { emptySet | weight = lastWeight }
            in
            ( { model | currentSets = model.currentSets ++ [ newSet ] }, Nothing )

        RemoveSet i ->
            ( { model | currentSets = removeAt i model.currentSets }, Nothing )

        SaveCurrentExercise ->
            let
                cached =
                    cacheCurrentState model

                newCache =
                    Dict.insert (currentExAbbr model) cached model.setsCache
            in
            if List.isEmpty cached.sets then
                ( model, Nothing )

            else
                ( { model | setsCache = newCache }
                , Just (FlushDraftEffect (buildDraft model newCache))
                )

        SaveAndNext ->
            let
                cached =
                    cacheCurrentState model

                newCache =
                    Dict.insert (currentExAbbr model) cached model.setsCache

                isLastEx =
                    model.sessionIdx >= List.length model.sessionQueue - 1
            in
            if isLastEx then
                ( { model | setsCache = newCache, step = FinishConfirm }
                , Nothing
                )

            else
                ( { model
                    | sessionIdx = model.sessionIdx + 1
                    , setsCache = newCache
                    , currentSets = restoreSets (model.sessionIdx + 1) newCache model.sessionQueue
                    , configValue = ""
                  }
                , Just (FlushDraftEffect (buildDraft model newCache))
                )

        ShowFinishConfirm ->
            let
                cached =
                    cacheCurrentState model

                newCache =
                    Dict.insert (currentExAbbr model) cached model.setsCache
            in
            ( { model | setsCache = newCache, step = FinishConfirm }, Nothing )

        ConfirmFinish ->
            ( { model
                | step = PickRoutine
                , selectedRoutine = Nothing
                , setsCache = Dict.empty
                , currentSets = [ emptySet ]
              }
            , Just (CommitSessionEffect model.setsCache)
            )

        CancelFinish ->
            ( { model | step = LoggingSets }, Nothing )

        _ ->
            ( model, Nothing )



-- ── Effects ───────────────────────────────────────────────────────────────────


type LogEffect
    = SaveDraftEffect Draft
    | FlushDraftEffect Draft
    | ClearDraftEffect
    | CommitSessionEffect (Dict String CachedExercise)



-- ── Helpers ───────────────────────────────────────────────────────────────────


currentExAbbr : LogModel -> String
currentExAbbr model =
    List.drop model.sessionIdx model.sessionQueue
        |> List.head
        |> Maybe.withDefault ""


currentAbbr : Draft -> String
currentAbbr d =
    List.drop d.sessionIdx d.sessionQueue |> List.head |> Maybe.withDefault ""


cacheCurrentState : LogModel -> CachedExercise
cacheCurrentState model =
    let
        validSets =
            List.filter
                (\s -> s.weight /= Nothing || s.reps /= Nothing)
                model.currentSets
    in
    { sets = validSets, config = model.configValue }


buildDraft : LogModel -> Dict String CachedExercise -> Draft
buildDraft model cache =
    { routine = Maybe.withDefault "" model.selectedRoutine
    , sessionQueue = model.sessionQueue
    , sessionIdx = model.sessionIdx
    , setsCache = Dict.map (\_ c -> { sets = c.sets, config = c.config }) cache
    , savedAt = ""
    }


restoreSets : Int -> Dict String CachedExercise -> List String -> List Set
restoreSets idx cache queue =
    List.drop idx queue
        |> List.head
        |> Maybe.andThen (\a -> Dict.get a cache)
        |> Maybe.map
            (\c ->
                if List.isEmpty c.sets then
                    [ emptySet ]

                else
                    c.sets
            )
        |> Maybe.withDefault [ emptySet ]


updateSetAt : Int -> (Set -> Set) -> List Set -> List Set
updateSetAt i f sets =
    List.indexedMap
        (\j s ->
            if j == i then
                f s

            else
                s
        )
        sets


removeAt : Int -> List a -> List a
removeAt i list =
    List.indexedMap Tuple.pair list
        |> List.filter (\( j, _ ) -> j /= i)
        |> List.map Tuple.second



-- ── View ──────────────────────────────────────────────────────────────────────


viewLog : LogModel -> Db -> Routines -> Html LogPageMsg
viewLog model db routines =
    case model.step of
        PickRoutine ->
            viewPickRoutine model db routines

        LoggingSets ->
            viewLoggingSets model db routines

        FinishConfirm ->
            viewFinishConfirm model routines



-- ── Pick routine ──────────────────────────────────────────────────────────────


viewPickRoutine : LogModel -> Db -> Routines -> Html LogPageMsg
viewPickRoutine model db routines =
    div [ class "p-4" ]
        [ viewDraftBanner model
        , viewLastSessionCard db routines
        , p [ class "text-xs font-mono text-gray-600 uppercase tracking-wider mb-3" ]
            [ text "Today's routine" ]
        , div [ class "grid grid-cols-2 gap-2" ]
            (routines
                |> Dict.filter (\_ r -> not r.archived)
                |> Dict.toList
                |> List.map (viewRoutineCard model db)
            )
        , button
            [ class "mt-4 w-full py-4 rounded-xl font-semibold text-base transition-all"
            , classList
                [ ( "bg-lime-400 text-black hover:bg-lime-300 active:scale-98", model.selectedRoutine /= Nothing )
                , ( "bg-gray-800 text-gray-600 cursor-default", model.selectedRoutine == Nothing )
                ]
            , disabled (model.selectedRoutine == Nothing)
            , onClick StartSession
            ]
            [ text "Start session →" ]
        ]


viewRoutineCard : LogModel -> Db -> ( String, Routine ) -> Html LogPageMsg
viewRoutineCard model db ( key, r ) =
    let
        completeDates =
            getCompleteDates r db

        lastDate =
            List.reverse completeDates |> List.head

        badgeText =
            case lastDate of
                Just d ->
                    "last " ++ String.slice 5 10 d |> String.replace "-" "/"

                Nothing ->
                    "new"

        isSelected =
            model.selectedRoutine == Just key
    in
    div
        [ class "relative bg-gray-900 border rounded-xl p-4 cursor-pointer transition-all min-h-20 active:scale-97"
        , classList
            [ ( "border-lime-400 bg-lime-950", isSelected )
            , ( "border-gray-800", not isSelected )
            ]
        , onClick (SelectRoutine key)
        ]
        [ div [ class "font-mono text-xl font-medium text-white" ] [ text r.name ]
        , div [ class "font-mono text-sm text-gray-400 mt-2" ]
            [ text (String.join " · " r.exercises) ]
        , div
            [ class "absolute top-2 right-2 text-xs font-mono rounded px-1.5 py-0.5"
            , classList
                [ ( "bg-lime-400 text-black", isSelected )
                , ( "bg-gray-800 text-gray-600", not isSelected )
                ]
            ]
            [ text badgeText ]
        ]



-- ── Draft banner ──────────────────────────────────────────────────────────────


viewDraftBanner : LogModel -> Html LogPageMsg
viewDraftBanner model =
    case model.draft of
        Nothing ->
            text ""

        Just draft ->
            div [ class "bg-lime-950 border border-lime-700 rounded-xl p-4 mb-4" ]
                [ div [ class "flex items-center gap-2 mb-2" ]
                    [ div [ class "w-2 h-2 rounded-full bg-lime-400" ] []
                    , span [ class "text-sm font-medium text-lime-400" ]
                        [ text ("Draft — " ++ draft.routine) ]
                    ]
                , div [ class "text-xs font-mono text-gray-400 mb-3" ]
                    [ text ("Saved " ++ draft.savedAt) ]
                , div [ class "flex gap-2" ]
                    [ button
                        [ class "flex-1 py-2 bg-lime-400 text-black rounded-lg text-sm font-semibold"
                        , onClick ResumeSession
                        ]
                        [ text "Resume →" ]
                    , button
                        [ class "px-4 py-2 border border-gray-700 text-gray-500 rounded-lg text-sm"
                        , onClick DiscardDraft
                        ]
                        [ text "Discard" ]
                    ]
                ]



-- ── Last session card ─────────────────────────────────────────────────────────


viewLastSessionCard : Db -> Routines -> Html LogPageMsg
viewLastSessionCard db routines =
    let
        latestComplete =
            routines
                |> Dict.filter (\_ r -> not r.archived)
                |> Dict.toList
                |> List.filterMap
                    (\( key, r ) ->
                        getCompleteDates r db
                            |> List.reverse
                            |> List.head
                            |> Maybe.map (\d -> ( key, r, d ))
                    )
                |> List.sortBy (\( _, _, d ) -> d)
                |> List.reverse
                |> List.head
    in
    case latestComplete of
        Nothing ->
            text ""

        Just ( _, r, date ) ->
            div [ class "relative bg-gray-900 border border-gray-800 rounded-xl p-4 mb-4 overflow-hidden" ]
                [ -- Glow effect
                  div [ class "absolute top-0 right-0 w-32 h-32 bg-lime-400 opacity-5 rounded-full -translate-y-1/2 translate-x-1/2 pointer-events-none" ] []
                , div [ class "text-xs font-mono text-gray-600 uppercase tracking-wider mb-3" ]
                    [ text "Last session" ]
                , div [ class "flex items-end justify-between mb-3" ]
                    [ div [ class "font-mono text-3xl font-medium text-white leading-none tracking-tight" ]
                        [ text r.name ]
                    , div [ class "text-right" ]
                        [ div [ class "font-mono text-xs text-gray-400" ] [ text date ]
                        ]
                    ]
                , div [ class "border-t border-gray-800 pt-3" ]
                    (List.map (viewLastSessionExRow db date) r.exercises)
                ]


viewLastSessionExRow : Db -> String -> String -> Html LogPageMsg
viewLastSessionExRow db date abbr =
    let
        sess =
            Dict.get abbr db
                |> Maybe.andThen (\ex -> List.filter (\s -> s.date == date) ex.sessions |> List.head)
    in
    case sess of
        Nothing ->
            text ""

        Just s ->
            div [ class "flex items-center gap-2 mb-2" ]
                [ span [ class "font-mono text-xs text-gray-600 w-8 shrink-0" ] [ text abbr ]
                , div [ class "flex gap-1 flex-wrap flex-1" ]
                    (List.map viewSetChip s.sets)
                ]


viewSetChip : Set -> Html msg
viewSetChip s =
    let
        label =
            case ( s.weight, s.reps ) of
                ( Just w, Just r ) ->
                    String.fromFloat w ++ "×" ++ String.fromInt r

                _ ->
                    "—"

        chipClass =
            case s.outcome of
                Warmup ->
                    "text-gray-600 border-gray-800 bg-gray-900"

                TooLight ->
                    "text-red-400 border-red-900 bg-red-950"

                AlmostFailed ->
                    "text-amber-400 border-amber-900 bg-amber-950"

                Partial ->
                    "text-blue-400 border-blue-900 bg-blue-950"

                Clean ->
                    "text-gray-400 border-gray-800 bg-gray-900"
    in
    span [ class ("font-mono text-xs rounded px-1.5 py-0.5 border " ++ chipClass) ]
        [ text label ]



-- ── Logging sets ─────────────────────────────────────────────────────────────


viewLoggingSets : LogModel -> Db -> Routines -> Html LogPageMsg
viewLoggingSets model db routines =
    let
        abbr =
            currentExAbbr model

        ex =
            Dict.get abbr db

        routineName =
            model.selectedRoutine
                |> Maybe.andThen (\k -> Dict.get k routines)
                |> Maybe.map .name
                |> Maybe.withDefault ""

        lastSess =
            ex |> Maybe.andThen (\e -> List.reverse e.sessions |> List.head)

        isFirst =
            model.sessionIdx == 0

        isLast =
            model.sessionIdx >= List.length model.sessionQueue - 1

        nextAbbr =
            List.drop (model.sessionIdx + 1) model.sessionQueue |> List.head
    in
    div [ class "p-4" ]
        [ -- Topbar
          div [ class "flex items-center gap-3 mb-4" ]
            [ button
                [ class "w-9 h-9 rounded-full border border-gray-700 bg-gray-900 text-gray-400 flex items-center justify-center text-base hover:border-red-500 hover:text-red-400"
                , onClick ExitSession
                ]
                [ text "←" ]
            , div [ class "flex-1 min-w-0" ]
                [ div [ class "text-xs font-mono text-gray-600 uppercase tracking-wider bg-gray-900 border border-gray-800 rounded px-2 py-0.5 inline-block mb-1" ]
                    [ text routineName ]
                , div [ class "flex items-center gap-1" ]
                    (List.indexedMap
                        (\i _ ->
                            div
                                [ classList
                                    [ ( "w-2 h-2 rounded-full", True )
                                    , ( "bg-lime-400", i == model.sessionIdx )
                                    , ( "bg-lime-700", i < model.sessionIdx )
                                    , ( "bg-gray-700", i > model.sessionIdx )
                                    ]
                                ]
                                []
                        )
                        model.sessionQueue
                        ++ [ span [ class "font-mono text-xs text-gray-600 ml-1" ]
                                [ text (abbr ++ " " ++ String.fromInt (model.sessionIdx + 1) ++ "/" ++ String.fromInt (List.length model.sessionQueue)) ]
                           ]
                    )
                ]
            ]

        -- Exercise header
        , div [ class "flex items-center justify-between mb-4" ]
            [ div []
                [ div [ class "font-mono text-xl text-white" ] [ text abbr ]
                , div [ class "text-xs text-gray-600 mt-0.5" ]
                    [ text (ex |> Maybe.map .fullName |> Maybe.withDefault "") ]
                ]
            , div [ class "flex gap-2" ]
                [ button
                    [ class "w-9 h-9 rounded-full border border-gray-700 bg-gray-900 text-white flex items-center justify-center"
                    , disabled isFirst
                    , classList [ ( "opacity-30 cursor-default", isFirst ) ]
                    , onClick (NavExercise -1)
                    ]
                    [ text "‹" ]
                , button
                    [ class "w-9 h-9 rounded-full border border-gray-700 bg-gray-900 text-white flex items-center justify-center"
                    , disabled isLast
                    , classList [ ( "opacity-30 cursor-default", isLast ) ]
                    , onClick (NavExercise 1)
                    ]
                    [ text "›" ]
                ]
            ]

        -- Last session panel
        , div [ class "bg-gray-900 border border-gray-800 rounded-xl p-4 mb-4" ]
            (case lastSess of
                Nothing ->
                    [ span [ class "text-sm text-gray-600" ] [ text "No previous session" ] ]

                Just sess ->
                    [ div [ class "flex items-center justify-between mb-3" ]
                        [ span [ class "text-xs font-mono text-gray-600 uppercase tracking-wider" ]
                            [ text "Last session" ]
                        , span [ class "text-xs font-mono text-gray-400" ] [ text sess.date ]
                        ]
                    , div [ class "flex flex-col gap-1" ]
                        (List.map viewSetReadOnly sess.sets)
                    , case ex |> Maybe.andThen .defaultConfig of
                        Just cfg ->
                            div [ class "mt-3 pt-3 border-t border-gray-800 text-xs text-gray-400 flex items-start gap-1.5" ]
                                [ span [ class "text-amber-400" ] [ text "⚙" ]
                                , text cfg
                                ]

                        Nothing ->
                            text ""
                    ]
            )

        -- New sets
        , p [ class "text-xs font-mono text-gray-600 uppercase tracking-wider mb-2" ]
            [ text "New sets" ]
        , div [ class "flex flex-col gap-2 mb-3" ]
            (List.indexedMap
                (\i s ->
                    viewSetInput
                        { index = i
                        , set = s
                        , isLast = i == List.length model.currentSets - 1
                        , isSingle = List.length model.currentSets == 1
                        , onWeight = UpdateWeight
                        , onReps = UpdateReps
                        , onOutcome = UpdateOutcome
                        , onRemove = RemoveSet
                        , onAdd = AddSet
                        }
                )
                model.currentSets
            )

        -- Config
        , div [ class "flex items-center gap-2 mt-3" ]
            [ span [ class "text-amber-400 text-sm shrink-0" ] [ text "⚙" ]
            , input
                [ type_ "text"
                , placeholder "Machine config (seat, grip, stance…)"
                , value model.configValue
                , class "flex-1 bg-gray-900 border border-gray-800 rounded-lg px-3 py-2.5 text-sm text-gray-400 outline-none focus:border-amber-500 focus:text-white"
                , onInput UpdateConfig
                ]
                []
            ]

        -- Save row
        , div [ class "flex gap-2 mt-4" ]
            [ button
                [ class "flex-1 py-4 bg-lime-400 text-black rounded-xl text-sm font-semibold hover:bg-lime-300 active:scale-98"
                , onClick SaveCurrentExercise
                ]
                [ text "Save" ]
            , button
                [ class "px-4 py-4 bg-gray-900 border border-gray-700 text-white rounded-xl text-sm hover:bg-gray-800 whitespace-nowrap"
                , onClick SaveAndNext
                ]
                [ text
                    (if isLast then
                        "Save & finish ✓"

                     else
                        "Save & next ("
                            ++ Maybe.withDefault "" nextAbbr
                            ++ ") →"
                    )
                ]
            ]
        ]



-- ── Finish confirm ────────────────────────────────────────────────────────────


viewFinishConfirm : LogModel -> Routines -> Html LogPageMsg
viewFinishConfirm model routines =
    let
        routineName =
            model.selectedRoutine
                |> Maybe.andThen (\k -> Dict.get k routines)
                |> Maybe.map .name
                |> Maybe.withDefault ""

        logged =
            List.filter
                (\a ->
                    Dict.get a model.setsCache
                        |> Maybe.map (\c -> not (List.isEmpty c.sets))
                        |> Maybe.withDefault False
                )
                model.sessionQueue

        skipped =
            List.filter (\a -> not (List.member a logged)) model.sessionQueue
    in
    div [ class "p-4" ]
        [ div [ class "bg-gray-900 border border-lime-700 rounded-xl p-5 mt-8" ]
            [ div [ class "text-sm font-medium text-lime-400 mb-4" ]
                [ text ("Finish " ++ routineName ++ "?") ]
            , if not (List.isEmpty logged) then
                div [ class "font-mono text-xs text-lime-400 mb-2" ]
                    [ text (String.join " · " logged ++ " logged") ]

              else
                text ""
            , if not (List.isEmpty skipped) then
                div [ class "font-mono text-xs text-gray-600 mb-4" ]
                    [ text (String.join " · " skipped ++ " skipped") ]

              else
                div [ class "mb-4" ] []
            , div [ class "flex gap-2" ]
                [ button
                    [ class "flex-1 py-3 bg-lime-400 text-black rounded-lg text-sm font-semibold hover:bg-lime-300"
                    , onClick ConfirmFinish
                    ]
                    [ text "Confirm ✓" ]
                , button
                    [ class "px-4 py-3 border border-gray-700 text-gray-400 rounded-lg text-sm hover:bg-gray-900"
                    , onClick CancelFinish
                    ]
                    [ text "Keep logging" ]
                ]
            ]
        ]



-- ── Shared helpers ────────────────────────────────────────────────────────────


getCompleteDates : Routine -> Db -> List String
getCompleteDates r db =
    let
        dateSets =
            List.map
                (\a ->
                    Dict.get a db
                        |> Maybe.map (\ex -> ex.sessions |> List.map .date)
                        |> Maybe.withDefault []
                )
                r.exercises

        allDates =
            List.head dateSets |> Maybe.withDefault []
    in
    List.filter
        (\d -> List.all (List.member d) dateSets)
        allDates
        |> List.sort
