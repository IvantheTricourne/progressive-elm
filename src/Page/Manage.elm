module Page.Manage exposing
    ( ManageEffect(..)
    , ManageModel
    , ManageSubView(..)
    , ManageUpdate
    , initManage
    , updateManage
    , viewManage
    )

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)



-- ── Model ─────────────────────────────────────────────────────────────────────


type ManageSubView
    = RoutineList
    | EditingRoutine (Maybe String) -- Nothing = new
    | ExerciseList
    | AddingExercise


type alias ManageModel =
    { subView : ManageSubView
    , routineNameInput : String
    , selectedExercises : List String
    , exAbbrInput : String
    , exNameInput : String
    }


initManage : ManageModel
initManage =
    { subView = RoutineList
    , routineNameInput = ""
    , selectedExercises = []
    , exAbbrInput = ""
    , exNameInput = ""
    }



-- ── Effects ───────────────────────────────────────────────────────────────────


type ManageEffect
    = SaveRoutinesEffect Routines
    | SaveExerciseEffect String Exercise



-- ── Update ────────────────────────────────────────────────────────────────────


type alias ManageUpdate =
    { model : ManageModel
    , effect : Maybe ManageEffect
    , newDb : Maybe Db
    , newRoutines : Maybe Routines
    }


updateManage : ManagePageMsg -> ManageModel -> Db -> Routines -> ManageUpdate
updateManage msg model db routines =
    let
        noOp m =
            { model = m, effect = Nothing, newDb = Nothing, newRoutines = Nothing }
    in
    case msg of
        ShowNewRoutine ->
            noOp
                { model
                    | subView = EditingRoutine Nothing
                    , routineNameInput = ""
                    , selectedExercises = []
                }

        EditRoutine key ->
            let
                r =
                    Dict.get key routines
            in
            noOp
                { model
                    | subView = EditingRoutine (Just key)
                    , routineNameInput = r |> Maybe.map .name |> Maybe.withDefault ""
                    , selectedExercises = r |> Maybe.map .exercises |> Maybe.withDefault []
                }

        CancelRoutineEdit ->
            noOp { model | subView = RoutineList }

        UpdateRoutineName name ->
            noOp { model | routineNameInput = name }

        ToggleRoutineExercise abbr ->
            let
                newSelected =
                    if List.member abbr model.selectedExercises then
                        List.filter (\a -> a /= abbr) model.selectedExercises

                    else
                        model.selectedExercises ++ [ abbr ]
            in
            noOp { model | selectedExercises = newSelected }

        SaveRoutineEdit ->
            let
                name =
                    String.trim model.routineNameInput
            in
            if String.isEmpty name || List.isEmpty model.selectedExercises then
                noOp model

            else
                let
                    newRoutines =
                        case model.subView of
                            EditingRoutine (Just key) ->
                                Dict.update key
                                    (Maybe.map (\r -> { r | name = name, exercises = model.selectedExercises }))
                                    routines

                            EditingRoutine Nothing ->
                                let
                                    key =
                                        String.replace " " "" name ++ "_" ++ String.fromInt (Dict.size routines)
                                in
                                Dict.insert key
                                    { name = name, exercises = model.selectedExercises, archived = False }
                                    routines

                            _ ->
                                routines
                in
                { model = { model | subView = RoutineList }
                , effect = Just (SaveRoutinesEffect newRoutines)
                , newDb = Nothing
                , newRoutines = Just newRoutines
                }

        CopyRoutine key ->
            case Dict.get key routines of
                Nothing ->
                    noOp model

                Just r ->
                    let
                        newKey =
                            key ++ "_copy"

                        newRoutines =
                            Dict.insert newKey
                                { r | name = r.name ++ " (copy)" }
                                routines
                    in
                    { model = model
                    , effect = Just (SaveRoutinesEffect newRoutines)
                    , newDb = Nothing
                    , newRoutines = Just newRoutines
                    }

        ToggleArchiveRoutine key ->
            let
                newRoutines =
                    Dict.update key
                        (Maybe.map (\r -> { r | archived = not r.archived }))
                        routines
            in
            { model = model
            , effect = Just (SaveRoutinesEffect newRoutines)
            , newDb = Nothing
            , newRoutines = Just newRoutines
            }

        ShowNewExercise ->
            noOp
                { model
                    | subView = AddingExercise
                    , exAbbrInput = ""
                    , exNameInput = ""
                }

        CancelExerciseEdit ->
            noOp { model | subView = ExerciseList }

        UpdateExerciseAbbr v ->
            noOp { model | exAbbrInput = String.toUpper v }

        UpdateExerciseFullName v ->
            noOp { model | exNameInput = v }

        SaveExerciseEdit ->
            let
                abbr =
                    String.trim model.exAbbrInput

                fullName =
                    String.trim model.exNameInput
            in
            if String.isEmpty abbr || String.length abbr < 2 || String.isEmpty fullName then
                noOp model

            else if Dict.member abbr db then
                noOp model

            else
                let
                    newEx =
                        { abbr = abbr
                        , fullName = fullName
                        , sessions = []
                        , defaultConfig = Nothing
                        }

                    newDb =
                        Dict.insert abbr newEx db
                in
                { model = { model | subView = ExerciseList }
                , effect = Just (SaveExerciseEffect abbr newEx)
                , newDb = Just newDb
                , newRoutines = Nothing
                }

        CopyExport ->
            noOp model



-- ── View ──────────────────────────────────────────────────────────────────────


viewManage : ManageModel -> Db -> Routines -> Html ManagePageMsg
viewManage model db routines =
    div [ class "p-4" ]
        (case model.subView of
            RoutineList ->
                [ p [ class "text-xs font-mono text-gray-600 uppercase tracking-wider mb-3" ]
                    [ text "Routines" ]
                , div [ class "flex flex-col gap-2 mb-3" ]
                    (Dict.toList routines |> List.map (viewRoutineRow model))
                , button
                    [ class "w-full py-3 bg-lime-400 text-black rounded-lg text-sm font-semibold mb-6"
                    , onClick ShowNewRoutine
                    ]
                    [ text "+ New routine" ]
                , div [ class "border-t border-gray-800 my-4" ] []
                , p [ class "text-xs font-mono text-gray-600 uppercase tracking-wider mb-3" ]
                    [ text "Exercises" ]
                , div [ class "flex flex-col gap-2 mb-3" ]
                    (Dict.toList db |> List.map viewExerciseRow)
                , button
                    [ class "w-full py-3 bg-lime-400 text-black rounded-lg text-sm font-semibold mb-6"
                    , onClick ShowNewExercise
                    ]
                    [ text "+ New exercise" ]
                , div [ class "border-t border-gray-800 my-4" ] []
                , p [ class "text-xs font-mono text-gray-600 uppercase tracking-wider mb-2" ]
                    [ text "Export data" ]
                , p [ class "text-xs text-gray-500 mb-3" ]
                    [ text "Copy CSV to back up or migrate your data." ]
                , button
                    [ class "w-full py-3 bg-gray-900 border border-gray-700 text-white rounded-lg text-sm"
                    , onClick CopyExport
                    ]
                    [ text "Copy to clipboard" ]
                ]

            EditingRoutine maybeKey ->
                [ viewRoutineEditor model db maybeKey ]

            ExerciseList ->
                [ p [ class "text-xs font-mono text-gray-600 uppercase tracking-wider mb-3" ]
                    [ text "Exercises" ]
                , div [ class "flex flex-col gap-2 mb-3" ]
                    (Dict.toList db |> List.map viewExerciseRow)
                , button
                    [ class "w-full py-3 bg-lime-400 text-black rounded-lg text-sm font-semibold"
                    , onClick ShowNewExercise
                    ]
                    [ text "+ New exercise" ]
                ]

            AddingExercise ->
                [ viewExerciseEditor model ]
        )


viewRoutineRow : ManageModel -> ( String, Routine ) -> Html ManagePageMsg
viewRoutineRow _ ( key, r ) =
    div [ class "bg-gray-900 border border-gray-800 rounded-lg px-4 py-3 flex items-center gap-3" ]
        [ div [ class "flex-1 min-w-0" ]
            [ div [ class "text-sm text-white" ] [ text r.name ]
            , div [ class "text-xs font-mono text-gray-600 mt-0.5" ]
                [ text
                    (String.join " · " r.exercises
                        ++ (if r.archived then
                                " · archived"

                            else
                                ""
                           )
                    )
                ]
            ]
        , button [ class "text-xs text-gray-500 border border-gray-700 rounded px-2 py-1 hover:text-white", onClick (EditRoutine key) ]
            [ text "Edit" ]
        , button [ class "text-xs text-gray-500 border border-gray-700 rounded px-2 py-1 hover:text-white", onClick (CopyRoutine key) ]
            [ text "Copy" ]
        , button
            [ class
                ("text-xs border rounded px-2 py-1 "
                    ++ (if r.archived then
                            "text-gray-500 border-gray-700 hover:text-white"

                        else
                            "text-red-400 border-red-900 hover:text-red-300"
                       )
                )
            , onClick (ToggleArchiveRoutine key)
            ]
            [ text
                (if r.archived then
                    "Restore"

                 else
                    "Archive"
                )
            ]
        ]


viewExerciseRow : ( String, Exercise ) -> Html ManagePageMsg
viewExerciseRow ( abbr, ex ) =
    div [ class "bg-gray-900 border border-gray-800 rounded-lg px-4 py-3 flex items-center gap-3" ]
        [ div [ class "flex-1 min-w-0" ]
            [ div [ class "text-sm text-white" ]
                [ span [ class "font-mono" ] [ text abbr ]
                , text (" — " ++ ex.fullName)
                ]
            , div [ class "text-xs text-gray-600 mt-0.5" ]
                [ text (String.fromInt (List.length ex.sessions) ++ " sessions") ]
            ]
        ]


viewRoutineEditor : ManageModel -> Db -> Maybe String -> Html ManagePageMsg
viewRoutineEditor model db maybeKey =
    div [ class "bg-gray-900 border border-gray-800 rounded-xl p-4" ]
        [ p [ class "text-xs font-mono text-gray-600 uppercase tracking-wider mb-4" ]
            [ text
                (if maybeKey == Nothing then
                    "New routine"

                 else
                    "Edit routine"
                )
            ]
        , div [ class "mb-4" ]
            [ label [ class "text-xs text-gray-400 block mb-1.5" ] [ text "Name" ]
            , input
                [ type_ "text"
                , placeholder "e.g. Push Day"
                , value model.routineNameInput
                , class "w-full bg-gray-800 border border-gray-700 rounded-lg px-3 py-2.5 text-sm text-white outline-none focus:border-lime-400"
                , onInput UpdateRoutineName
                ]
                []
            ]
        , div [ class "mb-4" ]
            [ label [ class "text-xs text-gray-400 block mb-1.5" ] [ text "Exercises" ]
            , div [ class "flex flex-col gap-2" ]
                (Dict.toList db
                    |> List.map
                        (\( abbr, ex ) ->
                            let
                                on =
                                    List.member abbr model.selectedExercises
                            in
                            div
                                [ class "flex items-center gap-3 px-3 py-2.5 rounded-lg border cursor-pointer"
                                , classList
                                    [ ( "bg-lime-950 border-lime-800", on )
                                    , ( "bg-gray-800 border-gray-700", not on )
                                    ]
                                , onClick (ToggleRoutineExercise abbr)
                                ]
                                [ span [ class "font-mono text-sm w-9 shrink-0 text-white" ] [ text abbr ]
                                , span [ class "text-sm text-gray-400 flex-1" ] [ text ex.fullName ]
                                , div
                                    [ class "w-5 h-5 rounded border flex items-center justify-center text-xs"
                                    , classList
                                        [ ( "bg-lime-400 border-lime-400 text-black", on )
                                        , ( "bg-gray-700 border-gray-600", not on )
                                        ]
                                    ]
                                    [ if on then
                                        text "✓"

                                      else
                                        text ""
                                    ]
                                ]
                        )
                )
            ]
        , div [ class "flex gap-2" ]
            [ button
                [ class "flex-1 py-2.5 bg-lime-400 text-black rounded-lg text-sm font-semibold"
                , onClick SaveRoutineEdit
                ]
                [ text "Save" ]
            , button
                [ class "px-4 py-2.5 border border-gray-700 text-gray-400 rounded-lg text-sm"
                , onClick CancelRoutineEdit
                ]
                [ text "Cancel" ]
            ]
        ]


viewExerciseEditor : ManageModel -> Html ManagePageMsg
viewExerciseEditor model =
    div [ class "bg-gray-900 border border-gray-800 rounded-xl p-4" ]
        [ p [ class "text-xs font-mono text-gray-600 uppercase tracking-wider mb-4" ]
            [ text "Add exercise" ]
        , div [ class "mb-4" ]
            [ label [ class "text-xs text-gray-400 block mb-1.5" ]
                [ text "Abbreviation "
                , span [ class "text-gray-600" ] [ text "(2–4 chars)" ]
                ]
            , input
                [ type_ "text"
                , placeholder "e.g. CF"
                , value model.exAbbrInput
                , maxlength 4
                , class "w-full bg-gray-800 border border-gray-700 rounded-lg px-3 py-2.5 text-sm font-mono text-white uppercase outline-none focus:border-lime-400"
                , onInput UpdateExerciseAbbr
                ]
                []
            ]
        , div [ class "mb-4" ]
            [ label [ class "text-xs text-gray-400 block mb-1.5" ] [ text "Full name" ]
            , input
                [ type_ "text"
                , placeholder "e.g. Cable Fly"
                , value model.exNameInput
                , class "w-full bg-gray-800 border border-gray-700 rounded-lg px-3 py-2.5 text-sm text-white outline-none focus:border-lime-400"
                , onInput UpdateExerciseFullName
                ]
                []
            ]
        , div [ class "flex gap-2" ]
            [ button
                [ class "flex-1 py-2.5 bg-lime-400 text-black rounded-lg text-sm font-semibold"
                , onClick SaveExerciseEdit
                ]
                [ text "Save" ]
            , button
                [ class "px-4 py-2.5 border border-gray-700 text-gray-400 rounded-lg text-sm"
                , onClick CancelExerciseEdit
                ]
                [ text "Cancel" ]
            ]
        ]
