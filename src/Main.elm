module Main exposing (main)

import Browser
import Decode exposing (decodeDb, decodeDraft, decodeRoutines)
import Dict exposing (Dict)
import Encode exposing (encodeExercise, encodeRoutines)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E
import Model exposing (..)
import Page.History as History
import Page.Log as Log
import Page.Manage as Manage
import Storage


-- ── Seed data ─────────────────────────────────────────────────────────────────

seedDb : Db
seedDb =
    Dict.fromList
        [ ( "LP",  { abbr = "LP",  fullName = "Leg Press",            sessions = [], defaultConfig = Just "Seat max, narrowish stance" } )
        , ( "BP",  { abbr = "BP",  fullName = "Bench Press",          sessions = [], defaultConfig = Just "S+1 outer grip" } )
        , ( "R",   { abbr = "R",   fullName = "Row",                  sessions = [], defaultConfig = Just "Seat/chest +3" } )
        , ( "RDL", { abbr = "RDL", fullName = "Romanian Deadlift",    sessions = [], defaultConfig = Just "Chucks" } )
        , ( "SP",  { abbr = "SP",  fullName = "Shoulder Press",       sessions = [], defaultConfig = Just "Seat +3, inner grip" } )
        , ( "LPD", { abbr = "LPD", fullName = "Lat Pulldown",         sessions = [], defaultConfig = Nothing } )
        , ( "LC",  { abbr = "LC",  fullName = "Leg Curl",             sessions = [], defaultConfig = Nothing } )
        , ( "CTE", { abbr = "CTE", fullName = "Cable Tricep Extension", sessions = [], defaultConfig = Just "Close straight bar grip" } )
        ]


seedRoutines : Routines
seedRoutines =
    Dict.fromList
        [ ( "A",     { name = "Routine A",  exercises = [ "LP", "BP", "R" ],          archived = False } )
        , ( "B",     { name = "Routine B",  exercises = [ "RDL", "SP", "LPD" ],       archived = False } )
        , ( "Aplus", { name = "Routine A+", exercises = [ "LP", "BP", "R", "LC" ],    archived = False } )
        , ( "Bplus", { name = "Routine B+", exercises = [ "RDL", "SP", "LPD", "CTE" ], archived = False } )
        ]


-- ── Model ─────────────────────────────────────────────────────────────────────

type alias Model =
    { db       : Db
    , routines : Routines
    , tab      : Tab
    , log      : Log.LogModel
    , history  : History.HistoryModel
    , manage   : Manage.ManageModel
    , today    : String
    , loaded   : Bool
    }


-- ── Init ──────────────────────────────────────────────────────────────────────

init : E.Value -> ( Model, Cmd Msg )
init flags =
    let
        today =
            flags
                |> D.decodeValue (D.field "today" D.string)
                |> Result.withDefault "2026-01-01"

        exKeys =
            Dict.keys seedDb
    in
    ( { db       = seedDb
      , routines = seedRoutines
      , tab      = LogTab
      , log      = Log.initLog Nothing
      , history  = History.initHistory today exKeys
      , manage   = Manage.initManage
      , today    = today
      , loaded   = False
      }
    , Storage.loadAll ()
    )


-- ── Update ────────────────────────────────────────────────────────────────────

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StorageLoaded value ->
            let
                db =
                    D.decodeValue (D.field "db" decodeDb) value
                        |> Result.withDefault model.db

                routines =
                    D.decodeValue (D.field "routines" decodeRoutines) value
                        |> Result.withDefault model.routines

                draft =
                    D.decodeValue (D.field "draft" (D.nullable decodeDraft)) value
                        |> Result.withDefault Nothing

                -- Migrate: add any seed exercises missing from stored db
                mergedDb =
                    Dict.foldl
                        (\k v acc ->
                            if Dict.member k acc then acc
                            else Dict.insert k v acc
                        )
                        db
                        seedDb

                -- Migrate: add any seed routines missing from stored routines
                mergedRoutines =
                    Dict.foldl
                        (\k v acc ->
                            if Dict.member k acc then acc
                            else Dict.insert k v acc
                        )
                        routines
                        seedRoutines
            in
            ( { model
                | db       = mergedDb
                , routines = mergedRoutines
                , log      = Log.initLog draft
                , history  = History.initHistory model.today (Dict.keys mergedDb)
                , loaded   = True
              }
            , Cmd.none
            )

        SwitchTab tab ->
            ( { model | tab = tab }, Cmd.none )

        LogMsg logMsg ->
            let
                ( newLog, effect ) =
                    Log.updateLog logMsg model.log model.db model.routines
            in
            case effect of
                Nothing ->
                    ( { model | log = newLog }, Cmd.none )

                Just eff ->
                    handleLogEffect eff newLog model

        HistoryMsg histMsg ->
            ( { model | history = History.updateHistory histMsg model.history }
            , Cmd.none
            )

        ManageMsg manMsg ->
            let
                manResult =
                    Manage.updateManage manMsg model.manage model.db model.routines

                newDb =
                    Maybe.withDefault model.db manResult.newDb

                newRoutines =
                    Maybe.withDefault model.routines manResult.newRoutines

                cmd =
                    case manResult.effect of
                        Just (Manage.SaveRoutinesEffect r) ->
                            Storage.saveRoutines
                                { key   = "progressive_routines_v1"
                                , value = E.encode 0 (encodeRoutines r)
                                }

                        Just (Manage.SaveExerciseEffect abbr ex) ->
                            Storage.saveExercise
                                { key   = "progressive_ex_" ++ abbr
                                , value = E.encode 0 (encodeExercise ex)
                                }

                        Nothing ->
                            Cmd.none
            in
            ( { model
                | manage   = manResult.model
                , db       = newDb
                , routines = newRoutines
              }
            , cmd
            )

        NoOp ->
            ( model, Cmd.none )


handleLogEffect : Log.LogEffect -> Log.LogModel -> Model -> ( Model, Cmd Msg )
handleLogEffect effect newLog model =
    case effect of
        Log.SaveDraftEffect draft ->
            ( { model | log = newLog }
            , Storage.saveDraft
                { key   = "progressive_draft_v1"
                , value = E.encode 0 (Encode.encodeDraft draft)
                }
            )

        Log.FlushDraftEffect draft ->
            ( { model | log = newLog }
            , Storage.saveDraft
                { key   = "progressive_draft_v1"
                , value = E.encode 0 (Encode.encodeDraft draft)
                }
            )

        Log.ClearDraftEffect ->
            ( { model | log = newLog }
            , Storage.deleteDraft { key = "progressive_draft_v1" }
            )

        Log.CommitSessionEffect setsCache ->
            let
                today = model.today

                ( newDb, cmds ) =
                    Dict.foldl
                        (\abbr cached ( dbAcc, cmdAcc ) ->
                            case ( Dict.get abbr dbAcc, List.isEmpty cached.sets ) of
                                ( Just ex, False ) ->
                                    let
                                        updatedSessions =
                                            case List.filter (\s -> s.date == today) ex.sessions of
                                                [] ->
                                                    ex.sessions ++ [ { date = today, sets = cached.sets } ]

                                                _ ->
                                                    List.map
                                                        (\s ->
                                                            if s.date == today then
                                                                { s | sets = cached.sets }
                                                            else
                                                                s
                                                        )
                                                        ex.sessions

                                        updatedEx =
                                            { ex
                                                | sessions      = updatedSessions
                                                , defaultConfig =
                                                    if String.isEmpty cached.config then
                                                        ex.defaultConfig
                                                    else
                                                        Just cached.config
                                            }

                                        saveCmd =
                                            Storage.saveExercise
                                                { key   = "progressive_ex_" ++ abbr
                                                , value = E.encode 0 (encodeExercise updatedEx)
                                                }
                                    in
                                    ( Dict.insert abbr updatedEx dbAcc, cmdAcc ++ [ saveCmd ] )

                                _ ->
                                    ( dbAcc, cmdAcc )
                        )
                        ( model.db, [] )
                        setsCache
            in
            ( { model | log = newLog, db = newDb }
            , Cmd.batch
                (cmds
                    ++ [ Storage.deleteDraft { key = "progressive_draft_v1" } ]
                )
            )


-- ── Subscriptions ─────────────────────────────────────────────────────────────

subscriptions : Model -> Sub Msg
subscriptions _ =
    Storage.storageLoaded StorageLoaded


-- ── View ──────────────────────────────────────────────────────────────────────

view : Model -> Browser.Document Msg
view model =
    { title = "progressive/"
    , body =
        [ div [ class "bg-gray-950 text-white min-h-dvh max-w-[480px] mx-auto pb-20 font-sans antialiased" ]
            [ viewNav
            , if not model.loaded then
                viewLoading
              else
                viewScreen model
            , viewBottomNav model.tab
            ]
        ]
    }


viewNav : Html Msg
viewNav =
    nav [ class "sticky top-0 z-10 bg-gray-950 border-b border-gray-800 flex items-center justify-between px-4 h-14" ]
        [ span [ class "font-mono text-base text-lime-400 tracking-tight" ]
            [ text "progressive/" ]
        ]


viewLoading : Html Msg
viewLoading =
    div [ class "p-4 font-mono text-sm text-gray-600" ]
        [ text "Loading…" ]


viewScreen : Model -> Html Msg
viewScreen model =
    case model.tab of
        LogTab ->
            Html.map LogMsg
                (Log.viewLog model.log model.db model.routines)

        HistoryTab ->
            Html.map HistoryMsg
                (History.viewHistory model.history model.db model.routines (Dict.keys model.db))

        ManageTab ->
            Html.map ManageMsg
                (Manage.viewManage model.manage model.db model.routines)


viewBottomNav : Tab -> Html Msg
viewBottomNav current =
    div [ class "fixed bottom-0 left-1/2 -translate-x-1/2 w-full max-w-[480px] bg-gray-950 border-t border-gray-800 flex h-16" ]
        [ navBtn "✚" "Log" LogTab current
        , navBtn "◈" "History" HistoryTab current
        , navBtn "⊞" "Manage" ManageTab current
        ]


navBtn : String -> String -> Tab -> Tab -> Html Msg
navBtn icon label tab current =
    button
        [ class "flex-1 flex flex-col items-center justify-center gap-1 border-none bg-transparent cursor-pointer transition-colors"
        , classList
            [ ( "text-lime-400", tab == current )
            , ( "text-gray-600", tab /= current )
            ]
        , onClick (SwitchTab tab)
        ]
        [ span [ class "text-xl leading-none" ] [ text icon ]
        , span [ class "text-xs" ] [ text label ]
        ]


-- ── Main ──────────────────────────────────────────────────────────────────────

main : Program E.Value Model Msg
main =
    Browser.document
        { init          = init
        , update        = update
        , subscriptions = subscriptions
        , view          = view
        }
