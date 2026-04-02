module LogTests exposing (..)

import Dict
import Expect
import Model exposing (..)
import Page.Log exposing (..)
import Test exposing (..)



-- ── Fixtures ──────────────────────────────────────────────────────────────────


emptySet : Set
emptySet =
    { weight = Nothing, reps = Nothing, outcome = Clean, note = Nothing }


set100x5 : Set
set100x5 =
    { weight = Just 100, reps = Just 5, outcome = Clean, note = Nothing }


set50xNothing : Set
set50xNothing =
    { weight = Just 50, reps = Nothing, outcome = Clean, note = Nothing }


testDb : Db
testDb =
    Dict.fromList
        [ ( "BP", { abbr = "BP", fullName = "Bench Press", sessions = [], defaultConfig = Just "outer grip" } )
        , ( "R", { abbr = "R", fullName = "Row", sessions = [], defaultConfig = Just "seat +3" } )
        ]


testRoutines : Routines
testRoutines =
    Dict.fromList
        [ ( "A", { name = "Routine A", exercises = [ "BP", "R" ], archived = False } ) ]


loggingModel : LogModel
loggingModel =
    { step = LoggingSets
    , selectedRoutine = Just "A"
    , sessionQueue = [ "BP", "R" ]
    , sessionIdx = 0
    , currentSets = [ emptySet ]
    , setsCache = Dict.empty
    , configValue = ""
    , draft = Nothing
    }


draftA : Draft
draftA =
    { routine = "A"
    , sessionQueue = [ "BP", "R" ]
    , sessionIdx = 0
    , setsCache = Dict.empty
    , savedAt = ""
    }



-- ── Tests ─────────────────────────────────────────────────────────────────────


suite : Test
suite =
    describe "Page.Log"
        [ describe "cacheCurrentState"
            [ test "strips sets where both weight and reps are Nothing" <|
                \_ ->
                    let
                        model =
                            { loggingModel | currentSets = [ emptySet, set100x5 ] }
                    in
                    cacheCurrentState model
                        |> .sets
                        |> Expect.equal [ set100x5 ]
            , test "keeps sets where only weight is set" <|
                \_ ->
                    let
                        model =
                            { loggingModel | currentSets = [ set50xNothing ] }
                    in
                    cacheCurrentState model
                        |> .sets
                        |> Expect.equal [ set50xNothing ]
            , test "returns empty sets list when all sets are blank" <|
                \_ ->
                    let
                        model =
                            { loggingModel | currentSets = [ emptySet, emptySet ] }
                    in
                    cacheCurrentState model
                        |> .sets
                        |> Expect.equal []
            , test "captures configValue" <|
                \_ ->
                    let
                        model =
                            { loggingModel | configValue = "outer grip" }
                    in
                    cacheCurrentState model
                        |> .config
                        |> Expect.equal "outer grip"
            ]
        , describe "restoreSets"
            [ test "returns [emptySet] when abbr is not in cache" <|
                \_ ->
                    restoreSets 0 Dict.empty [ "BP" ]
                        |> Expect.equal [ emptySet ]
            , test "returns [emptySet] when cached sets list is empty" <|
                \_ ->
                    let
                        cache =
                            Dict.fromList [ ( "BP", { sets = [], config = "" } ) ]
                    in
                    restoreSets 0 cache [ "BP" ]
                        |> Expect.equal [ emptySet ]
            , test "returns stored sets when cache has non-empty sets" <|
                \_ ->
                    let
                        cache =
                            Dict.fromList [ ( "BP", { sets = [ set100x5 ], config = "" } ) ]
                    in
                    restoreSets 0 cache [ "BP" ]
                        |> Expect.equal [ set100x5 ]
            , test "uses idx to pick correct abbr from queue" <|
                \_ ->
                    let
                        cache =
                            Dict.fromList [ ( "R", { sets = [ set100x5 ], config = "" } ) ]
                    in
                    restoreSets 1 cache [ "BP", "R" ]
                        |> Expect.equal [ set100x5 ]
            , test "returns [emptySet] when idx is out of range" <|
                \_ ->
                    restoreSets 5 Dict.empty [ "BP" ]
                        |> Expect.equal [ emptySet ]
            ]
        , describe "updateLog — ExitSession"
            [ test "returns SaveDraftEffect" <|
                \_ ->
                    let
                        ( _, effect ) =
                            updateLog ExitSession loggingModel testDb testRoutines
                    in
                    case effect of
                        Just (SaveDraftEffect _) ->
                            Expect.pass

                        _ ->
                            Expect.fail "Expected SaveDraftEffect"
            , test "transitions step back to PickRoutine" <|
                \_ ->
                    let
                        ( newModel, _ ) =
                            updateLog ExitSession loggingModel testDb testRoutines
                    in
                    newModel.step |> Expect.equal PickRoutine
            , test "draft in SaveDraftEffect contains current sessionQueue" <|
                \_ ->
                    let
                        ( _, effect ) =
                            updateLog ExitSession loggingModel testDb testRoutines
                    in
                    case effect of
                        Just (SaveDraftEffect draft) ->
                            draft.sessionQueue |> Expect.equal [ "BP", "R" ]

                        _ ->
                            Expect.fail "Expected SaveDraftEffect"
            ]
        , describe "updateLog — DiscardDraft"
            [ test "returns ClearDraftEffect" <|
                \_ ->
                    let
                        model =
                            { loggingModel | draft = Just draftA }

                        ( _, effect ) =
                            updateLog DiscardDraft model testDb testRoutines
                    in
                    effect |> Expect.equal (Just ClearDraftEffect)
            , test "clears draft from model" <|
                \_ ->
                    let
                        model =
                            { loggingModel | draft = Just draftA }

                        ( newModel, _ ) =
                            updateLog DiscardDraft model testDb testRoutines
                    in
                    newModel.draft |> Expect.equal Nothing
            ]
        , describe "updateLog — SaveCurrentExercise"
            [ test "returns Nothing when there are no valid sets" <|
                \_ ->
                    let
                        ( _, effect ) =
                            updateLog SaveCurrentExercise loggingModel testDb testRoutines
                    in
                    effect |> Expect.equal Nothing
            , test "does not mutate model when there are no valid sets" <|
                \_ ->
                    let
                        ( newModel, _ ) =
                            updateLog SaveCurrentExercise loggingModel testDb testRoutines
                    in
                    newModel.setsCache |> Expect.equal Dict.empty
            , test "returns FlushDraftEffect when valid sets exist" <|
                \_ ->
                    let
                        model =
                            { loggingModel | currentSets = [ set100x5 ] }

                        ( _, effect ) =
                            updateLog SaveCurrentExercise model testDb testRoutines
                    in
                    case effect of
                        Just (FlushDraftEffect _) ->
                            Expect.pass

                        _ ->
                            Expect.fail "Expected FlushDraftEffect"
            , test "updates setsCache with the valid sets" <|
                \_ ->
                    let
                        model =
                            { loggingModel | currentSets = [ set100x5 ] }

                        ( newModel, _ ) =
                            updateLog SaveCurrentExercise model testDb testRoutines
                    in
                    Dict.get "BP" newModel.setsCache
                        |> Maybe.map .sets
                        |> Expect.equal (Just [ set100x5 ])
            , test "draft in FlushDraftEffect contains the updated cache (not stale)" <|
                \_ ->
                    let
                        model =
                            { loggingModel | currentSets = [ set100x5 ] }

                        ( _, effect ) =
                            updateLog SaveCurrentExercise model testDb testRoutines
                    in
                    case effect of
                        Just (FlushDraftEffect draft) ->
                            Dict.get "BP" draft.setsCache
                                |> Maybe.map .sets
                                |> Expect.equal (Just [ set100x5 ])

                        _ ->
                            Expect.fail "Expected FlushDraftEffect"
            ]
        , describe "updateLog — ConfirmFinish"
            [ test "returns CommitSessionEffect carrying the setsCache" <|
                \_ ->
                    let
                        cachedBP =
                            { sets = [ set100x5 ], config = "" }

                        model =
                            { loggingModel | setsCache = Dict.fromList [ ( "BP", cachedBP ) ] }

                        ( _, effect ) =
                            updateLog ConfirmFinish model testDb testRoutines
                    in
                    case effect of
                        Just (CommitSessionEffect cache) ->
                            Dict.get "BP" cache
                                |> Maybe.map .sets
                                |> Expect.equal (Just [ set100x5 ])

                        _ ->
                            Expect.fail "Expected CommitSessionEffect"
            , test "transitions step back to PickRoutine" <|
                \_ ->
                    let
                        ( newModel, _ ) =
                            updateLog ConfirmFinish loggingModel testDb testRoutines
                    in
                    newModel.step |> Expect.equal PickRoutine
            , test "clears setsCache from model after commit" <|
                \_ ->
                    let
                        model =
                            { loggingModel
                                | setsCache = Dict.fromList [ ( "BP", { sets = [ set100x5 ], config = "" } ) ]
                            }

                        ( newModel, _ ) =
                            updateLog ConfirmFinish model testDb testRoutines
                    in
                    newModel.setsCache |> Expect.equal Dict.empty
            ]
        , describe "updateLog — NavExercise"
            [ test "saves current exercise sets to cache before navigating" <|
                \_ ->
                    let
                        model =
                            { loggingModel | currentSets = [ set100x5 ] }

                        ( newModel, _ ) =
                            updateLog (NavExercise 1) model testDb testRoutines
                    in
                    Dict.get "BP" newModel.setsCache
                        |> Maybe.map .sets
                        |> Expect.equal (Just [ set100x5 ])
            , test "restores [emptySet] when target exercise has no cached sets" <|
                \_ ->
                    let
                        ( newModel, _ ) =
                            updateLog (NavExercise 1) loggingModel testDb testRoutines
                    in
                    newModel.currentSets |> Expect.equal [ emptySet ]
            , test "restores cached sets for target exercise" <|
                \_ ->
                    let
                        cachedR =
                            { sets = [ set100x5 ], config = "" }

                        model =
                            { loggingModel | setsCache = Dict.fromList [ ( "R", cachedR ) ] }

                        ( newModel, _ ) =
                            updateLog (NavExercise 1) model testDb testRoutines
                    in
                    newModel.currentSets |> Expect.equal [ set100x5 ]
            , test "does not prematurely append an empty set when restoring non-empty cache" <|
                \_ ->
                    let
                        cachedR =
                            { sets = [ set100x5 ], config = "" }

                        model =
                            { loggingModel | setsCache = Dict.fromList [ ( "R", cachedR ) ] }

                        ( newModel, _ ) =
                            updateLog (NavExercise 1) model testDb testRoutines
                    in
                    List.length newModel.currentSets |> Expect.equal 1
            , test "advances sessionIdx" <|
                \_ ->
                    let
                        ( newModel, _ ) =
                            updateLog (NavExercise 1) loggingModel testDb testRoutines
                    in
                    newModel.sessionIdx |> Expect.equal 1
            ]
        ]
