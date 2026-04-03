module Decode exposing
    ( decodeDb
    , decodeDraft
    , decodeRoutines
    )

import Json.Decode as D exposing (Decoder)
import Model exposing (..)



-- ── Set ───────────────────────────────────────────────────────────────────────


setDecoder : Decoder Set
setDecoder =
    D.map4 Set
        (D.field "weight" (D.nullable D.float))
        (D.field "reps" (D.nullable D.int))
        (D.field "outcome" D.string |> D.map outcomeFromString)
        (D.maybe (D.field "note" D.string))



-- ── Session ───────────────────────────────────────────────────────────────────


sessionDecoder : Decoder Session
sessionDecoder =
    D.map2 Session
        (D.field "date" D.string)
        (D.field "sets" (D.list setDecoder))



-- ── Exercise ──────────────────────────────────────────────────────────────────


exerciseDecoder : Decoder Exercise
exerciseDecoder =
    D.map4 Exercise
        (D.field "abbr" D.string)
        (D.field "fullName" D.string)
        (D.field "sessions" (D.list sessionDecoder))
        (D.maybe (D.field "defaultConfig" D.string))



-- ── Db ────────────────────────────────────────────────────────────────────────


decodeDb : Decoder Db
decodeDb =
    D.dict exerciseDecoder



-- ── Routine ───────────────────────────────────────────────────────────────────


routineDecoder : Decoder Routine
routineDecoder =
    D.map3 Routine
        (D.field "name" D.string)
        (D.field "exercises" (D.list D.string))
        (D.field "archived" D.bool)


decodeRoutines : Decoder Routines
decodeRoutines =
    D.dict routineDecoder



-- ── Draft ─────────────────────────────────────────────────────────────────────


cachedExerciseDecoder : Decoder CachedExercise
cachedExerciseDecoder =
    D.map2 CachedExercise
        (D.field "sets" (D.list setDecoder))
        (D.field "config" D.string)


decodeDraft : Decoder Draft
decodeDraft =
    D.map5 Draft
        (D.field "routine" D.string)
        (D.field "sessionQueue" (D.list D.string))
        (D.field "sessionIdx" D.int)
        (D.field "setsCache" (D.dict cachedExerciseDecoder))
        (D.field "savedAt" D.string)



-- ── CachedExercise needs to be exposed from Model ────────────────────────────


type alias CachedExercise =
    { sets : List Set
    , config : String
    }
