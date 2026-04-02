module Encode exposing
    ( encodeDb
    , encodeDraft
    , encodeExercise
    , encodeRoutines
    )

import Dict exposing (Dict)
import Json.Encode as E
import Model exposing (..)


-- ── Set ───────────────────────────────────────────────────────────────────────

encodeSet : Set -> E.Value
encodeSet s =
    E.object
        ([ ( "weight",  Maybe.withDefault E.null (Maybe.map E.float s.weight) )
         , ( "reps",    Maybe.withDefault E.null (Maybe.map E.int s.reps) )
         , ( "outcome", E.string (outcomeToString s.outcome) )
         ]
            ++ (case s.note of
                    Just n  -> [ ( "note", E.string n ) ]
                    Nothing -> []
               )
        )


-- ── Session ───────────────────────────────────────────────────────────────────

encodeSession : Session -> E.Value
encodeSession sess =
    E.object
        [ ( "date", E.string sess.date )
        , ( "sets", E.list encodeSet sess.sets )
        ]


-- ── Exercise ──────────────────────────────────────────────────────────────────

encodeExercise : Exercise -> E.Value
encodeExercise ex =
    E.object
        ([ ( "abbr",     E.string ex.abbr )
         , ( "fullName", E.string ex.fullName )
         , ( "sessions", E.list encodeSession ex.sessions )
         ]
            ++ (case ex.defaultConfig of
                    Just c  -> [ ( "defaultConfig", E.string c ) ]
                    Nothing -> [ ( "defaultConfig", E.null ) ]
               )
        )


-- ── Db ────────────────────────────────────────────────────────────────────────

encodeDb : Db -> E.Value
encodeDb db =
    E.dict identity encodeExercise db


-- ── Routine ───────────────────────────────────────────────────────────────────

encodeRoutine : Routine -> E.Value
encodeRoutine r =
    E.object
        [ ( "name",      E.string r.name )
        , ( "exercises", E.list E.string r.exercises )
        , ( "archived",  E.bool r.archived )
        ]


encodeRoutines : Routines -> E.Value
encodeRoutines routines =
    E.dict identity encodeRoutine routines


-- ── Draft ─────────────────────────────────────────────────────────────────────

encodeCachedExercise : CachedExercise -> E.Value
encodeCachedExercise c =
    E.object
        [ ( "sets",   E.list encodeSet c.sets )
        , ( "config", E.string c.config )
        ]


encodeDraft : Draft -> E.Value
encodeDraft d =
    E.object
        [ ( "routine",      E.string d.routine )
        , ( "sessionQueue", E.list E.string d.sessionQueue )
        , ( "sessionIdx",   E.int d.sessionIdx )
        , ( "setsCache",    E.dict identity encodeCachedExercise d.setsCache )
        , ( "savedAt",      E.string d.savedAt )
        ]


type alias CachedExercise =
    { sets   : List Set
    , config : String
    }
