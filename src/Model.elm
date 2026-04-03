module Model exposing
    ( Db
    , Draft
    , Exercise
    , HistoryPageMsg(..)
    , HistoryView(..)
    , LogPageMsg(..)
    , ManagePageMsg(..)
    , Msg(..)
    , Outcome(..)
    , Routine
    , Routines
    , Session
    , Set
    , Tab(..)
    , emptyDraft
    , outcomeFromString
    , outcomeToString
    )

import Dict exposing (Dict)
import Json.Decode as D



-- ── Core types ────────────────────────────────────────────────────────────────


type Outcome
    = Clean
    | Warmup
    | TooLight
    | AlmostFailed
    | Partial


outcomeToString : Outcome -> String
outcomeToString o =
    case o of
        Clean ->
            ""

        Warmup ->
            "+"

        TooLight ->
            "++"

        AlmostFailed ->
            "-"

        Partial ->
            "/"


outcomeFromString : String -> Outcome
outcomeFromString s =
    case s of
        "+" ->
            Warmup

        "++" ->
            TooLight

        "-" ->
            AlmostFailed

        "/" ->
            Partial

        _ ->
            Clean


type alias Set =
    { weight : Maybe Float
    , reps : Maybe Int
    , outcome : Outcome
    , note : Maybe String
    }


type alias Session =
    { date : String -- YYYY-MM-DD
    , sets : List Set
    }


type alias Exercise =
    { abbr : String
    , fullName : String
    , sessions : List Session
    , defaultConfig : Maybe String
    }


type alias Db =
    Dict String Exercise



-- keyed by abbr


type alias Routine =
    { name : String
    , exercises : List String -- abbr list
    , archived : Bool
    }


type alias Routines =
    Dict String Routine



-- keyed by routine key
-- ── Draft ─────────────────────────────────────────────────────────────────────


type alias CachedExercise =
    { sets : List Set
    , config : String
    }


type alias Draft =
    { routine : String
    , sessionQueue : List String
    , sessionIdx : Int
    , setsCache : Dict String CachedExercise
    , savedAt : String
    }


emptyDraft : Draft
emptyDraft =
    { routine = ""
    , sessionQueue = []
    , sessionIdx = 0
    , setsCache = Dict.empty
    , savedAt = ""
    }



-- ── Navigation ────────────────────────────────────────────────────────────────


type Tab
    = LogTab
    | HistoryTab
    | ManageTab



-- ── Top-level Msg ─────────────────────────────────────────────────────────────


type Msg
    = -- Storage responses
      StorageLoaded D.Value
    | StorageDraftSaved
    | StorageRoutinesSaved
      -- Tab
    | SwitchTab Tab
      -- Delegated page messages (opaque, handled per-page)
    | LogMsg LogPageMsg
    | HistoryMsg HistoryPageMsg
    | ManageMsg ManagePageMsg
    | NoOp


type LogPageMsg
    = SelectRoutine String
    | StartSession
    | ResumeSession
    | DiscardDraft
    | ConfirmDiscard
    | ExitSession
    | NavExercise Int
    | UpdateWeight Int String
    | UpdateReps Int String
    | UpdateOutcome Int String
    | UpdateConfig String
    | AddSet
    | RemoveSet Int
    | SaveCurrentExercise
    | SaveAndNext
    | ShowFinishConfirm
    | ConfirmFinish
    | CancelFinish


type HistoryPageMsg
    = SetHistoryView HistoryView
    | SelectExercise String
    | SelectRoutineSummary String
    | CalendarNav Int
    | SelectCalDay String


type HistoryView
    = ByExercise
    | ByRoutine
    | CalendarView


type ManagePageMsg
    = ShowNewRoutine
    | EditRoutine String
    | CancelRoutineEdit
    | UpdateRoutineName String
    | ToggleRoutineExercise String
    | SaveRoutineEdit
    | CopyRoutine String
    | ToggleArchiveRoutine String
    | ShowNewExercise
    | CancelExerciseEdit
    | UpdateExerciseAbbr String
    | UpdateExerciseFullName String
    | SaveExerciseEdit
    | CopyExport
