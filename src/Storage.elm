port module Storage exposing
    ( deleteDraft
    , loadAll
    , saveDraft
    , saveExercise
    , saveRoutines
    , storageDraftSaved
    , storageLoaded
    , storageRoutinesSaved
    )

import Json.Encode as E


-- ── Outgoing ports (Elm → JS) ─────────────────────────────────────────────────

-- Load all data from window.storage on init
port loadAll : () -> Cmd msg

-- Save a single exercise (keyed by abbr)
port saveExercise : { key : String, value : String } -> Cmd msg

-- Save routines blob
port saveRoutines : { key : String, value : String } -> Cmd msg

-- Save draft
port saveDraft : { key : String, value : String } -> Cmd msg

-- Clear draft
port deleteDraft : { key : String } -> Cmd msg


-- ── Incoming ports (JS → Elm) ─────────────────────────────────────────────────

-- All storage data loaded on init
-- Payload shape: { db: {...}, routines: {...}, draft: {...} | null }
port storageLoaded : (E.Value -> msg) -> Sub msg

-- Acknowledgement that draft was saved (optional, for UI feedback)
port storageDraftSaved : (() -> msg) -> Sub msg

-- Acknowledgement that routines were saved
port storageRoutinesSaved : (() -> msg) -> Sub msg
