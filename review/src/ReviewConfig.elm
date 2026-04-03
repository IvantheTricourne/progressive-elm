module ReviewConfig exposing (config)

import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Parameters
import NoUnused.Variables
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ NoUnused.Variables.rule
    , NoUnused.Parameters.rule
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    ]
