module ReviewConfig exposing (config)

import NoUnused.Parameters
import NoUnused.Variables
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ NoUnused.Variables.rule
    , NoUnused.Parameters.rule
    ]
