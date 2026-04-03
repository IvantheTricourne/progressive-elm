module Component.Badge exposing
    ( Badge
    , badgeClass
    , badgeLabel
    , computeBadges
    , topBadge
    )

import Model exposing (..)


type Badge
    = PB
    | Clean
    | VolumeUp
    | Stall


badgeLabel : Badge -> String
badgeLabel b =
    case b of
        PB ->
            "PB"

        Clean ->
            "Clean"

        VolumeUp ->
            "Vol ↑"

        Stall ->
            "Stall"


badgeClass : Badge -> String
badgeClass b =
    case b of
        PB ->
            "badge-pb"

        Clean ->
            "badge-clean"

        VolumeUp ->
            "badge-volume"

        Stall ->
            "badge-stall"



-- ── Badge computation ─────────────────────────────────────────────────────────


workingSets : List Set -> List Set
workingSets sets =
    List.filter
        (\s -> s.outcome /= Warmup && s.outcome /= TooLight && s.weight /= Nothing)
        sets


maxWeight : List Set -> Float
maxWeight sets =
    workingSets sets
        |> List.filterMap .weight
        |> List.foldl max 0


totalVolume : List Set -> Float
totalVolume sets =
    workingSets sets
        |> List.foldl
            (\s acc ->
                acc
                    + (Maybe.withDefault 0 s.weight
                        * toFloat (Maybe.withDefault 0 s.reps)
                      )
            )
            0


computeBadges : Session -> List Session -> List Badge
computeBadges sess allSessions =
    let
        sessIdx =
            allSessions
                |> List.indexedMap Tuple.pair
                |> List.filter (\( _, s ) -> s.date == sess.date)
                |> List.head
                |> Maybe.map Tuple.first
                |> Maybe.withDefault 0

        prevSessions =
            List.take sessIdx allSessions

        currentMax =
            maxWeight sess.sets

        historicMax =
            prevSessions
                |> List.map (\s -> maxWeight s.sets)
                |> List.foldl max 0

        pbBadge =
            if currentMax > 0 && currentMax > historicMax then
                [ PB ]

            else
                []

        hasFailOrPartial =
            workingSets sess.sets
                |> List.any (\s -> s.outcome == AlmostFailed || s.outcome == Partial)

        cleanBadge =
            if not hasFailOrPartial && not (List.isEmpty (workingSets sess.sets)) then
                [ Clean ]

            else
                []

        prevSess =
            if sessIdx > 0 then
                List.head (List.drop (sessIdx - 1) allSessions)

            else
                Nothing

        volBadge =
            case prevSess of
                Just prev ->
                    if totalVolume sess.sets > totalVolume prev.sets then
                        [ VolumeUp ]

                    else
                        []

                Nothing ->
                    []

        stallBadge =
            if sessIdx >= 2 then
                let
                    last3 =
                        List.drop (sessIdx - 2) allSessions |> List.take 3

                    weights =
                        List.map (\s -> maxWeight s.sets) last3
                in
                case weights of
                    w :: rest ->
                        if w > 0 && List.all (\x -> x == w) rest then
                            [ Stall ]

                        else
                            []

                    _ ->
                        []

            else
                []
    in
    pbBadge ++ cleanBadge ++ volBadge ++ stallBadge


topBadge : List Badge -> Maybe Badge
topBadge badges =
    let
        priority =
            [ PB, Stall, VolumeUp, Clean ]
    in
    priority
        |> List.filter (\b -> List.member b badges)
        |> List.head
