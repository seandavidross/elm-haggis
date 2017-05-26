module Haggis.Combination exposing (..)

import Haggis.Card exposing (..)
import List.Extra exposing (..)


type Combination
    = Set
    | Sequence
    | Bomb


type Set
    = Single
    | Double
    | Triple
    | Quadruple
    | Quintuple
    | Sextuple
    | Septuple
    | Octuple


type Sequence
    = SingleRun
    | DoubleRun
    | TripleRun
    | QuadrupleRun
    | QuintupleRun
    | SextupleRun
    | SeptupleRun
    | OctupleRun


type Bomb
    = Rainbow
    | JQ
    | JK
    | QK
    | JQK
    | Suited


set : Cards -> Maybe Set
set cards =
    let
        ( spotCards, wildCards ) =
            List.partition (isSpotCard) cards

        numberOfCards =
            List.length cards
    in
        if allSameRank spotCards then
            case numberOfCards of
                1 ->
                    Just Single

                2 ->
                    Just Double

                3 ->
                    Just Triple

                4 ->
                    Just Quadruple

                5 ->
                    Just Quintuple

                6 ->
                    Just Sextuple

                7 ->
                    Just Septuple

                8 ->
                    Just Octuple

                otherwise ->
                    Nothing
        else if isSoloWildCard wildCards && numberOfCards == 1 then
            Just Single
        else
            Nothing


allSameRank : Cards -> Bool
allSameRank cards =
    case cards of
        [] ->
            False

        first :: rest ->
            List.all (equal first) rest


isSoloWildCard : Cards -> Bool
isSoloWildCard wildCards =
    List.length wildCards == 1


bomb : Cards -> Maybe Bomb
bomb cards =
    case List.sortBy rank cards of
        [ wild, wild' ] ->
            case ( wild.rank, wild'.rank ) of
                ( Jack, Queen ) ->
                    Just JQ

                ( Jack, King ) ->
                    Just JK

                ( Queen, King ) ->
                    Just QK

                otherwise ->
                    Nothing

        [ j, q, k ] ->
            case ( j.rank, q.rank, k.rank ) of
                ( Jack, Queen, King ) ->
                    Just JQK

                otherwise ->
                    Nothing

        [ three, five, seven, nine ] ->
            case List.map .rank [ three, five, seven, nine ] of
                [ Three, Five, Seven, Nine ] ->
                    if allSameSuit cards then
                        Just Suited
                    else if hasFourSuits cards then
                        Just Rainbow
                    else
                        Nothing

                otherwise ->
                    Nothing

        otherwise ->
            Nothing


allSameSuit : Cards -> Bool
allSameSuit cards =
    case cards of
        [] ->
            False

        first :: rest ->
            List.all (hasSameSuit first) rest


hasSameSuit : Card -> Card -> Bool
hasSameSuit card card' =
    card.suit == card'.suit


hasFourSuits : Cards -> Bool
hasFourSuits cards =
    let
        numberOfSuits =
            cards
                |> List.map suit
                |> dropDuplicates
                |> List.length
    in
        numberOfSuits == 4



{-
   dropDuplicates is based on code from elm-community/elm-list-extras
-}


dropDuplicates : List Suit -> List Suit
dropDuplicates suits =
    dropDuplicates' [] suits


dropDuplicates' : List Suit -> List Suit -> List Suit
dropDuplicates' existing remaining =
    case remaining of
        [] ->
            []

        first :: rest ->
            if List.member first existing then
                dropDuplicates' existing rest
            else
                first :: dropDuplicates' (first :: existing) rest


sequence : Cards -> Maybe Sequence
sequence cards =
    let
        sets =
            List.Extra.groupWhileTransitively (equal) cards
    in
        if List.length cards >= 3 && canMakeSequence sets then
            makeSequence sets
        else
            Nothing


canMakeSequence : List (List Card) -> Bool
canMakeSequence sets =
    allSetsSameSize sets && allSetsSameSuits sets && allSetsConsecutive sets


allSetsSameSize : List (List Card) -> Bool
allSetsSameSize sets =
    case sets of
        first :: rest ->
            let
                sizeOfSets =
                    List.length first
            in
                List.all (\s -> List.length s == sizeOfSets) rest

        otherwise ->
            False


allSetsSameSuits : List (List Card) -> Bool
allSetsSameSuits sets =
    let
        suitGroups =
            List.map (\s -> List.map .suit s) sets
    in
        case suitGroups of
            first :: rest ->
                List.all ((==) first) rest

            otherwise ->
                False


allSetsConsecutive : List (List Card) -> Bool
allSetsConsecutive sets =
    let
        ranks =
            List.map (\s -> List.head s) sets
    in
        allRanksConsecutive ranks


allRanksConsecutive : List (Maybe Card) -> Bool
allRanksConsecutive cards =
    case cards of
        [] ->
            False

        c :: [] ->
            case c of
                Just c ->
                    True

                Nothing ->
                    False

        c :: c' :: rest ->
            case ( c, c' ) of
                ( Just c, Just c' ) ->
                    rank c < (rank c' + 1) && allRanksConsecutive ((Maybe.map identity (Just c')) :: rest)

                otherwise ->
                    False


makeSequence : List Cards -> Maybe Sequence
makeSequence sets =
    case sets of
        set :: _ ->
            case sizeOfSet set of
                1 ->
                    Just SingleRun

                2 ->
                    Just DoubleRun

                3 ->
                    Just TripleRun

                4 ->
                    Just QuadrupleRun

                5 ->
                    Just QuintupleRun

                6 ->
                    Just SextupleRun

                7 ->
                    Just SeptupleRun

                8 ->
                    Just OctupleRun

                otherwise ->
                    Nothing

        otherwise ->
            Nothing


sizeOfSet : List Card -> Int
sizeOfSet set =
    List.length set
