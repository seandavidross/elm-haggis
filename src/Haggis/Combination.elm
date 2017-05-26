module Haggis.Combination
    exposing
        ( Combination(..)
        , Set(..)
        , Sequence(..)
        , Bomb(..)
        , set
        , sequence
        , bomb
        )

import Haggis.Card exposing (..)
import List.Extra exposing (..)


type Combination
    = Set
    | Sequence
    | Bomb


type Set
    = Single
    | Pair
    | Triple
    | FourOfAKind
    | FiveOfAKind
    | SixOfAKind
    | SevenOfAKind
    | EightOfAKind


type Sequence
    = RunOfSingles
    | RunOfPairs
    | RunOfTriples
    | RunOfFourOfAKinds
    | RunOfFiveOfAKinds
    | RunOfSixOfAKinds
    | RunOfSevenOfAKinds
    | RunOfEightOfAKinds


type Bomb
    = Rainbow
    | JQ
    | JK
    | QK
    | JQK
    | Suited



-- SET


set : Cards -> Maybe Set
set cards =
    let
        ( spotCards, wildCards ) =
            List.partition (isSpotCard) cards
    in
        if allSameRank spotCards then
            makeSet cards
        else if count wildCards == 1 && count cards == 1 then
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


makeSet : Cards -> Maybe Set
makeSet cards =
    case count cards of
        1 ->
            Just Single

        2 ->
            Just Pair

        3 ->
            Just Triple

        4 ->
            Just FourOfAKind

        5 ->
            Just FiveOfAKind

        6 ->
            Just SixOfAKind

        7 ->
            Just SevenOfAKind

        8 ->
            Just EightOfAKind

        otherwise ->
            Nothing


count : Cards -> Int
count cards =
    List.length cards


size : Cards -> Int
size =
    count



-- BOMB


bomb : Cards -> Maybe Bomb
bomb cards =
    case sorted cards of
        [ wild, wild' ] ->
            case collectRanks [ wild, wild' ] of
                [ Jack, Queen ] ->
                    Just JQ

                [ Jack, King ] ->
                    Just JK

                [ Queen, King ] ->
                    Just QK

                otherwise ->
                    Nothing

        [ j, q, k ] ->
            case collectRanks [ j, q, k ] of
                [ Jack, Queen, King ] ->
                    Just JQK

                otherwise ->
                    Nothing

        [ three, five, seven, nine ] ->
            case collectRanks [ three, five, seven, nine ] of
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


sorted : Cards -> Cards
sorted cards =
    List.sortBy rank cards


collectRanks : Cards -> List Rank
collectRanks cards =
    List.map .rank cards


allSameSuit : Cards -> Bool
allSameSuit cards =
    case cards of
        first :: rest ->
            List.all (hasSameSuit first) rest

        otherwise ->
            False


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



-- SEQUENCE
-- TODO still need to handle Wild cards...


sequence : Cards -> Maybe Sequence
sequence cards =
    let
        sets =
            collectSets cards
    in
        if count cards >= 3 && canFormSequence sets then
            makeSequence sets
        else
            Nothing


collectSets : List Card -> List (List Card)
collectSets cards =
    List.Extra.groupWhile (equal) (sorted cards)


canFormSequence : List (List Card) -> Bool
canFormSequence sets =
    allSetsSameSize sets
        && allSetsSameSuits sets
        && allSetsConsecutive sets


allSetsSameSize : List (List Card) -> Bool
allSetsSameSize sets =
    case sets of
        first :: rest ->
            List.all (\s -> size s == size first) rest

        otherwise ->
            False


allSetsSameSuits : List (List Card) -> Bool
allSetsSameSuits sets =
    case collectSuitGroups sets of
        suitGroup :: rest ->
            List.all ((==) suitGroup) rest

        otherwise ->
            False


collectSuitGroups : List Cards -> List (List Suit)
collectSuitGroups sets =
    List.map (collectSuits) sets


collectSuits : Cards -> List Suit
collectSuits set =
    set
        |> List.map .suit
        |> sortSuits


sortSuits : List Suit -> List Suit
sortSuits suits =
    List.sortWith (compareSuits) suits


allSetsConsecutive : List (List Card) -> Bool
allSetsConsecutive sets =
    allRanksConsecutive (collectOneofEachRank sets)


collectOneofEachRank : List (List Card) -> List (Maybe Card)
collectOneofEachRank sets =
    List.map (\s -> List.head s) sets


allRanksConsecutive : List (Maybe Card) -> Bool
allRanksConsecutive cards =
    case cards of
        c :: [] ->
            case c of
                Just c ->
                    True

                Nothing ->
                    False

        c :: c' :: rest ->
            case ( c, c' ) of
                ( Just c, Just c' ) ->
                    (rank c < (rank c' + 1))
                        && allRanksConsecutive (maybe c' :: rest)

                otherwise ->
                    False

        otherwise ->
            False


maybe : Card -> Maybe Card
maybe card =
    Maybe.map identity (Just card)


makeSequence : List Cards -> Maybe Sequence
makeSequence sets =
    case sets of
        set :: _ ->
            case size set of
                1 ->
                    Just RunOfSingles

                2 ->
                    Just RunOfPairs

                3 ->
                    Just RunOfTriples

                4 ->
                    Just RunOfFourOfAKinds

                5 ->
                    Just RunOfFiveOfAKinds

                6 ->
                    Just RunOfSixOfAKinds

                7 ->
                    Just RunOfSevenOfAKinds

                8 ->
                    Just RunOfEightOfAKinds

                otherwise ->
                    Nothing

        otherwise ->
            Nothing
