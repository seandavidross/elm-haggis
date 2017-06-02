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
import List exposing (..)
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


type alias ListOfSets =
    List (List Card)


type Sequence
    = RunOfSingles
    | RunOfPairs
    | RunOfTriples
    | RunOfFourOfAKinds
    | RunOfFiveOfAKinds
    | RunOfSixOfAKinds


type Bomb
    = Rainbow
    | JQ
    | JK
    | QK
    | JQK
    | Suited



-- SET


set : List Card -> Maybe Set
set cards =
    let
        ( spotcards, wildcards ) =
            partition (isSpotCard) cards
    in
        if allSameRank spotcards then
            makeSet cards
        else if length wildcards == 1 && length cards == 1 then
            Just Single
        else
            Nothing


allSameRank : List Card -> Bool
allSameRank cards =
    case cards of
        [] ->
            False

        first :: rest ->
            all (equal first) rest


makeSet : List Card -> Maybe Set
makeSet cards =
    case length cards of
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

        otherwise ->
            Nothing



-- BOMB


bomb : List Card -> Maybe Bomb
bomb cards =
    let
        ranks =
            cards |> sortBy (.order) |> map .rank
    in
        case ranks of
            [ Jack, Queen ] ->
                Just JQ

            [ Jack, King ] ->
                Just JK

            [ Queen, King ] ->
                Just QK

            [ Jack, Queen, King ] ->
                Just JQK

            [ Three, Five, Seven, Nine ] ->
                if allSameSuit cards then
                    Just Suited
                else if hasFourSuits cards then
                    Just Rainbow
                else
                    Nothing

            otherwise ->
                Nothing


allSameSuit : List Card -> Bool
allSameSuit cards =
    case cards of
        first :: rest ->
            all (hasSameSuit first) rest

        otherwise ->
            False


hasSameSuit : Card -> Card -> Bool
hasSameSuit card card' =
    card.suit == card'.suit


hasFourSuits : List Card -> Bool
hasFourSuits cards =
    (countSuits cards) == 4


countSuits : List Card -> Int
countSuits cards =
    cards
        |> map .suit
        |> dropDuplicates
        |> length


{-| dropDuplicates is based on code from elm-community/elm-list-extras
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


sequence : List Card -> List (Maybe Sequence)
sequence cards =
    let
        ( spotcards, wildcards ) =
            partition (isSpotCard) cards

        numberOfCards =
            length cards

        numberOfWilds =
            length wildcards

        naturalSuitCount =
            countSuits spotcards

        numberOfSuits =
            countSuits cards

        lowestRank =
            findLowestRank spotcards

        widths =
            range naturalSuitCount numberOfSuits

        maybeSequenceOfWidth sequenceWidth =
            let
                sequenceLength =
                    floor (toFloat (numberOfCards) / toFloat (sequenceWidth))

                sequenceRank =
                    lowestRank + sequenceLength - 1

                hasEnoughCardsForSequenceWidth =
                    (sequenceWidth == 1 && numberOfCards >= 3)
                        || (sequenceWidth > 1 && numberOfCards >= sequenceWidth * 2)

                canFormSequenceUpToRank highestRank =
                    let
                        ranks =
                            range lowestRank highestRank

                        cardsWithTheseRanks =
                            map (\rank -> (filter (\c -> c.order == rank) cards)) ranks

                        wildsForTheseRanks =
                            map (\set -> sequenceWidth - (length set)) cardsWithTheseRanks

                        wildsUsedAsNaturals =
                            length (filter (\w -> member w.order ranks) wildcards)

                        wildsNeeded =
                            (sum wildsForTheseRanks)
                    in
                        wildsNeeded == (numberOfWilds - wildsUsedAsNaturals)

                makeSequenceWithWidth w =
                    case w of
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

                        otherwise ->
                            Nothing
            in
                if
                    hasEnoughCardsForSequenceWidth
                        && (numberOfCards == (sequenceLength * sequenceWidth))
                        && canFormSequenceUpToRank sequenceRank
                then
                    makeSequenceWithWidth sequenceWidth
                else
                    Nothing
    in
        if length spotcards == 0 then
            [ Nothing ]
        else
            widths
                |> map maybeSequenceOfWidth
                |> stripNothings


findLowestRank : List Card -> Ordinal
findLowestRank cards =
    let
        two =
            2
    in
        cards |> map .order |> minimum |> Maybe.withDefault two


stripNothings : List (Maybe Sequence) -> List (Maybe Sequence)
stripNothings sequences =
    let
        somethings =
            filter (isNotNothing) sequences
    in
        case somethings of
            [] ->
                [ Nothing ]

            otherwise ->
                somethings


isNotNothing : Maybe Sequence -> Bool
isNotNothing s =
    case s of
        Nothing ->
            False

        otherwise ->
            True


{-| It seems the version of Elm I'm using doesn't expose List.range...??
-}
range : Int -> Int -> List Int
range lo hi =
    rangeHelp lo hi []


rangeHelp : Int -> Int -> List Int -> List Int
rangeHelp lo hi list =
    if lo <= hi then
        rangeHelp lo (hi - 1) (hi :: list)
    else
        list
