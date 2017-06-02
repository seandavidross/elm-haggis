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

        sequenceWidths =
            range (countSuits spotcards) (countSuits cards)
    in
        if length spotcards == 0 then
            [ Nothing ]
        else
            sequenceWidths
                |> map (maybeSequenceOfWidth cards)
                |> keepJustSequences


maybeSequenceOfWidth : List Card -> Int -> Maybe Sequence
maybeSequenceOfWidth cards sequenceWidth =
    let
        ( spotcards, wildcards ) =
            partition (isSpotCard) cards

        numberOfCards =
            length cards

        lowRank =
            findLowestRank spotcards

        sequenceLength =
            numberOfCards // sequenceWidth

        highRank =
            lowRank + sequenceLength - 1
    in
        if
            hasEnoughCardsForSequenceWidth sequenceWidth numberOfCards
                && (numberOfCards == (sequenceLength * sequenceWidth))
                && canFormSequence lowRank highRank sequenceWidth cards
        then
            makeSequenceOfWidth sequenceWidth
        else
            Nothing


findLowestRank : List Card -> Ordinal
findLowestRank cards =
    cards |> map .order |> minimum |> Maybe.withDefault 2


hasEnoughCardsForSequenceWidth : Int -> Int -> Bool
hasEnoughCardsForSequenceWidth sequenceWidth numberOfCards =
    (sequenceWidth == 1 && numberOfCards >= 3)
        || (sequenceWidth > 1 && numberOfCards >= sequenceWidth * 2)


canFormSequence : Int -> Int -> Int -> List Card -> Bool
canFormSequence lowRank highRank sequenceWidth cards =
    let
        ( spotcards, wildcards ) =
            partition (isSpotCard) cards

        ranks =
            range lowRank highRank

        wildsNeedToCompleteSets =
            wildsNeeded sequenceWidth (collectCardsWithRanks ranks cards)

        wildsUsedAsNaturals =
            length (filter (\w -> member w.order ranks) wildcards)
    in
        wildsNeedToCompleteSets == ((length wildcards) - wildsUsedAsNaturals)


wildsNeeded : Int -> ListOfSets -> Int
wildsNeeded sizeOfSets sets =
    sum (map (\set -> sizeOfSets - (length set)) sets)


collectCardsWithRanks : List Int -> List Card -> ListOfSets
collectCardsWithRanks ranks cards =
    map (\rank -> (filter (\c -> c.order == rank) cards)) ranks


makeSequenceOfWidth : Int -> Maybe Sequence
makeSequenceOfWidth width' =
    case width' of
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


keepJustSequences : List (Maybe Sequence) -> List (Maybe Sequence)
keepJustSequences sequences =
    let
        justSequences =
            filter (isSequence) sequences
    in
        case justSequences of
            [] ->
                [ Nothing ]

            otherwise ->
                justSequences


isSequence : Maybe Sequence -> Bool
isSequence s =
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
