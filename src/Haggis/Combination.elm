module Haggis.Combination
    exposing
        ( Bomb(..)
        , Combination(..)
        , Sequence(..)
        , Set(..)
        , bomb
        , sequence
        , set
        )

import Haggis.Card exposing (..)
import Haggis.Cards exposing (..)
import List exposing (..)


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


type alias Sets =
    List Cards


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


set : Cards -> Maybe Set
set cards =
    let
        ( spotcards, wildcards ) =
            partition isSpotCard cards
    in
    if allSameRank spotcards then
        makeSet cards
    else if length wildcards == 1 && length cards == 1 then
        Just Single
    else
        Nothing


allSameRank : Cards -> Bool
allSameRank cards =
    case cards of
        [] ->
            False

        first :: rest ->
            all (equal first) rest


makeSet : Cards -> Maybe Set
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


bomb : Cards -> Maybe Bomb
bomb cards =
    let
        ranks =
            cards |> sortBy .order |> map .rank
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


allSameSuit : Cards -> Bool
allSameSuit cards =
    case cards of
        first :: rest ->
            all (hasSameSuit first) rest

        otherwise ->
            False


hasSameSuit : Card -> Card -> Bool
hasSameSuit card card_ =
    card.suit == card_.suit


hasFourSuits : Cards -> Bool
hasFourSuits cards =
    countSuits cards == 4


countSuits : Cards -> Int
countSuits cards =
    cards
        |> map .suit
        |> dropDuplicates
        |> length


{-| dropDuplicates is based on code from elm-community/elm-list-extras
-}
dropDuplicates : List Suit -> List Suit
dropDuplicates suits =
    dropDuplicates_ [] suits


dropDuplicates_ : List Suit -> List Suit -> List Suit
dropDuplicates_ existing remaining =
    case remaining of
        [] ->
            []

        first :: rest ->
            if List.member first existing then
                dropDuplicates_ existing rest
            else
                first :: dropDuplicates_ (first :: existing) rest



-- SEQUENCE


sequence : Cards -> List (Maybe Sequence)
sequence cards =
    let
        ( spotcards, wildcards ) =
            partition isSpotCard cards

        sequenceWidths =
            range (countSuits spotcards) (countSuits cards)
    in
    if length spotcards == 0 then
        [ Nothing ]
    else
        sequenceWidths
            |> map (maybeSequenceOfWidth cards)
            |> keepJustSequences


maybeSequenceOfWidth : Cards -> Int -> Maybe Sequence
maybeSequenceOfWidth cards sequenceWidth =
    let
        ( spotcards, wildcards ) =
            partition isSpotCard cards

        numberOfCards =
            length cards

        lowRank =
            findLowestRank spotcards

        sequenceLength =
            numberOfCards // sequenceWidth

        highRank =
            lowRank + sequenceLength - 1

        ranks =
            range lowRank highRank
    in
    if
        hasEnoughCardsForSequenceWidth sequenceWidth numberOfCards
            && (numberOfCards == (sequenceLength * sequenceWidth))
            && canFormSequence sequenceWidth ranks cards
    then
        makeSequenceOfWidth sequenceWidth
    else
        Nothing


findLowestRank : Cards -> Haggis.Card.Order
findLowestRank cards =
    cards |> map .order |> minimum |> Maybe.withDefault 2


hasEnoughCardsForSequenceWidth : Int -> Int -> Bool
hasEnoughCardsForSequenceWidth sequenceWidth numberOfCards =
    (sequenceWidth == 1 && numberOfCards >= 3)
        || (sequenceWidth > 1 && numberOfCards >= sequenceWidth * 2)


canFormSequence : Int -> List Int -> Cards -> Bool
canFormSequence sequenceWidth ranks cards =
    let
        ( spotcards, wildcards ) =
            partition isSpotCard cards

        wildsNeedToCompleteSets =
            countWildsNeeded sequenceWidth (collectCardsWithRanks ranks cards)

        wildsUsedAsNaturals =
            length (filter (\w -> member w.order ranks) wildcards)
    in
    wildsNeedToCompleteSets == (length wildcards - wildsUsedAsNaturals)


countWildsNeeded : Int -> Sets -> Int
countWildsNeeded sizeOfSets sets =
    sum (map (\set -> sizeOfSets - length set) sets)


collectCardsWithRanks : List Int -> Cards -> Sets
collectCardsWithRanks ranks cards =
    map (\rank -> filter (\c -> c.order == rank) cards) ranks


makeSequenceOfWidth : Int -> Maybe Sequence
makeSequenceOfWidth width_ =
    case width_ of
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
            filter isSequence sequences
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
