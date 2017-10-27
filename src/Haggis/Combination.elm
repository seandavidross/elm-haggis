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

import Haggis.Card as Card exposing (..)
import Haggis.Cards as Cards exposing (..)
import List exposing (..)


type Combination
    = Set
    | Sequence
    | Bomb


type Set rank
    = Single Card.Rank
    | Pair Card.Rank
    | Triple Card.Rank
    | FourOfAKind Card.Rank
    | FiveOfAKind Card.Rank
    | SixOfAKind Card.Rank
    | SevenOfAKind Card.Rank
    | EightOfAKind Card.Rank


type alias Sets =
    List Cards


type Sequence qty rank
    = RunOfSingles Int Card.Order
    | RunOfPairs Int Card.Order
    | RunOfTriples Int Card.Order
    | RunOfFourOfAKinds Int Card.Order
    | RunOfFiveOfAKinds Int Card.Order
    | RunOfSixOfAKinds Int Card.Order


type Bomb
    = Rainbow
    | JQ
    | JK
    | QK
    | JQK
    | Suited



-- SET


set : Cards -> Maybe (Set Card.Rank)
set cards =
    let
        ( spotcards, wildcards ) =
            List.partition isSpotCard cards

        rank =
            Result.withDefault Two (findRank cards)
    in
        if allSameRank spotcards then
            makeSet cards
        else if List.length wildcards == 1 && List.length cards == 1 then
            Just (Single rank)
        else
            Nothing


allSameRank : Cards -> Bool
allSameRank cards =
    case cards of
        [] ->
            False

        first :: rest ->
            List.all (equal first) rest


findRank : Cards -> Result String Card.Rank
findRank cards =
    case cards of
        [] ->
            Err "Expected to get one or more cards but got zero."

        c :: [] ->
            Ok c.rank

        c :: cs ->
            Ok c.rank


makeSet : Cards -> Maybe (Set Card.Rank)
makeSet cards =
    let
        rank =
            Result.withDefault Two (findRank cards)
    in
        case length cards of
            1 ->
                Just (Single rank)

            2 ->
                Just (Pair rank)

            3 ->
                Just (Triple rank)

            4 ->
                Just (FourOfAKind rank)

            5 ->
                Just (FiveOfAKind rank)

            6 ->
                Just (SixOfAKind rank)

            otherwise ->
                Nothing



-- BOMB


bomb : Cards -> Maybe Bomb
bomb cards =
    let
        ranks =
            cards |> List.sortBy .order |> List.map .rank
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
            List.all (hasSameSuit first) rest

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
        |> List.map .suit
        |> dropDuplicates
        |> List.length


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


sequence : Cards -> List (Maybe (Sequence Int Card.Order))
sequence cards =
    let
        ( spotcards, wildcards ) =
            List.partition isSpotCard cards

        sequenceWidths =
            range (countSuits spotcards) (countSuits cards)
    in
        if List.length spotcards == 0 then
            [ Nothing ]
        else
            sequenceWidths
                |> List.map (maybeSequenceOfWidth cards)
                |> keepJustSequences


maybeSequenceOfWidth : Cards -> Int -> Maybe (Sequence Int Card.Order)
maybeSequenceOfWidth cards sequenceWidth =
    let
        ( spotcards, wildcards ) =
            List.partition isSpotCard cards

        numberOfCards =
            List.length cards

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
            hasEnoughCards sequenceWidth numberOfCards
                && (numberOfCards == (sequenceLength * sequenceWidth))
                && canFormSequence sequenceWidth ranks cards
        then
            makeSequence sequenceLength sequenceWidth highRank
        else
            Nothing


findLowestRank : Cards -> Card.Order
findLowestRank cards =
    cards |> List.map .order |> List.minimum |> Maybe.withDefault 2


hasEnoughCards : Int -> Int -> Bool
hasEnoughCards sequenceWidth numberOfCards =
    (sequenceWidth == 1 && numberOfCards >= 3)
        || (sequenceWidth > 1 && numberOfCards >= sequenceWidth * 2)


canFormSequence : Int -> List Int -> Cards -> Bool
canFormSequence sequenceWidth ranks cards =
    let
        ( spotcards, wildcards ) =
            List.partition isSpotCard cards

        wildsNeedToCompleteSets =
            countWildsNeeded sequenceWidth (collectCardsWithRanks ranks cards)

        wildsUsedAsNaturals =
            List.length (List.filter (\w -> member w.order ranks) wildcards)
    in
        wildsNeedToCompleteSets == (List.length wildcards - wildsUsedAsNaturals)


countWildsNeeded : Int -> Sets -> Int
countWildsNeeded sizeOfSets sets =
    List.sum (List.map (\set -> sizeOfSets - List.length set) sets)


collectCardsWithRanks : List Int -> Cards -> Sets
collectCardsWithRanks ranks cards =
    List.map (\rank -> List.filter (\c -> c.order == rank) cards) ranks


makeSequence : Int -> Int -> Card.Order -> Maybe (Sequence Int Card.Order)
makeSequence sequenceLength sequenceWidth rank =
    case sequenceWidth of
        1 ->
            Just (RunOfSingles sequenceLength rank)

        2 ->
            Just (RunOfPairs sequenceLength rank)

        3 ->
            Just (RunOfTriples sequenceLength rank)

        4 ->
            Just (RunOfFourOfAKinds sequenceLength rank)

        5 ->
            Just (RunOfFiveOfAKinds sequenceLength rank)

        6 ->
            Just (RunOfSixOfAKinds sequenceLength rank)

        otherwise ->
            Nothing


keepJustSequences : List (Maybe (Sequence Int Card.Order)) -> List (Maybe (Sequence Int Card.Order))
keepJustSequences sequences =
    let
        justSequences =
            List.filter isSequence sequences
    in
        case justSequences of
            [] ->
                [ Nothing ]

            otherwise ->
                justSequences


isSequence : Maybe (Sequence Int Card.Order) -> Bool
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
