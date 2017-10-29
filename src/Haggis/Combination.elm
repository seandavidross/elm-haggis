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
    = RunOfSingles Int Card.Rank
    | RunOfPairs Int Card.Rank
    | RunOfTriples Int Card.Rank
    | RunOfFourOfAKinds Int Card.Rank
    | RunOfFiveOfAKinds Int Card.Rank
    | RunOfSixOfAKinds Int Card.Rank


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
        ( naturals, wilds ) =
            split cards

        rank =
            Result.withDefault Two (findRank cards)
    in
        if allSameRank naturals then
            makeSet cards
        else if List.length wilds == 1 && List.length cards == 1 then
            Just (Single rank)
        else
            Nothing


split : List Card -> ( List Card, List Card )
split cards =
    List.partition isNatural cards


allSameRank : Cards -> Bool
allSameRank cards =
    case cards of
        [] ->
            False

        first :: rest ->
            List.all (Card.equal first) rest


findRank : Cards -> Result String Card.Rank
findRank cards =
    case cards of
        [] ->
            Err "Expected to get one or more cards but got zero."

        c :: [] ->
            Ok (Card.rank c)

        c :: cs ->
            case c.suit of
                Wild ->
                    findRank cs

                otherwise ->
                    Ok (Card.rank c)


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
            cards |> List.map Card.rank |> List.sortWith Card.byRank
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
hasSameSuit c1 c2 =
    Card.suit c1 == Card.suit c2


hasFourSuits : Cards -> Bool
hasFourSuits cards =
    countSuits cards == 4


countSuits : Cards -> Int
countSuits cards =
    cards
        |> List.map Card.suit
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


sequence : Cards -> List (Maybe (Sequence Int Card.Rank))
sequence cards =
    let
        ( naturals, wilds ) =
            split cards

        widths =
            List.range (countSuits naturals) (countSuits cards)
    in
        if List.length naturals == 0 then
            [ Nothing ]
        else
            widths
                |> List.map (maybeSequenceOfWidth cards)
                |> keepJustSequences


maybeSequenceOfWidth : Cards -> Int -> Maybe (Sequence Int Card.Rank)
maybeSequenceOfWidth cards setSize =
    let
        ( naturals, wilds ) =
            split cards

        cardCount =
            List.length cards

        lowestRank =
            findLowestRank naturals

        runLength =
            cardCount // setSize

        highestRank =
            lowestRank + runLength - 1

        ranks =
            List.range lowestRank highestRank
    in
        if
            hasEnoughCards setSize cardCount
                && (cardCount == (runLength * setSize))
                && canFormSequence setSize ranks cards
        then
            makeSequence runLength setSize highestRank
        else
            Nothing


findLowestRank : Cards -> Card.Order
findLowestRank cards =
    cards |> List.map Card.order |> List.minimum |> Maybe.withDefault 2


hasEnoughCards : Int -> Int -> Bool
hasEnoughCards setSize cardCount =
    (setSize == 1 && cardCount >= 3)
        || (setSize > 1 && cardCount >= setSize * 2)


canFormSequence : Int -> List Int -> Cards -> Bool
canFormSequence setSize ranks cards =
    let
        ( naturals, wilds ) =
            split cards

        wildsNeeded =
            countWildsNeeded setSize (collectCardsWithRanks ranks cards)

        wildsUsed =
            List.length (List.filter (\w -> member (Card.order w) ranks) wilds)
    in
        wildsNeeded == (List.length wilds - wildsUsed)


countWildsNeeded : Int -> Sets -> Int
countWildsNeeded sizeOfSets sets =
    List.sum (List.map (\set -> sizeOfSets - List.length set) sets)


collectCardsWithRanks : List Int -> Cards -> Sets
collectCardsWithRanks ranks cards =
    List.map (\rank -> List.filter (\c -> (Card.order c) == rank) cards) ranks


makeSequence : Int -> Int -> Card.Order -> Maybe (Sequence Int Card.Rank)
makeSequence runLength setSize order =
    let
        rank =
            Maybe.withDefault Two (toRank order)
    in
        case setSize of
            1 ->
                Just (RunOfSingles runLength rank)

            2 ->
                Just (RunOfPairs runLength rank)

            3 ->
                Just (RunOfTriples runLength rank)

            4 ->
                Just (RunOfFourOfAKinds runLength rank)

            5 ->
                Just (RunOfFiveOfAKinds runLength rank)

            6 ->
                Just (RunOfSixOfAKinds runLength rank)

            otherwise ->
                Nothing


keepJustSequences : List (Maybe (Sequence Int Card.Rank)) -> List (Maybe (Sequence Int Card.Rank))
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


isSequence : Maybe (Sequence Int Card.Rank) -> Bool
isSequence s =
    case s of
        Nothing ->
            False

        otherwise ->
            True
