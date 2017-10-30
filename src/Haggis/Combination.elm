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

        highestRank =
            findRank cards
    in
    case highestRank of
        Just rank ->
            if allSameRank naturals then
                makeSet cards
            else if List.length wilds == 1 && List.length cards == 1 then
                Just (Single rank)
            else
                Nothing

        otherwise ->
            Nothing


split : List Card -> ( List Card, List Card )
split cards =
    List.partition Card.isNatural cards


allSameRank : Cards -> Bool
allSameRank cards =
    case cards of
        [] ->
            False

        first :: rest ->
            List.all (Card.equal first) rest


findRank : Cards -> Maybe Card.Rank
findRank cards =
    case cards of
        [] ->
            Nothing

        first :: [] ->
            Just (Card.rank first)

        first :: rest ->
            case Card.suit first of
                Wild ->
                    findRank rest

                otherwise ->
                    Just (Card.rank first)


makeSet : Cards -> Maybe (Set Card.Rank)
makeSet cards =
    let
        highestRank =
            findRank cards
    in
    case highestRank of
        Just rank ->
            case List.length cards of
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
dropDuplicates : List Card.Suit -> List Card.Suit
dropDuplicates suits =
    dropDuplicates_ [] suits


dropDuplicates_ : List Card.Suit -> List Card.Suit -> List Card.Suit
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


sequence : Cards -> List (Sequence Int Card.Rank)
sequence cards =
    let
        ( naturals, wilds ) =
            split cards

        setSizes =
            List.range (countSuits naturals) (countSuits cards)
    in
    if List.length naturals == 0 then
        []
    else
        List.filterMap (maybeRunOfSets cards) setSizes


maybeRunOfSets : Cards -> Int -> Maybe (Sequence Int Card.Rank)
maybeRunOfSets cards setSize =
    let
        ( naturals, wilds ) =
            split cards

        cardCount =
            List.length cards

        runLength =
            cardCount // setSize

        ranksInRun =
            collectRanksInRun runLength naturals
    in
    case ranksInRun of
        Just ( highestRank, ranks ) ->
            if
                hasEnoughCards setSize cardCount
                    && (cardCount == (runLength * setSize))
                    && canFormSequence setSize ranks cards
            then
                makeSequence runLength setSize highestRank
            else
                Nothing

        otherwise ->
            Nothing


collectRanksInRun : Int -> List Card -> Maybe ( Rank, List Card.Order )
collectRanksInRun runLength cards =
    let
        lowestOrder =
            findLowestOrder cards
    in
    case lowestOrder of
        Just low ->
            let
                high =
                    low + runLength - 1

                highestRank =
                    Card.toRank high
            in
            case highestRank of
                Just rank ->
                    Just ( rank, List.range low high )

                otherwise ->
                    Nothing

        otherwise ->
            Nothing


findLowestOrder : Cards -> Maybe Card.Order
findLowestOrder cards =
    cards
        |> List.map Card.order
        |> List.minimum


hasEnoughCards : Int -> Int -> Bool
hasEnoughCards setSize cardCount =
    let
        shortestSinglesRun =
            3

        shortestOfAKindRun =
            2
    in
    (setSize == 1 && cardCount >= shortestSinglesRun)
        || (setSize > 1 && cardCount >= setSize * shortestOfAKindRun)


canFormSequence : Int -> List Int -> Cards -> Bool
canFormSequence setSize ranks cards =
    let
        ( naturals, wilds ) =
            split cards

        wildsNeeded =
            countWildsNeeded setSize (collectCardsWithRanks ranks cards)

        wildsUsed =
            List.length (List.filter (\w -> List.member (Card.order w) ranks) wilds)
    in
    wildsNeeded == (List.length wilds - wildsUsed)


countWildsNeeded : Int -> Sets -> Int
countWildsNeeded setSize sets =
    List.sum (List.map (\s -> setSize - List.length s) sets)


collectCardsWithRanks : List Int -> Cards -> Sets
collectCardsWithRanks ranks cards =
    List.map (\r -> List.filter (\c -> Card.order c == r) cards) ranks


makeSequence : Int -> Int -> Card.Rank -> Maybe (Sequence Int Card.Rank)
makeSequence runLength setSize rank =
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
