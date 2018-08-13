module Haggis.Combination
    exposing
        ( BombType(..)
        , Combination(..)
        , SequenceType(..)
        , SetType(..)
        , bomb
        , sequence
        , set
        )

import Haggis.Card as Card exposing (..)
import Haggis.Cards as Cards exposing (..)
import List exposing (..)


type SetType
    = Single Card.Rank
    | Pair Card.PipRank
    | Triple Card.PipRank
    | FourOfAKind Card.PipRank
    | FiveOfAKind Card.PipRank
    | SixOfAKind Card.PipRank
    | SevenOfAKind Card.PipRank
    | EightOfAKind Card.PipRank


type alias Sets =
    List Cards


type SinglesLength
    = ThreeSingles
    | FourSingles
    | FiveSingles
    | SixSingles
    | SevenSingles
    | EightSingles
    | NineSingles
    | TenSingles
    | ElevenSingles
    | TwelveSingles
    | ThirteenSingles


type PairsLength
    = TwoPairs
    | ThreePairs
    | FourPairs
    | FivePairs
    | SixPairs
    | SevenPairs
    | EightPairs


type TriplesLength
    = TwoTriples
    | ThreeTriples
    | FourTriples
    | FiveTriples


type FourOfAKindsLength
    = TwoFourOfAKinds
    | ThreeFourOfAKinds
    | FourFourOfAKinds


type FiveOfAKindsLength
    = TwoFiveOfAKinds
    | ThreeFiveOfAKinds


type SixOfAKindsLength
    = TwoSixOfAKinds


type SequenceDimensions
    = Singles SinglesLength
    | Pairs PairsLength
    | Triples TriplesLength
    | FourOfAKinds FourOfAKindsLength
    | FiveOfAKinds FiveOfAKindsLength
    | SixOfAKinds SixOfAKindsLength


type SequenceType
    = Run SequenceDimensions Card.Rank


type BombType
    = Rainbow
    | JQ
    | JK
    | QK
    | JQK
    | Suited


type Combination
    = Set SetType
    | Sequence SequenceType
    | Bomb BombType



-- SET


set : Cards -> Maybe (SetType Card.Rank)
set cards =
    if List.any Card.isNatural cards then
        let
            rank =
                List.head cards |> Maybe.map Card.rank
                
        in
            case List.length cards of
                1 ->
                    rank |> Maybe.map Single

                2 ->
                    rank |> Maybe.map Pair

                3 ->
                    rank |> Maybe.map Triple

                4 ->
                    rank |> Maybe.map FourOfAKind

                5 ->
                    rank |> Maybe.map FiveOfAKind

                6 ->
                    rank |> Maybe.map SixOfAKind

                7 ->
                    rank |> Maybe.map SevenOfAKind

                8 ->
                    rank |> Maybe.map EightOfAKind

                otherwise ->
                    Nothing
    else
        case cards of
            CourtCard ((rank, _), AsNaturalCourt _) _ ->
                rank |> Maybe.map Single
            
            otherwise ->
                Nothing


split : List Card -> ( List Card, List Card )
split cards =
    List.partition Card.isNatural cards



-- BOMB


bomb : Cards -> Maybe BombType
bomb cards =
    let
        ranks =
            cards
                |> List.map Card.rank
                |> List.sortWith Card.byRank
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

            [ Odd Three, Odd Five, Odd Seven, Odd Nine ] ->
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
    Nothing



-- let
--     ( naturals, wilds ) =
--         split cards
--
--     cardCount =
--         List.length cards
--
--     runLength =
--         cardCount // setSize
--
--     ranksInRun =
--         collectRanksInRun runLength naturals
-- in
--     case ranksInRun of
--         Just ( highestRank, ranks ) ->
--             if
--                 hasEnoughCards setSize cardCount
--                     && (cardCount == (runLength * setSize))
--                     && canFormSequence setSize ranks cards
--             then
--                 makeSequence runLength setSize highestRank
--             else
--                 Nothing
--
--         otherwise ->
--             Nothing


collectRanksInRun : Int -> List Card -> Maybe ( Rank, List Card.Rank )
collectRanksInRun runLength cards =
    Nothing



-- let
--     lowestOrder =
--         findLowestOrder cards
-- in
--     case lowestOrder of
--         Just low ->
--             let
--                 high =
--                     low + runLength - 1
--
--                 -- highestRank =
--                 --     Card.toRank high
--             in
--                 case highestRank of
--                     Just rank ->
--                         Just ( rank, List.range low high )
--
--                     otherwise ->
--                         Nothing
--
--         otherwise ->
--             Nothing


findLowestOrder : Cards -> Maybe Card.Rank
findLowestOrder cards =
    cards
        |> List.map Card.rank
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
            List.length (List.filter (\w -> List.member (Card.rank w) ranks) wilds)
    in
        wildsNeeded == (List.length wilds - wildsUsed)


countWildsNeeded : Int -> Sets -> Int
countWildsNeeded setSize sets =
    List.sum (List.map (\s -> setSize - List.length s) sets)


collectCardsWithRanks : List Int -> Cards -> Sets
collectCardsWithRanks ranks cards =
    List.map (\r -> List.filter (\c -> Card.rank c == r) cards) ranks



-- makeSequence : Int -> Int -> Card.Rank -> Maybe (Sequence Int Card.Rank)
-- makeSequence runLength setSize rank =
--     case setSize of
--         1 ->
--             Just (Singles runLength rank)
--
--         2 ->
--             Just (Pairs runLength rank)
--
--         3 ->
--             Just (Triples runLength rank)
--
--         4 ->
--             Just (FourOfAKinds runLength rank)
--
--         5 ->
--             Just (FiveOfAKinds runLength rank)
--
--         6 ->
--             Just (SixOfAKinds runLength rank)
--
--         otherwise ->
--             Nothing
