module Haggis.Card exposing (..)

import Array exposing (Array)
import Ordering exposing (..)


type alias Card =
    { suit : Suit
    , rank : Rank
    , order : Order
    , points : Points
    }


type Suit
    = Red
    | Orange
    | Yellow
    | Green
    | Blue
    | Wild


type Rank
    = Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King


type alias Order =
    Int


type alias Points =
    Int


suit : Card -> Suit
suit card =
    card.suit


suits : List Suit
suits =
    [ Red
    , Orange
    , Yellow
    , Green
    , Blue
    , Wild
    ]


rank : Card -> Rank
rank card =
    card.rank


ranks : Array Rank
ranks =
    Array.fromList
        [ Two
        , Three
        , Four
        , Five
        , Six
        , Seven
        , Eight
        , Nine
        , Ten
        , Jack
        , Queen
        , King
        ]


order : Card -> Order
order card =
    card.order


rankOrdering : Ordering Rank
rankOrdering =
    Ordering.explicit (Array.toList ranks)


byRank : Rank -> Rank -> Basics.Order
byRank r1 r2 =
    rankOrdering r1 r2


cardOrdering : Ordering Card
cardOrdering =
    Ordering.byRank
        (\card ->
            1
        )
        (\c1 c2 ->
            rankOrdering (rank c1) (rank c2)
        )


toRank : Int -> Maybe Rank
toRank order =
    case order of
        2 ->
            Just Two

        3 ->
            Just Three

        4 ->
            Just Four

        5 ->
            Just Five

        6 ->
            Just Six

        7 ->
            Just Seven

        8 ->
            Just Eight

        9 ->
            Just Nine

        10 ->
            Just Ten

        11 ->
            Just Jack

        12 ->
            Just Queen

        13 ->
            Just King

        otherwise ->
            Nothing


equal : Card -> Card -> Bool
equal card card_ =
    (rank card) == (rank card_)


{-| A "spot card" is a card game term for any card that is not
a Jack, Queen, or King which are called face cards. In Haggis,
since face cards are wild, all spot cards are natural.
-}
isNatural : Card -> Bool
isNatural card =
    (suit card) /= Wild
