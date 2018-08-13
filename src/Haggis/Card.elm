module Haggis.Card exposing (..)

import Array exposing (Array)
import Ordering exposing (..)


type NaturalSuit
    = Red
    | Orange
    | Yellow
    | Green
    | Blue


type WildSuit
    = Black


type Suit
    = Natural NaturalSuit
    | Wild WildSuit


type EvenRank
    = Two
    | Four
    | Six
    | Eight
    | Ten


type OddRank
    = Three
    | Five
    | Seven
    | Nine


type PipRank
    = Even EvenRank
    | Odd OddRank


type CourtRank
    = Jack
    | Queen
    | King


type Rank
    = Pip PipRank
    | Court CourtRank


type alias PipIndex =
    ( PipRank, NaturalSuit )


type alias CourtIndex =
    ( CourtRank, WildSuit )


type CardIndex
    = PipCardIndex PipIndex
    | CourtCardIndex CourtIndex


pip : PipIndex -> CardIndex
pip =
    PipCardIndex


court : CourtIndex -> CardIndex
court =
    CourtCardIndex


type alias UnnaturalIndex =
    ( CourtRank, NaturalSuit )


type AsRoleIndex
    = AsNaturalPip PipIndex
    | AsNaturalCourt CourtIndex
    | AsUnnaturalCourt UnnaturalIndex


type alias WildIndex =
    ( CourtIndex, AsRoleIndex )


type Points
    = Points Int


type Card
    = NaturalCard PipIndex Points
    | CourtCard WildIndex Points


create : CardIndex -> Card
create cardIndex =
    case cardIndex of
        PipCardIndex pipIndex ->
            case pipIndex of
                ( Even _, _ ) ->
                    NaturalCard pipIndex <| Points 0

                ( Odd _, _ ) ->
                    NaturalCard pipIndex <| Points 1

        CourtCardIndex courtIndex ->
            case courtIndex of
                ( Jack, _ ) ->
                    CourtCard ( courtIndex, AsNaturalCourt courtIndex ) <| Points 2

                ( Queen, _ ) ->
                    CourtCard ( courtIndex, AsNaturalCourt courtIndex ) <| Points 3

                ( King, _ ) ->
                    CourtCard ( courtIndex, AsNaturalCourt courtIndex ) <| Points 5


rank : Card -> Rank
rank card =
    case card of
        NaturalCard ( r, _ ) _ ->
            Pip r

        CourtCard ( _, AsNaturalPip ( r, _ ) ) _ ->
            Pip r

        CourtCard ( _, AsNaturalCourt ( r, _ ) ) _ ->
            Court r

        CourtCard ( _, AsUnnaturalCourt ( r, _ ) ) _ ->
            Court r


suit : Card -> Suit
suit card =
    case card of
        NaturalCard ( _, s ) _ ->
            Natural s

        CourtCard ( _, AsNaturalPip ( _, s ) ) _ ->
            Natural s

        CourtCard ( _, AsUnnaturalCourt ( _, s ) ) _ ->
            Natural s

        CourtCard ( _, AsNaturalCourt ( _, s ) ) _ ->
            Wild s


suits : List Suit
suits =
    [ Natural Red
    , Natural Orange
    , Natural Yellow
    , Natural Green
    , Natural Blue
    , Wild Black
    ]


ranks : Array Rank
ranks =
    Array.fromList
        [ Pip <| Even Two
        , Pip <| Odd Three
        , Pip <| Even Four
        , Pip <| Odd Five
        , Pip <| Even Six
        , Pip <| Odd Seven
        , Pip <| Even Eight
        , Pip <| Odd Nine
        , Pip <| Even Ten
        , Court Jack
        , Court Queen
        , Court King
        ]


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


equals : Card -> Card -> Bool
equals lhs rhs =
    (rank lhs) == (rank rhs)


isNatural : Card -> Bool
isNatural card =
    case card of
        NaturalCard _ _ ->
            True

        otherwise ->
            False


points : Card -> Points
points card =
    case card of
        NaturalCard _ p ->
            p

        CourtCard _ p ->
            p


isFlush : Card -> Card -> Bool
isFlush lhs rhs =
    suit lhs == suit rhs

