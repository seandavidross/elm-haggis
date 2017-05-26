module Haggis.Card exposing (..)


type alias Card =
    { suit : Suit
    , rank : Rank
    , points : Int
    }


type alias Cards =
    List Card


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


suit : Card -> Suit
suit card =
    card.suit


rank : Card -> Int
rank c =
    case c.rank of
        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6

        Seven ->
            7

        Eight ->
            8

        Nine ->
            9

        Ten ->
            10

        Jack ->
            11

        Queen ->
            12

        King ->
            13


equal : Card -> Card -> Bool
equal c1 c2 =
    rank c1 == rank c2


isSpotCard : Card -> Bool
isSpotCard card =
    card.suit /= Wild


compareSuits : Suit -> Suit -> Basics.Order
compareSuits s s' =
    case s of
        Red ->
            case s' of
                Red ->
                    EQ

                otherwise ->
                    LT

        Orange ->
            case s' of
                Red ->
                    GT

                Orange ->
                    EQ

                otherwise ->
                    LT

        Yellow ->
            case s' of
                Red ->
                    GT

                Orange ->
                    GT

                Yellow ->
                    EQ

                otherwise ->
                    LT

        Green ->
            case s' of
                Wild ->
                    LT

                Blue ->
                    LT

                Green ->
                    EQ

                otherwise ->
                    GT

        Blue ->
            case s' of
                Wild ->
                    LT

                Blue ->
                    EQ

                otherwise ->
                    GT

        Wild ->
            GT
