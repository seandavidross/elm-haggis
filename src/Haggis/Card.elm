module Haggis.Card exposing (..)

{-| TODO
Would it be worthwhile to make points' type be 'Point value'?
-}


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


{-| TODO
Might want to try 'type Rank ordinal' so that when we say 'Two 2' we have
both the rank's denomination (Two) and its order relative to other ranks.
-}
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


{-| TODO
Probably need to handle changing wild card ranks
to fit the combination in which they are played...
-}
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


{-| A "spot card" is a card game term for any card that is not
a Jack, Queen, or King which are called face cards. In Haggis,
face cards are wildcards: so, any non-wildcard is a spot card.
-}
isSpotCard : Card -> Bool
isSpotCard card =
    card.suit /= Wild


{-| We need to be able to sort cards by rank or by suit (or both)
so, Suits need to be compareable. This seems like an instance
where we could benefit from using Haskell-like typeclasses...
-}
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
