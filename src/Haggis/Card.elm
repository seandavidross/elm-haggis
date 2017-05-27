module Haggis.Card exposing (..)

{-| NOTE
Would it be worthwhile to make points' type be 'Point value'?
-}


type alias Card =
    { suit : Suit
    , rank : Rank
    , order : Ordinal
    , points : Int
    }


type Suit
    = Red
    | Orange
    | Yellow
    | Green
    | Blue
    | Wild


{-| NOTE
Might want to try 'type Rank ordinal' so that when we say 'Two 2' we have
both the rank's denomination (Two) and its order relative to other ranks.

NOTE
Tried Rank ordinal, couldn't figure out how to get the ordinal value out
of a card's rank. Rather than fight with that, I'm moving forward with
what is already working

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


type alias Ordinal =
    Int


equal : Card -> Card -> Bool
equal card card' =
    card.order == card'.order


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
