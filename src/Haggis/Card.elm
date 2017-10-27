module Haggis.Card exposing (..)


type alias Card =
    { suit : Suit
    , rank : Rank
    , order : Order
    , points : Int
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
    card.order == card_.order


{-| A "spot card" is a card game term for any card that is not
a Jack, Queen, or King which are called face cards. In Haggis,
face cards are wildcards: so, any non-wildcard is a spot card.
-}
isNatural : Card -> Bool
isNatural card =
    card.suit /= Wild


{-| We need to be able to sort cards by rank (order) or by suit (or both)
so, Suits need to be compareable. This seems like an instance
where we could benefit from using Haskell-like typeclasses...
-}
compareSuits : Suit -> Suit -> Basics.Order
compareSuits s s_ =
    case s of
        Red ->
            case s_ of
                Red ->
                    EQ

                otherwise ->
                    LT

        Orange ->
            case s_ of
                Red ->
                    GT

                Orange ->
                    EQ

                otherwise ->
                    LT

        Yellow ->
            case s_ of
                Red ->
                    GT

                Orange ->
                    GT

                Yellow ->
                    EQ

                otherwise ->
                    LT

        Green ->
            case s_ of
                Wild ->
                    LT

                Blue ->
                    LT

                Green ->
                    EQ

                otherwise ->
                    GT

        Blue ->
            case s_ of
                Wild ->
                    LT

                Blue ->
                    EQ

                otherwise ->
                    GT

        Wild ->
            GT
