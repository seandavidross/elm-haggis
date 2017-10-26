module Haggis.Cards exposing (Cards, Deck, Hand, subsets)

import Haggis.Card exposing (..)


type alias Cards =
    List Card


type alias Hand =
    Cards


type alias Deck =
    Cards


subsets : Cards -> List Cards
subsets cards =
    case cards of
        [] ->
            [ [] ]

        c :: cs ->
            List.map ((::) c) (subsets cs) ++ subsets cs
