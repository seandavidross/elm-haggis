module Haggis.Cards exposing (Cards, subsets)

import Haggis.Card exposing (..)


type alias Cards =
    List Card


subsets : Cards -> List Cards
subsets cards =
    case cards of
        [] ->
            [ [] ]

        c :: cs ->
            List.map ((::) c) (subsets cs) ++ subsets cs
