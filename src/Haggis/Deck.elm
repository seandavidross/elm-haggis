module Haggis.Deck exposing (..)

import Haggis.Card exposing (..)
import Haggis.Cards exposing (..)
import Haggis.Hand exposing (..)
import Random exposing (generate)
import Random.List exposing (..)


type alias Deck =
    Cards


shuffle : Random.Seed -> Deck -> ( Deck, Random.Seed )
shuffle seed deck =
    Random.step (Random.List.shuffle deck) seed


deal : Int -> Deck -> ( Deck, Hand )
deal handSize deck =
    ( List.drop handSize deck, List.take handSize deck )


stock : Deck
stock =
    [ Card Red Two 2 0
    , Card Red Three 3 1
    , Card Red Four 4 0
    , Card Red Five 5 1
    , Card Red Six 6 0
    , Card Red Seven 7 1
    , Card Red Eight 8 0
    , Card Red Nine 9 1
    , Card Red Ten 10 0
    , Card Orange Two 2 0
    , Card Orange Three 3 1
    , Card Orange Four 4 0
    , Card Orange Five 5 1
    , Card Orange Six 6 0
    , Card Orange Seven 7 1
    , Card Orange Eight 8 0
    , Card Orange Nine 9 1
    , Card Orange Ten 10 0
    , Card Yellow Two 2 0
    , Card Yellow Three 3 1
    , Card Yellow Four 4 0
    , Card Yellow Five 5 1
    , Card Yellow Six 6 0
    , Card Yellow Seven 7 1
    , Card Yellow Eight 8 0
    , Card Yellow Nine 9 1
    , Card Yellow Ten 10 0
    , Card Green Two 2 0
    , Card Green Three 3 1
    , Card Green Four 4 0
    , Card Green Five 5 1
    , Card Green Six 6 0
    , Card Green Seven 7 1
    , Card Green Eight 8 0
    , Card Green Nine 9 1
    , Card Green Ten 10 0
    , Card Blue Two 2 0
    , Card Blue Three 3 1
    , Card Blue Four 4 0
    , Card Blue Five 5 1
    , Card Blue Six 6 0
    , Card Blue Seven 7 1
    , Card Blue Eight 8 0
    , Card Blue Nine 9 1
    , Card Blue Ten 10 0
    , Card Wild Jack 11 2
    , Card Wild Queen 12 3
    , Card Wild King 13 5
    ]
