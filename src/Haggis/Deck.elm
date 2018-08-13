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
    [ Card.create <| Card.pip ( Even Two, Red )
    , Card.create <| Card.pip ( Odd Three, Red )
    , Card.create <| Card.pip ( Even Four, Red )
    , Card.create <| Card.pip ( Odd Five, Red )
    , Card.create <| Card.pip ( Even Six, Red )
    , Card.create <| Card.pip ( Odd Seven, Red )
    , Card.create <| Card.pip ( Even Eight, Red )
    , Card.create <| Card.pip ( Odd Nine, Red )
    , Card.create <| Card.pip ( Even Ten, Red )
    , Card.create <| Card.pip ( Even Two, Orange )
    , Card.create <| Card.pip ( Odd Three, Orange )
    , Card.create <| Card.pip ( Even Four, Orange )
    , Card.create <| Card.pip ( Odd Five, Orange )
    , Card.create <| Card.pip ( Even Six, Orange )
    , Card.create <| Card.pip ( Odd Seven, Orange )
    , Card.create <| Card.pip ( Even Eight, Orange )
    , Card.create <| Card.pip ( Odd Nine, Orange )
    , Card.create <| Card.pip ( Even Ten, Orange )
    , Card.create <| Card.pip ( Even Two, Yellow )
    , Card.create <| Card.pip ( Odd Three, Yellow )
    , Card.create <| Card.pip ( Even Four, Yellow )
    , Card.create <| Card.pip ( Odd Five, Yellow )
    , Card.create <| Card.pip ( Even Six, Yellow )
    , Card.create <| Card.pip ( Odd Seven, Yellow )
    , Card.create <| Card.pip ( Even Eight, Yellow )
    , Card.create <| Card.pip ( Odd Nine, Yellow )
    , Card.create <| Card.pip ( Even Ten, Yellow )
    , Card.create <| Card.pip ( Even Two, Green )
    , Card.create <| Card.pip ( Odd Three, Green )
    , Card.create <| Card.pip ( Even Four, Green )
    , Card.create <| Card.pip ( Odd Five, Green )
    , Card.create <| Card.pip ( Even Six, Green )
    , Card.create <| Card.pip ( Odd Seven, Green )
    , Card.create <| Card.pip ( Even Eight, Green )
    , Card.create <| Card.pip ( Odd Nine, Green )
    , Card.create <| Card.pip ( Even Ten, Green )
    , Card.create <| Card.pip ( Even Two, Blue )
    , Card.create <| Card.pip ( Odd Three, Blue )
    , Card.create <| Card.pip ( Even Four, Blue )
    , Card.create <| Card.pip ( Odd Five, Blue )
    , Card.create <| Card.pip ( Even Six, Blue )
    , Card.create <| Card.pip ( Odd Seven, Blue )
    , Card.create <| Card.pip ( Even Eight, Blue )
    , Card.create <| Card.pip ( Odd Nine, Blue )
    , Card.create <| Card.pip ( Even Ten, Blue )
    ]


wilds : Cards
wilds =
    [ Card.create <| Card.court ( Jack, Black )
    , Card.create <| Card.court ( Queen, Black )
    , Card.create <| Card.court ( King, Black )
    ]
