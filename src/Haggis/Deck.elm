module Haggis.Deck exposing (..)

import Haggis.Card exposing (..)
import Haggis.Cards exposing (..)
import Haggis.Hand exposing (..)
import Random exposing (generate)
import Random.List exposing (..)
import Time exposing (..)


type alias Deck =
    Cards


shuffle : Deck -> Random.Seed -> ( Deck, Random.Seed )
shuffle deck seed =
    Random.step (Random.List.shuffle deck) seed
