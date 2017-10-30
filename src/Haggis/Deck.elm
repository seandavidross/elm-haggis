module Haggis.Deck exposing (..)

import Haggis.Card exposing (..)
import Haggis.Cards exposing (..)
import Haggis.Hand exposing (..)
import Random exposing (generate)
import Random.List exposing (..)


type alias Deck =
    Cards



-- shuffle : Deck -> Cmd Deck
-- shuffle deck =
--     Random.generate (Random.List.shuffle) deck
