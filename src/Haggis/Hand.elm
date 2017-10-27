module Haggis.Hand exposing (..)

import Haggis.Card exposing (..)
import Haggis.Cards exposing (..)
import Haggis.Combination exposing (..)


type alias Hand =
    Cards


collectSets : Hand -> List (Set Rank)
collectSets hand =
    List.filterMap set (subsets hand)


collectSequences : Hand -> List (Sequence Int Haggis.Card.Order)
collectSequences hand =
    List.filterMap
        identity
        (List.concatMap sequence (subsets hand))


collectBombs : Hand -> List Bomb
collectBombs hand =
    List.filterMap bomb (subsets hand)
