module Haggis.Hand exposing (..)

import Haggis.Card as Card exposing (..)
import Haggis.Cards as Cards exposing (..)
import Haggis.Combination as Combination exposing (..)


type alias Hand =
    Cards


collectSets : Hand -> List (Combination.Set Card.Rank)
collectSets hand =
    List.filterMap Combination.set (Cards.subsets hand)


collectSequences : Hand -> List (Combination.Sequence Int Card.Order)
collectSequences hand =
    List.filterMap
        identity
        (List.concatMap Combination.sequence (Cards.subsets hand))


collectBombs : Hand -> List Combination.Bomb
collectBombs hand =
    List.filterMap Combination.bomb (Cards.subsets hand)
