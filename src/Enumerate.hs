{- |
   Module      : Enumerate
   Copyright   : Copyright (C) 2025 barsanges

Enumère des configurations initiales possibles.
-}

module Enumerate
  ( allPossibleFigures
  , allPossibleFences
  ) where

import Combinatorics ( tuples )
import qualified Data.Set as S
import Puzzle

-- | Toutes les combinaisons d'animaux possibles (indépendamment de
-- leur placement).
allPossibleFigures :: [S.Set Figure]
allPossibleFigures = go [ Green, Hare, Purple, Red, Blue, Yellow ]
  where
    go = S.toList . (S.filter (\ x -> S.size x > 1)) . S.powerSet . S.fromList

-- | Toutes les combinaisons de barrières possibles, de zéro à quatre
-- barrières.
allPossibleFences :: [(Maybe Fence, Maybe Fence, Maybe Fence, Maybe Fence)]
allPossibleFences = threes ++ fours
  where
    fences = [ F01, F12, F34, F45, F67, F78, F03, F14, F25, F36, F47, F58 ]
    threes = [ (Just x1, Just x2, Just x3, Nothing) | (x1:x2:x3:_) <- tuples 3 fences ]
    fours = [ (Just x1, Just x2, Just x3, Just x4) | (x1:x2:x3:x4:_) <- tuples 4 fences ]
