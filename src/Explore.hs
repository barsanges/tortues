{- |
   Module      : Explore
   Copyright   : Copyright (C) 2025 barsanges

Explore toutes les configurations possibles d'une grille.
-}

module Explore
  ( explore
  ) where

import Data.Foldable ( foldl' )
import qualified Data.Map as M
import qualified Data.Set as S
import Puzzle

-- | Renvoie toutes les configurations atteignables depuis une grille.
-- L'entier associé à chaque configuration indique le nombre minimum de
-- coups nécessaires pour atteindre cette configuration depuis la
-- configuration de départ.
explore :: (Int -> Puzzle -> Bool) -> Puzzle -> M.Map Puzzle Int
explore criterion = depth . (successors criterion)

-- | Indique la "profondeur" à laquelle se trouve chaque élément dans la
-- liste initiale.
depth :: (Foldable t, Ord a) => [t a] -> M.Map a Int
depth = snd . (foldl' go (0, M.empty))
  where
    -- go :: (Int, M.Map Int) -> [a] -> (Int, M.Map Int)
    go (n, dict) xs = (n+1, dict')
      where
        dict' = foldr (\ x d -> M.insert x n d) dict xs

-- | Renvoie toutes les configurations atteignables depuis une grille.
successors :: (Int -> Puzzle -> Bool) -> Puzzle -> [S.Set Puzzle]
successors criterion x0 = drill 0 S.empty (S.singleton x0)
  where
    drill :: Int -> S.Set Puzzle -> S.Set Puzzle -> [S.Set Puzzle]
    drill n prev xs
      | null xs = []
      | any (criterion n) xs = [xs]
      | otherwise = (xs:(drill (n+1) new ys))
      where
        new = S.union prev xs
        ys = (S.fromList $ concatMap move xs) `S.difference` new
