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
explore :: (Int -> Puzzle -> Bool) -> Puzzle -> M.Map Hash Int
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
successors :: (Int -> Puzzle -> Bool) -> Puzzle -> [S.Set Hash]
successors criterion x0 = drill 0 S.empty ((S.singleton . fromPuzzle) x0, [x0])
  where

    drill :: Int -> S.Set Hash -> (S.Set Hash, [Puzzle]) -> [S.Set Hash]
    drill _ _ (hs, []) = [hs]
    drill n prev (hs, xs)
      | any (criterion n) xs = [hs]
      | otherwise = (hs:(drill (n+1) new (gs, ys)))
      where
        new = S.union prev hs
        (gs, ys) = filterHash $ concatMap move xs

        filterHash :: [Puzzle] -> (S.Set Hash, [Puzzle])
        filterHash [] = (S.empty, [])
        filterHash (z:zs)
          | h `S.member` prev = (hs', zs')
          | otherwise = (h `S.insert` hs', z:zs')
          where
            h = fromPuzzle z
            (hs', zs') = filterHash zs
