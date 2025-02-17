{- |
   Module      : Solve
   Copyright   : Copyright (C) 2025 barsanges

Résout un puzzle du jeu "Le lièvre et les tortues".
-}

module Solve
  ( test
  , solve
  ) where

import Data.Maybe ( isNothing )
import qualified Data.Set as S
import Data.Sequence ( Seq(..) )
import Puzzle

-- | Teste si un puzzle admet une solution.
test :: Puzzle -> Bool
test x = isNothing (test' [x] S.empty)

-- | Utilitaire de `test`.
test' :: [Puzzle] -> S.Set Hash -> Maybe (S.Set Hash)
test' [] s = Just s
test' (x:xs) s
  | check x = Nothing
  | otherwise = ms' >>= (test' xs)
  where
    h = fromPuzzle x
    ms' = if h `S.member` s
          then Just s
          else test' (move x) (S.insert h s)

-- | Trouve une solution optimale (si elle existe) au puzzle. S'il
-- existe plusieurs solutions optimales, aucune garantie n'est faite
-- sur la solution exacte renvoyée par la fonction.
solve :: Puzzle -> Maybe (Seq Puzzle)
solve x = fmap (fmap toPuzzle) $ solve' Empty x Nothing

-- | Utilitaire de `solve`.
solve' :: Seq Hash
       -> Puzzle
       -> Maybe (Seq Hash)
       -> Maybe (Seq Hash)
solve' prev x mbest
  | maybeLowerBy length mbest (Just prev) = mbest
  | h `elem` prev = mbest
  | check x = Just (prev :|> h)
  | maybeLowerBy length mbest msol = mbest
  | otherwise =  msol
  where
    h = fromPuzzle x
    msol = foldr (solve' (prev :|> h)) mbest (move x)

-- | Compare deux éléments incertains selon une métrique donnée.
maybeLowerBy :: Ord b => (a -> b) -> Maybe a -> Maybe a -> Bool
maybeLowerBy _ Nothing Nothing = True
maybeLowerBy _ (Just _) Nothing = True
maybeLowerBy _ Nothing (Just _) = False
maybeLowerBy f (Just x) (Just y) = (f x) <= (f y)
