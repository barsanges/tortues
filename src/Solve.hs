{- |
   Module      : Solve
   Copyright   : Copyright (C) 2025 barsanges

Résout un puzzle du jeu "Le lièvre et les tortues".
-}

module Solve
  ( test
  , solve
  , solve'
  ) where

import Algorithm.Search ( dijkstra )
import Data.Maybe ( isNothing )
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Sequence ( Seq(..) )
import qualified Data.Sequence as Sq
import Puzzle
import Explore

-- | Teste si un puzzle admet une solution.
test :: Puzzle -> Bool
test x = isNothing (test' [x] S.empty)

-- | Utilitaire de `test`.
test' :: [Puzzle] -> S.Set Puzzle -> Maybe (S.Set Puzzle)
test' [] s = Just s
test' (x:xs) s
  | check x = Nothing
  | otherwise = ms' >>= (test' xs)
  where
    ms' = if x `S.member` s
          then Just s
          else test' (move x) (S.insert x s)

-- | Trouve une solution optimale (si elle existe) au puzzle. S'il
-- existe plusieurs solutions optimales, aucune garantie n'est faite
-- sur la solution exacte renvoyée par la fonction.
solve :: Puzzle -> Maybe (Seq Puzzle)
solve x0 = case M.lookup x0 dict of
             Nothing -> Nothing
             Just n -> go n [x0]
  where
    solved = convertToSolved x0
    dict = explore (\ _ y -> y == x0) solved

    go :: Int -> [Puzzle] -> Maybe (Seq Puzzle)
    go _ [] = Nothing
    go n (y:ys) = case M.lookup y dict of
      Nothing -> go n ys
      Just p | y == solved -> Just (Sq.singleton solved)
             | p == n -> fmap ((:<|) y) (go (n-1) (move y))
             | otherwise -> go n ys

-- | Une implémentation alternative de `solve` - peut-être un peu
-- moins rapide, mais plus maligne parce qu'elle réemploie une
-- librairie existante.
solve' :: Puzzle -> Maybe (Seq Puzzle)
solve' x = fmap (Sq.fromList . ((:) x) . snd) res
  where
    res = dijkstra move (\ _ _ -> 1 :: Int) check x
