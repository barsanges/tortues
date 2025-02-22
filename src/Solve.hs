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
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Sequence ( Seq(..), (><) )
import qualified Data.Sequence as Sq
import Puzzle

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
solve x = snd $ solve' Empty x (M.empty, Nothing)

-- | Utilitaire de `solve`.
solve' :: Seq Puzzle
       -> Puzzle
       -> (M.Map Puzzle (Maybe (Seq Puzzle)), Maybe (Seq Puzzle))
       -> (M.Map Puzzle (Maybe (Seq Puzzle)), Maybe (Seq Puzzle))
solve' prev x (known, mbest) = case x `M.lookup` known of
  Just Nothing -> (known, mbest)
  Just (Just next) | maybeLower mn (Just $ p + 1 + (length next)) -> (known, mbest)
                   | otherwise -> (known, Just $ (prev :|> x) >< next)
  Nothing | maybeLower mn (Just p) -> (known, mbest)
          | x `elem` prev -> (known, mbest)
          | check x -> (M.insert x (Just Sq.empty) known, Just (prev :|> x)) -- FIXME : c'est là qu'il faudrait introduire un test avec mbest, pas dessous !
          | otherwise -> case mnew of
                          Nothing -> (known', mbest)
                          Just new | maybeLower mn (Just $ length new) -> (known', mbest)
                                   | otherwise -> (go (Sq.tails new) $ M.insert x Nothing known', Just new)
  where
    mn = fmap length mbest
    p = length prev
    (known', mnew) = foldr (solve' (prev :|> x)) (known, mbest) (move x)

    go :: Seq (Seq Puzzle)
       -> M.Map Puzzle (Maybe (Seq Puzzle))
       -> M.Map Puzzle (Maybe (Seq Puzzle))
    go Empty dict = dict
    go (Empty :<| _) dict = dict
    go ((u :<| us) :<| uss) dict = go uss (M.insert u (Just us) dict)

-- | Compare deux éléments incertains.
maybeLower :: Ord a => Maybe a -> Maybe a -> Bool
maybeLower Nothing Nothing = True
maybeLower (Just _) Nothing = True
maybeLower Nothing (Just _) = False
maybeLower (Just x) (Just y) = x <= y
