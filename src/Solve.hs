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
solve x = fmap (fmap toPuzzle) $ snd $ solve' Empty x (M.empty, Nothing)

-- | Utilitaire de `solve`.
solve' :: Seq Hash
       -> Puzzle
       -> (M.Map Hash (Maybe (Seq Hash)), Maybe (Seq Hash))
       -> (M.Map Hash (Maybe (Seq Hash)), Maybe (Seq Hash))
solve' prev x (known, mbest) = case h `M.lookup` known of
  Just Nothing -> (known, mbest)
  Just (Just next) | maybeLower mn (Just $ p + 1 + (length next)) -> (known, mbest)
                   | otherwise -> (known, Just $ (prev :|> h) >< next)
  Nothing | maybeLower mn (Just p) -> (known, mbest)
          | h `elem` prev -> (known, mbest)
          | check x -> (M.insert h (Just Sq.empty) known, Just (prev :|> h)) -- FIXME : c'est là qu'il faudrait introduire un test avec mbest, pas dessous !
          | otherwise -> case mnew of
                          Nothing -> (known', mbest)
                          Just new | maybeLower mn (Just $ length new) -> (known', mbest)
                                   | otherwise -> (go (Sq.tails new) $ M.insert h Nothing known', Just new)
  where
    h = fromPuzzle x
    mn = fmap length mbest
    p = length prev
    (known', mnew) = foldr (solve' (prev :|> h)) (known, mbest) (move x)

    go :: Seq (Seq Hash)
       -> M.Map Hash (Maybe (Seq Hash))
       -> M.Map Hash (Maybe (Seq Hash))
    go Empty dict = dict
    go (Empty :<| _) dict = dict
    go ((u :<| us) :<| uss) dict = go uss (M.insert u (Just us) dict)

-- | Compare deux éléments incertains.
maybeLower :: Ord a => Maybe a -> Maybe a -> Bool
maybeLower Nothing Nothing = True
maybeLower (Just _) Nothing = True
maybeLower Nothing (Just _) = False
maybeLower (Just x) (Just y) = x <= y
