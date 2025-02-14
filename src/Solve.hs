{- |
   Module      : Solve
   Copyright   : Copyright (C) 2025 barsanges

Résout un puzzle du jeu "Le lièvre et les tortues".
-}

module Solve
  ( test
  ) where

import Data.Maybe ( isNothing )
import qualified Data.Set as S
import Puzzle

-- | Teste si un puzzle admet une solution.
test :: Puzzle -> Bool
test x = isNothing (test' [x] S.empty)

-- | Utilitaire de `test`.
test' :: [Puzzle] -> S.Set Puzzle -> Maybe (S.Set Puzzle)
test' [] s = Just s
test' (x:xs) s
  | x `S.member` s = Just s
  | check x = Nothing
  | otherwise = (test' (move x) (S.insert x s)) >>= (test' xs)
