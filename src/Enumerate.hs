{- |
   Module      : Enumerate
   Copyright   : Copyright (C) 2025 barsanges

Enumère des configurations initiales possibles.
-}

module Enumerate
  ( figuresConfigurations
  , fencesConfigurations
  , nubSymmetric
  ) where

import Combinatorics ( tuples )
import Data.List ( delete, nubBy )
import Data.Maybe ( catMaybes )
import qualified Data.Set as S
import Puzzle

-- | Toutes les combinaisons d'animaux possibles (indépendamment de
-- leur placement).
figuresConfigurations :: [S.Set Figure]
figuresConfigurations = go [ Green, Hare, Purple, Red, Blue, Yellow ]
  where
    go = S.toList . (S.filter (\ x -> S.size x > 1)) . S.powerSet . S.fromList

-- | Toutes les combinaisons de barrières possibles, avec trois ou
-- quatre barrières, en supprimant les occurrences symétriques (i.e. :
-- ne conserve que la première occurrence de chaque élément).
fencesConfigurations :: [(Maybe Fence, Maybe Fence, Maybe Fence, Maybe Fence)]
fencesConfigurations = nubSymmetric (threes ++ fours)
  where
    fences = [ F01, F12, F34, F45, F67, F78, F03, F14, F25, F36, F47, F58 ]
    threes = [ (Just x1, Just x2, Just x3, Nothing) | (x1:x2:x3:_) <- tuples 3 fences ]
    fours = [ (Just x1, Just x2, Just x3, Just x4) | (x1:x2:x3:x4:_) <- tuples 4 fences ]

-- | Supprime les jeux de barrière symétriques (i.e. : ne conserve que la
-- première occurrence de chaque élément).
nubSymmetric :: [(Maybe Fence, Maybe Fence, Maybe Fence, Maybe Fence)]
             -> [(Maybe Fence, Maybe Fence, Maybe Fence, Maybe Fence)]
nubSymmetric = (fmap og) . (nubBy isSymmetric) . (fmap go)
    where
        go :: (Maybe Fence, Maybe Fence, Maybe Fence, Maybe Fence) -> [Fence]
        go (mf1, mf2, mf3, mf4) = catMaybes [mf1, mf2, mf3, mf4]

        og :: [Fence] -> (Maybe Fence, Maybe Fence, Maybe Fence, Maybe Fence)
        og [] = (Nothing, Nothing, Nothing, Nothing)
        og (f1:[]) = (Just f1, Nothing, Nothing, Nothing)
        og (f1:f2:[]) = (Just f1, Just f2, Nothing, Nothing)
        og (f1:f2:f3:[]) = (Just f1, Just f2, Just f3, Nothing)
        og (f1:f2:f3:f4:[]) = (Just f1, Just f2, Just f3, Just f4)
        og _ = undefined -- Ne devrait pas arriver.

-- | Indique si deux jeux de barrière sont symétriques. On pourrait sans
-- doute faire plus malin pour parcourir `ys` moins de fois.
isSymmetric :: [Fence] -> [Fence] -> Bool
isSymmetric [] [] = True
isSymmetric [] _ = False
isSymmetric (x:xs) ys = case mx' of
  Just x' | x' `elem` ys -> True && (isSymmetric xs (delete x' ys))
          | otherwise -> False
  Nothing -> False
  where
    mx' = symmetric x

-- | Donne la barrière symétrique d'une barrière donnée.
symmetric :: Fence -> Maybe Fence
symmetric F01 = Just F12
symmetric F12 = Just F01
symmetric F34 = Just F45
symmetric F45 = Just F34
symmetric F67 = Just F78
symmetric F78 = Just F67
symmetric F03 = Just F25
symmetric F25 = Just F03
symmetric F36 = Just F58
symmetric F58 = Just F36
symmetric _ = Nothing
