{- |
   Module      : Puzzle
   Copyright   : Copyright (C) 2025 barsanges

Un puzzle du jeu "Le lièvre et les tortues".
-}

module Puzzle
  ( Figure(..)
  , Puzzle
  , mkPuzzle
  , check
  , move
  ) where

import Control.Monad ( forM )
import qualified Data.IntMap as I
import Data.List ( nub )
import Data.Maybe ( mapMaybe )
import qualified Data.Set as S
import qualified Data.Vector as V

-- | Un des animaux.
data Figure = Green | Hare | Purple | Red | Blue | Yellow
  deriving (Eq, Ord, Show)

-- | Un puzzle.
data Puzzle = Puzzle { figures_ :: I.IntMap Figure
                     , tortoiseMoves_ :: V.Vector [Int]
                     , hareMoves_ :: V.Vector [Int]
                     }
  deriving (Eq, Ord, Show)

-- | Mouvements autorisés pour les tortues pour chaque case du plateau, en
-- l'absence de barrières.
steps :: V.Vector [Int]
steps = V.fromListN 9 [   [1, 3],    [0, 2, 4],    [1, 5],
                       [0, 4, 6], [1, 3, 5, 7], [2, 4, 8],
                          [3, 7],    [4, 6, 8],    [5, 7]]

-- | Sauts autorisés pour le lièvre pour chaque case du plateau, en l'absence
-- de barrières.
jumps :: V.Vector [(Int, Int)]
jumps = V.fromListN 9 [[(1, 2), (3, 6)], [(4, 7)], [(1, 0), (5, 8)],
                               [(4, 5)],       [],         [(4, 3)],
                       [(3, 0), (7, 8)], [(4, 1)], [(7, 6), (5, 2)]]

-- | Positions autorisées pour les barrières.
admissibleFences :: S.Set (Int, Int)
admissibleFences = S.fromList [(0, 1), (1, 2), (3, 4), (4, 5), (6, 7), (7, 8),
                               (0, 3), (1, 4), (2, 5), (3, 6), (4, 7), (5, 8)]

-- | Trie les coordonnées d'une barrière.
orderFence :: Int -> Int -> (Int, Int)
orderFence i j = (min i j, max i j)

-- | Vérifie qu'une barrière est admissible.
checkFence :: (Int, Int) -> Either String (Int, Int)
checkFence (i, j) = if res `S.member` admissibleFences
                    then Right res
                    else Left "a fence should be set between two adjacent cells inside the grid"
  where
    res = orderFence i j

-- | Vérifie que le plateau (hors barrière) est admissible.
checkFigures :: I.IntMap Figure -> Either String (I.IntMap Figure)
checkFigures fs
  | any (\ i -> i < 0 || i > 8) (I.keys fs) =  Left "the figures should be placed in a 3x3 grid"
  | (length (nub (I.elems fs))) /= (length (I.elems fs)) = Left "all figures should be different"
  | otherwise = Right fs

-- | Filtre les mouvements `js` à partir de `i` lorsque ceux-ci sont
-- bloqués par une barrière. La fonction ne vérifie pas si les barrières
-- sont admissibles.
filterSteps :: S.Set (Int, Int) -> Int -> [Int] -> [Int]
filterSteps xs i js = filter (\ j -> (orderFence i j) `S.notMember` xs) js

-- | Filtre les sauts `jks` à partir de `i` lorsque ceux-ci sont bloqués
-- par une barrière. La fonction ne vérifie pas si les barrières sont
-- admissibles.
filterJumps :: S.Set (Int, Int) -> Int -> [(Int, Int)] -> [Int]
filterJumps xs i jks = fmap snd
                       $ filter (\ (j, k) -> ((orderFence i j) `S.notMember` xs)
                                             && ((orderFence j k) `S.notMember` xs))
                       jks

-- | Construit un puzzle.
mkPuzzle :: I.IntMap Figure -> [(Int, Int)] -> Either String Puzzle
mkPuzzle fs fences = do
  fs' <- checkFigures fs
  fences' <- forM fences checkFence
  let fences'' = S.fromList fences'
  if (S.size fences'') /= (length fences)
    then Left "all fences should be different"
    else if (S.size fences'') > 4
         then Left "There should be at most 4 fences"
         else let admissibleSteps = V.imap (filterSteps fences'') steps
                  admissibleJumps = V.imap (filterJumps fences'') jumps
              in Right Puzzle { figures_ = fs',
                                tortoiseMoves_ = admissibleSteps,
                                hareMoves_ = V.zipWith (++) admissibleSteps admissibleJumps }

-- | Vérifie si le puzzle est résolu.
check :: Puzzle -> Bool
check x = ((mgreen == Nothing) || (mgreen == Just Green))
          && ((mhare == Nothing) || (mhare == Just Hare))
          && ((mpurple == Nothing) || (mpurple == Just Purple))
          && (m3 == Nothing)
          && (m4 == Nothing)
          && (m5 == Nothing)
          && ((mred == Nothing) || (mred == Just Red))
          && ((mblue == Nothing) || (mblue == Just Blue))
          && ((myellow == Nothing) || (myellow == Just Yellow))
  where
    -- Pourrait être factorisé si on représentait les animaux par des entiers.
    mgreen = I.lookup 0 (figures_ x)
    mhare = I.lookup 1 (figures_ x)
    mpurple = I.lookup 2 (figures_ x)
    m3 = I.lookup 3 (figures_ x)
    m4 = I.lookup 4 (figures_ x)
    m5 = I.lookup 5 (figures_ x)
    mred = I.lookup 6 (figures_ x)
    mblue = I.lookup 7 (figures_ x)
    myellow = I.lookup 8 (figures_ x)

-- | Renvoie les coups possibles à partir d'une situation donnée.
move :: Puzzle -> [Puzzle]
move x = concatMap go1 [0..8]
  where
    fs = figures_ x

    go1 :: Int -> [Puzzle]
    go1 i = case I.lookup i fs of
      Nothing -> []
      Just Hare -> mapMaybe (go2 Hare i) ((hareMoves_ x) V.! i)
      Just t -> mapMaybe (go2 t i) ((tortoiseMoves_ x) V.! i)

    go2 :: Figure -> Int -> Int -> Maybe Puzzle
    go2 f i j = if (I.lookup j fs) == Nothing
                then Just (x { figures_ = (I.insert j f (I.delete i fs)) })
                else Nothing
