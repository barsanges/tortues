{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Puzzle
   Copyright   : Copyright (C) 2025 barsanges

Un puzzle du jeu "Le lièvre et les tortues".
-}

module Puzzle
  ( Figure(..)
  , Fence(..)
  , Puzzle
  , Rated(..)
  , mkPuzzle
  , mkSolvedPuzzle
  , convertToSolved
  , check
  , move
  ) where

import Data.Aeson
import qualified Data.IntMap as I
import Data.List ( nub )
import qualified Data.Map as M
import Data.Maybe ( catMaybes, mapMaybe )
import qualified Data.Set as S
import qualified Data.Vector as V

-- | Un des animaux.
data Figure = Green | Hare | Purple | Red | Blue | Yellow
  deriving (Eq, Ord, Show)

-- | Une barrière sur le plateau. Les deux chiffres du constructeur
-- indiquent les deux cases que la barrière sépare : ainsi, `F01`
-- représente une barrière à la frontière entre la case 0 (case de la
-- tortue verte) et la case 1 (case du lièvre).
data Fence = F01 | F12 | F34 | F45 | F67 | F78
           | F03 | F14 | F25 | F36 | F47 | F58
  deriving (Eq, Ord, Show)

instance ToJSON Fence where
  toJSON = error "toJSON is not implemented for 'Fence'"

  toEncoding F01 = toEncoding ([0, 1] :: [Int])
  toEncoding F12 = toEncoding ([1, 2] :: [Int])
  toEncoding F34 = toEncoding ([3, 4] :: [Int])
  toEncoding F45 = toEncoding ([4, 5] :: [Int])
  toEncoding F67 = toEncoding ([6, 7] :: [Int])
  toEncoding F78 = toEncoding ([7, 8] :: [Int])
  toEncoding F03 = toEncoding ([0, 3] :: [Int])
  toEncoding F14 = toEncoding ([1, 4] :: [Int])
  toEncoding F25 = toEncoding ([2, 5] :: [Int])
  toEncoding F36 = toEncoding ([3, 6] :: [Int])
  toEncoding F47 = toEncoding ([4, 7] :: [Int])
  toEncoding F58 = toEncoding ([5, 8] :: [Int])

-- | L'identifiant unique d'un puzzle.
data Hash = H {-# UNPACK #-} !Int
  deriving (Eq, Ord, Show)

-- | Un puzzle.
data Puzzle = Puzzle { figures_ :: I.IntMap Figure
                     , fences_ :: S.Set Fence
                     , tortoiseMoves_ :: V.Vector [Int]
                     , hareMoves_ :: V.Vector [Int]
                     , hash_ :: Hash
                     }
  deriving Show

instance Eq Puzzle where
  x == y = (hash_ x) == (hash_ y)

instance Ord Puzzle where
  compare x y = compare (hash_ x) (hash_ y)

-- | Un puzzle et un score.
data Rated = Rated Int Puzzle

instance ToJSON Rated where
  toJSON = error "toJSON is not implemented for 'Rated'"

  toEncoding (Rated n p) = pairs ( "score" .= n
                                   <> "figures" .= fs
                                   <> "fences" .= (S.toList . fences_) p
                                 )
    where
      fs = I.foldlWithKey' go M.empty (figures_ p)
      go :: M.Map String Int -> Int -> Figure -> M.Map String Int
      go d i Green = M.insert "green" i d
      go d i Hare = M.insert "hare" i d
      go d i Purple = M.insert "purple" i d
      go d i Red = M.insert "red" i d
      go d i Blue = M.insert "blue" i d
      go d i Yellow = M.insert "yellow" i d

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

-- | Transforme un objet `Fence` en un tuple d'entiers.
fenceToTuple :: Fence -> (Int, Int)
fenceToTuple F01 = (0, 1)
fenceToTuple F12 = (1, 2)
fenceToTuple F34 = (3, 4)
fenceToTuple F45 = (4, 5)
fenceToTuple F67 = (6, 7)
fenceToTuple F78 = (7, 8)
fenceToTuple F03 = (0, 3)
fenceToTuple F14 = (1, 4)
fenceToTuple F25 = (2, 5)
fenceToTuple F36 = (3, 6)
fenceToTuple F47 = (4, 7)
fenceToTuple F58 = (5, 8)

-- | Trie les coordonnées d'une barrière.
orderFence :: Int -> Int -> (Int, Int)
orderFence i j = (min i j, max i j)

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

-- | Calcule le hash d'un puzzle.
mkHash :: I.IntMap Figure -> S.Set Fence -> Hash
mkHash figures fences = H (fencesAsInt + figuresAsInt)
  where
    fenceToInt :: Int -> (Int, [Fence]) -> (Int, [Fence])
    fenceToInt _ (n, []) = (n, [])
    fenceToInt i (n, f:fs) = (n + (go1 f) * 10^i, fs)

    fencesAsInt = fst $ foldr fenceToInt (0, S.toList fences) [15, 13, 11, 9]

    go1 :: Fence -> Int
    go1 F01 = 1
    go1 F12 = 2
    go1 F34 = 3
    go1 F45 = 4
    go1 F67 = 5
    go1 F78 = 6
    go1 F03 = 7
    go1 F14 = 8
    go1 F25 = 9
    go1 F36 = 10
    go1 F47 = 11
    go1 F58 = 12

    figureToInt :: (Int, Figure) -> Int -> Int
    figureToInt (i, f) n = n + (go2 f) * 10^(8 - i)

    figuresAsInt = foldr figureToInt 0 (I.assocs figures)

    go2 :: Figure -> Int
    go2 Green = 1
    go2 Hare = 2
    go2 Purple = 3
    go2 Red = 4
    go2 Blue = 5
    go2 Yellow = 6

-- | Construit un puzzle sans vérifier que les arguments sont valides.
unsafeMkPuzzle :: I.IntMap Figure -> S.Set Fence -> Puzzle
unsafeMkPuzzle fs fences = Puzzle { figures_ = fs,
                                    fences_ = fences,
                                    tortoiseMoves_ = admissibleSteps,
                                    hareMoves_ = V.zipWith (++) admissibleSteps admissibleJumps,
                                    hash_ = mkHash fs fences }
  where
    fences' = S.map fenceToTuple fences
    admissibleSteps = V.imap (filterSteps fences') steps
    admissibleJumps = V.imap (filterJumps fences') jumps

-- | Construit un puzzle.
mkPuzzle :: I.IntMap Figure -> S.Set Fence -> Either String Puzzle
mkPuzzle fs fences = do
  fs' <- checkFigures fs
  if (S.size fences) > 4
    then Left "there should be at most 4 fences"
    else Right $ unsafeMkPuzzle fs' fences

-- | Construit un puzzle résolu.
mkSolvedPuzzle :: S.Set Figure
               -> Maybe Fence
               -> Maybe Fence
               -> Maybe Fence
               -> Maybe Fence
               -> Puzzle
mkSolvedPuzzle fs mf1 mf2 mf3 mf4 = unsafeMkPuzzle fs' fences
  where
    fs' = foldr (\ x dict -> I.insert (go x) x dict) I.empty fs
    fences = (S.fromList . catMaybes) [mf1, mf2, mf3, mf4]
    go :: Figure -> Int
    go Green = 0
    go Hare = 1
    go Purple = 2
    go Red = 6
    go Blue = 7
    go Yellow = 8

-- | Renvoie le puzzle résolu correspondant au puzzle fourni.
convertToSolved :: Puzzle -> Puzzle
convertToSolved x = x { figures_ = figs, hash_ = mkHash figs (fences_ x) }
  where
    figs = (I.fromList . (fmap go) . (I.elems . figures_)) x
    go :: Figure -> (Int, Figure)
    go Green = (0, Green)
    go Hare = (1, Hare)
    go Purple = (2, Purple)
    go Red = (6, Red)
    go Blue = (7, Blue)
    go Yellow = (8, Yellow)

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
                then let figs = (I.insert j f (I.delete i fs))
                     in Just (x { figures_ = figs
                                , hash_ = mkHash figs (fences_ x) })
                else Nothing
