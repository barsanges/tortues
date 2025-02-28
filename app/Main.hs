{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Main
   Copyright   : Copyright (C) 2025 barsanges

Point d'entrée du programme.
-}

module Main where

import Combinatorics ( tuples )
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Options.Applicative
import Explore
import Puzzle

-- | Le type de sortie.
data Output = OutputFile String
            | StdOut

outputParser :: Parser Output
outputParser = ( OutputFile <$> strOption
                 ( long "output"
                   <> short 'o'
                   <> metavar "FOUT"
                   <> help "Write the result to the file 'FOUT' instead of sending it to stdout"
                 )
               ) <|> pure StdOut

-- | Le parser de la ligne de commande pour 'tortues'.
args :: ParserInfo Output
args = info (outputParser <**> helper)
       ( fullDesc
         <> header "tortues"
         <> progDesc "Generate solvable puzzles for 'tortues'" )

-- | Toutes les combinaisons d'animaux possibles (indépendamment de
-- leur placement).
allPossibleFigures :: [S.Set Figure]
allPossibleFigures = (S.toList . S.powerSet . S.fromList) [ Green, Hare,
                                                            Purple, Red,
                                                            Blue, Yellow ]

-- | Toutes les combinaisons de barrières possibles, de zéro à quatre
-- barrières.
allPossibleFences :: [(Maybe Fence, Maybe Fence, Maybe Fence, Maybe Fence)]
allPossibleFences = zero ++ ones ++ twos ++ threes ++ fours
  where
    fences = [ F01, F12, F34, F45, F67, F78, F03, F14, F25, F36, F47, F58 ]
    zero = [ (Nothing, Nothing, Nothing, Nothing) ]
    ones = [ (Just x1, Nothing, Nothing, Nothing) | (x1:_) <- tuples 1 fences ]
    twos = [ (Just x1, Just x2, Nothing, Nothing) | (x1:x2:_) <- tuples 2 fences ]
    threes = [ (Just x1, Just x2, Just x3, Nothing) | (x1:x2:x3:_) <- tuples 3 fences ]
    fours = [ (Just x1, Just x2, Just x3, Just x4) | (x1:x2:x3:x4:_) <- tuples 4 fences ]

-- | Un puzzle et le nombre de coups minimal qu'il faut pour le résoudre.
data Rated = Rated Int Puzzle

instance ToJSON Rated where
  toJSON = error "toJSON is not implemented for 'Rated'"

  toEncoding (Rated n p) = pairs ( "nmoves" .= n <> "puzzle" .= p )

-- | Renvoie la liste des ancêtres d'un puzzle, avec leurs notes.
ancestors :: Puzzle -> [Rated]
ancestors = (fmap (\ (p, n) -> Rated n p)) . M.assocs . (explore (\ _ _ -> False))

-- | Point d'entrée du programme.
main :: IO ()
main = do
  output <- execParser args
  let solved = [ mkSolvedPuzzle s mf1 mf2 mf3 mf4
               | s <- allPossibleFigures
               , (mf1, mf2, mf3, mf4) <- allPossibleFences
               ]
  let solvable = concatMap ancestors solved
  case output of
    OutputFile f -> encodeFile f solvable
    StdOut -> (BS.putStr . encode) solvable
