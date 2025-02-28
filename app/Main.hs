{- |
   Module      : Main
   Copyright   : Copyright (C) 2025 barsanges

Point d'entrée du programme.
-}

module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Foldable ( maximumBy )
import qualified Data.Map.Strict as M
import Options.Applicative
import Enumerate
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

-- | Renvoie l'ancêtre le plus lointain d'un puzzle, avec sa note.
furthest :: Puzzle -> Rated
furthest = (\ (p, n) -> Rated n p) . (maximumBy (\ (p, _) (q, _) -> compare p q)) . M.assocs . (explore (\ _ _ -> False))

-- | Point d'entrée du programme.
main :: IO ()
main = do
  output <- execParser args
  let solved = [ mkSolvedPuzzle s mf1 mf2 mf3 mf4
               | s <- figuresConfigurations
               , (mf1, mf2, mf3, mf4) <- fencesConfigurations
               ]
  let solvable = fmap furthest solved
  case output of
    OutputFile f -> encodeFile f solvable
    StdOut -> (BS.putStr . encode) solvable
