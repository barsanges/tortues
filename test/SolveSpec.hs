{- |
   Module      : SolveSpec
   Copyright   : Copyright (C) 2025 barsanges

Teste le module `Solve`.
-}

module SolveSpec ( spec ) where

import qualified Data.IntMap as I
import qualified Data.Set as S
import Test.Hspec
import Puzzle
import Solve

spec :: Spec
spec = do
  describe "test" $ do
    it "should return `True` if the puzzle may be solved (1)" $
      (fmap test $ mkPuzzle (I.fromList [(7, Blue)]) (S.fromList [])) `shouldBe` (Right True)

    it "should return `True` if the puzzle may be solved (2)" $
      (fmap test $ mkPuzzle (I.fromList [(4, Hare)]) (S.fromList [])) `shouldBe` (Right True)

    it "should return `True` if the puzzle may be solved (3)" $
      (fmap test $ mkPuzzle (I.fromList [(0, Red), (5, Purple)]) (S.fromList [F01, F34, F67])) `shouldBe` (Right True)

    it "should return `False` if the puzzle may not be solved (1)" $
      (fmap test $ mkPuzzle (I.fromList [(4, Hare)]) (S.fromList [F14, F34, F45, F47])) `shouldBe` (Right False)

    it "should return `False` if the puzzle may not be solved (2)" $
      (fmap test $ mkPuzzle (I.fromList [(0, Purple), (5, Red)]) (S.fromList [F01, F34, F67])) `shouldBe` (Right False)

    it "should return `True` for the 1st puzzle in the rules" $
      (fmap test $ mkPuzzle (I.fromList [(6, Blue), (7, Red)]) (S.fromList [F14, F34, F58, F78])) `shouldBe` (Right True)

    it "should return `True` for the 48th puzzle in the rules" $
      (fmap test $ mkPuzzle (I.fromList [(3, Purple), (4, Hare), (5, Blue), (6, Yellow), (7, Green), (8, Red)]) (S.fromList [F34, F45, F36, F78])) `shouldBe` (Right True)
