{- |
   Module      : PuzzleSpec
   Copyright   : Copyright (C) 2025 barsanges

Teste le module `Puzzle`.
-}

module PuzzleSpec ( spec ) where

import Data.Either ( isRight, rights )
import qualified Data.IntMap as I
import Test.Hspec
import Puzzle

unsafeFromRight :: Either a b -> b
unsafeFromRight (Right x) = x
unsafeFromRight (Left _) = error "use unsafeFromRight at your own risk!"

spec :: Spec
spec = do
  describe "mkPuzzle" $ do
    it "should fail if the figures are outside a 3x3 grid (1)" $
      (mkPuzzle (I.fromList [(-1, Blue)]) []) `shouldBe` Left "the figures should be placed in a 3x3 grid"

    it "should fail if the figures are outside a 3x3 grid (2)" $
      (mkPuzzle (I.fromList [(9, Red)]) []) `shouldBe` Left "the figures should be placed in a 3x3 grid"

    it "should fail if the same figure appears several times in the grid" $
      (mkPuzzle (I.fromList [(1, Yellow), (5, Yellow)]) []) `shouldBe` Left "all figures should be different"

    it "should fail if a fence is not set between adjacent cells" $
      (mkPuzzle I.empty [(0, 4)]) `shouldBe` Left "a fence should be set between two adjacent cells inside the grid"

    it "should fail if a fence is not set inside a 3x3 grid" $
      (mkPuzzle I.empty [(8, 9)]) `shouldBe` Left "a fence should be set between two adjacent cells inside the grid"

    it "should fail if the same fence is given several times" $
      (mkPuzzle I.empty [(3, 4), (3, 6), (3, 4)]) `shouldBe` Left "all fences should be different"

    it "should fail if there is more than 4 fences" $
      (mkPuzzle I.empty [(1, 2), (4, 5), (1, 4), (0, 3), (4, 7)]) `shouldBe` Left "there should be at most 4 fences"

    it "should give a result otherwise" $
      (mkPuzzle (I.fromList [(0, Hare)]) [(0, 1), (0, 3)]) `shouldSatisfy` isRight

    it "should give a result otherwise, even if the grid is empty" $
      (mkPuzzle I.empty [(0, 1), (3, 6)]) `shouldSatisfy` isRight

  describe "check" $ do
    it "should return True if the puzzle is solved (1)" $
      (fmap check $ mkPuzzle I.empty []) `shouldBe` (Right True)

    it "should return True if the puzzle is solved (2)" $
      (fmap check $ mkPuzzle (I.fromList [(2, Purple), (6, Red)]) []) `shouldBe` (Right True)

    it "should return False if the puzzle is not solved (1)" $
      (fmap check $ mkPuzzle (I.fromList [(2, Red), (6, Purple)]) []) `shouldBe` (Right False)

    it "should return False if the puzzle is not solved (2)" $
      (fmap check $ mkPuzzle (I.fromList [(4, Hare)]) []) `shouldBe` (Right False)

  describe "move" $ do
    it "should return all possible moves (1)" $
      (move $ unsafeFromRight $ mkPuzzle (I.fromList [(0, Hare)]) [(0, 1), (0, 3)]) `shouldMatchList ` []

    it "should return all possible moves (2)" $
      (move $ unsafeFromRight $ mkPuzzle (I.fromList [(4, Green)]) []) `shouldMatchList ` (rights [mkPuzzle (I.fromList [(1, Green)]) [],
                                                                                                    mkPuzzle (I.fromList [(3, Green)]) [],
                                                                                                    mkPuzzle (I.fromList [(5, Green)]) [],
                                                                                                    mkPuzzle (I.fromList [(7, Green)]) []])

    it "should return all possible moves (3)" $
      (move $ unsafeFromRight $ mkPuzzle (I.fromList [(4, Green)]) [(1, 4), (4, 5)]) `shouldMatchList ` (rights [mkPuzzle (I.fromList [(3, Green)]) [(1, 4), (4, 5)],
                                                                                                                  mkPuzzle (I.fromList [(7, Green)]) [(1, 4), (4, 5)]])

    it "should return all possible moves (4)" $
      (move $ unsafeFromRight $ mkPuzzle (I.fromList [(6, Red), (7, Blue), (8, Yellow)]) [(4, 7), (5, 8)]) `shouldMatchList ` (rights [mkPuzzle (I.fromList [(3, Red), (7, Blue), (8, Yellow)]) [(4, 7), (5, 8)]])

    it "should return all possible moves (5)" $
      (move $ unsafeFromRight $ mkPuzzle (I.fromList [(0, Hare), (3, Purple)]) [(0, 1), (3, 4)]) `shouldMatchList ` (rights [mkPuzzle (I.fromList [(6, Purple), (0, Hare)]) [(0, 1), (3, 4)],
                                                                                                                              mkPuzzle (I.fromList [(6, Hare), (3, Purple)]) [(0, 1), (3, 4)]])

    it "should return all possible moves (6)" $
      (move $ unsafeFromRight $ mkPuzzle (I.fromList [(2, Hare), (8, Purple)]) [(1, 2), (4, 5)]) `shouldMatchList ` (rights [mkPuzzle (I.fromList [(2, Hare), (5, Purple)]) [(1, 2), (4, 5)],
                                                                                                                              mkPuzzle (I.fromList [(5, Hare), (8, Purple)]) [(1, 2), (4, 5)],
                                                                                                                              mkPuzzle (I.fromList [(7, Purple), (2, Hare)]) [(1, 2), (4, 5)]])

    it "should return all possible moves (7)" $
      (move $ unsafeFromRight $ mkPuzzle (I.fromList [(3, Hare)]) [(4, 5)]) `shouldMatchList ` (rights [mkPuzzle (I.fromList [(0, Hare)]) [(4, 5)],
                                                                                                         mkPuzzle (I.fromList [(4, Hare)]) [(4, 5)],
                                                                                                         mkPuzzle (I.fromList [(6, Hare)]) [(4, 5)]])
