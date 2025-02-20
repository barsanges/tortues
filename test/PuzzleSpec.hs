{- |
   Module      : PuzzleSpec
   Copyright   : Copyright (C) 2025 barsanges

Teste le module `Puzzle`.
-}

module PuzzleSpec ( spec ) where

import Data.Either ( isRight, rights )
import qualified Data.IntMap as I
import qualified Data.Set as S
import Test.Hspec
import Puzzle

unsafeFromRight :: Either a b -> b
unsafeFromRight (Right x) = x
unsafeFromRight (Left _) = error "use unsafeFromRight at your own risk!"

spec :: Spec
spec = do
  describe "mkPuzzle" $ do
    it "should fail if the figures are outside a 3x3 grid (1)" $
      (mkPuzzle (I.fromList [(-1, Blue)]) (S.fromList [])) `shouldBe` Left "the figures should be placed in a 3x3 grid"

    it "should fail if the figures are outside a 3x3 grid (2)" $
      (mkPuzzle (I.fromList [(9, Red)]) (S.fromList [])) `shouldBe` Left "the figures should be placed in a 3x3 grid"

    it "should fail if the same figure appears several times in the grid" $
      (mkPuzzle (I.fromList [(1, Yellow), (5, Yellow)]) (S.fromList [])) `shouldBe` Left "all figures should be different"

    it "should fail if there is more than 4 fences" $
      (mkPuzzle I.empty (S.fromList [F12, F45, F14, F03, F47])) `shouldBe` Left "there should be at most 4 fences"

    it "should give a result otherwise" $
      (mkPuzzle (I.fromList [(0, Hare)]) (S.fromList [F01, F03])) `shouldSatisfy` isRight

    it "should give a result otherwise, even if the grid is empty" $
      (mkPuzzle I.empty (S.fromList [F01, F36])) `shouldSatisfy` isRight

  describe "mkSolvedPuzzle" $ do
    it "should always return a completed puzzle (1)" $ do
      (Right $ mkSolvedPuzzle S.empty (Just F78) (Just F14) (Just F45) (Just F36)) `shouldBe` (mkPuzzle I.empty (S.fromList [F14, F36, F45, F78]))

    it "should always return a completed puzzle (2)" $ do
      (Right $ mkSolvedPuzzle (S.fromList [Green, Red]) (Just F01) Nothing (Just F03) Nothing) `shouldBe` (mkPuzzle (I.fromList [(0, Green), (6, Red)]) (S.fromList [F01, F03]))

    it "should always return a completed puzzle (3)" $ do
      (Right $ mkSolvedPuzzle (S.fromList [Green, Hare, Purple, Red, Blue, Yellow]) Nothing (Just F47) (Just F45) (Just F01)) `shouldBe` (mkPuzzle (I.fromList [(0, Green), (1, Hare), (2, Purple), (6, Red), (7, Blue), (8, Yellow)]) (S.fromList [F01, F45, F47]))

    it "should always return a completed puzzle (4)" $ do
      (check $ mkSolvedPuzzle (S.fromList [Blue, Hare, Purple]) Nothing Nothing Nothing Nothing) `shouldBe` True

    it "should always return a completed puzzle (5)" $ do
      (check $ mkSolvedPuzzle (S.fromList [Yellow, Hare, Green, Red]) (Just F14) (Just F25) (Just F36) (Just F47)) `shouldBe` True

  describe "Hash" $ do
    it "toPuzzle . fromPuzzle == id (1)" $
      (fmap (toPuzzle . fromPuzzle) $ mkPuzzle (I.fromList [(0, Hare)]) (S.fromList [F47, F03])) `shouldBe` (mkPuzzle (I.fromList [(0, Hare)]) (S.fromList [F47, F03]))

    it "toPuzzle . fromPuzzle == id (2)" $
      (fmap (toPuzzle . fromPuzzle) $ mkPuzzle I.empty (S.fromList [F01, F36])) `shouldBe` (mkPuzzle I.empty (S.fromList [F01, F36]))

    it "should give different identifiers to different puzzles" $
      (fmap fromPuzzle $ mkPuzzle (I.fromList [(0, Hare)]) (S.fromList [F01, F03])) `shouldNotBe` (fmap fromPuzzle $ mkPuzzle (I.fromList [(0, Green)]) (S.fromList [F01, F03]))

  describe "check" $ do
    it "should return True if the puzzle is solved (1)" $
      (fmap check $ mkPuzzle I.empty (S.fromList [])) `shouldBe` (Right True)

    it "should return True if the puzzle is solved (2)" $
      (fmap check $ mkPuzzle (I.fromList [(2, Purple), (6, Red)]) (S.fromList [])) `shouldBe` (Right True)

    it "should return False if the puzzle is not solved (1)" $
      (fmap check $ mkPuzzle (I.fromList [(2, Red), (6, Purple)]) (S.fromList [])) `shouldBe` (Right False)

    it "should return False if the puzzle is not solved (2)" $
      (fmap check $ mkPuzzle (I.fromList [(4, Hare)]) (S.fromList [])) `shouldBe` (Right False)

  describe "move" $ do
    it "should return all possible moves (1)" $
      (move $ unsafeFromRight $ mkPuzzle (I.fromList [(0, Hare)]) (S.fromList [F01, F03])) `shouldMatchList ` []

    it "should return all possible moves (2)" $
      (move $ unsafeFromRight $ mkPuzzle (I.fromList [(4, Green)]) (S.fromList [])) `shouldMatchList ` (rights [mkPuzzle (I.fromList [(1, Green)]) (S.fromList []),
                                                                                                                mkPuzzle (I.fromList [(3, Green)]) (S.fromList []),
                                                                                                                mkPuzzle (I.fromList [(5, Green)]) (S.fromList []),
                                                                                                                mkPuzzle (I.fromList [(7, Green)]) (S.fromList [])])

    it "should return all possible moves (3)" $
      (move $ unsafeFromRight $ mkPuzzle (I.fromList [(4, Green)]) (S.fromList [F14, F45])) `shouldMatchList ` (rights [mkPuzzle (I.fromList [(3, Green)]) (S.fromList [F14, F45]),
                                                                                                                        mkPuzzle (I.fromList [(7, Green)]) (S.fromList [F14, F45])])

    it "should return all possible moves (4)" $
      (move $ unsafeFromRight $ mkPuzzle (I.fromList [(6, Red), (7, Blue), (8, Yellow)]) (S.fromList [F47, F58])) `shouldMatchList ` (rights [mkPuzzle (I.fromList [(3, Red), (7, Blue), (8, Yellow)]) (S.fromList [F47, F58])])

    it "should return all possible moves (5)" $
      (move $ unsafeFromRight $ mkPuzzle (I.fromList [(0, Hare), (3, Purple)]) (S.fromList [F01, F34])) `shouldMatchList ` (rights [mkPuzzle (I.fromList [(6, Purple), (0, Hare)]) (S.fromList [F01, F34]),
                                                                                                                                    mkPuzzle (I.fromList [(6, Hare), (3, Purple)]) (S.fromList [F01, F34])])

    it "should return all possible moves (6)" $
      (move $ unsafeFromRight $ mkPuzzle (I.fromList [(2, Hare), (8, Purple)]) (S.fromList [F12, F45])) `shouldMatchList ` (rights [mkPuzzle (I.fromList [(2, Hare), (5, Purple)]) (S.fromList [F12, F45]),
                                                                                                                                    mkPuzzle (I.fromList [(5, Hare), (8, Purple)]) (S.fromList [F12, F45]),
                                                                                                                                    mkPuzzle (I.fromList [(7, Purple), (2, Hare)]) (S.fromList [F12, F45])])

    it "should return all possible moves (7)" $
      (move $ unsafeFromRight $ mkPuzzle (I.fromList [(3, Hare)]) (S.fromList [F45])) `shouldMatchList ` (rights [mkPuzzle (I.fromList [(0, Hare)]) (S.fromList [F45]),
                                                                                                                  mkPuzzle (I.fromList [(4, Hare)]) (S.fromList [F45]),
                                                                                                                  mkPuzzle (I.fromList [(6, Hare)]) (S.fromList [F45])])
