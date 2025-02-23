{- |
   Module      : SolveSpec
   Copyright   : Copyright (C) 2025 barsanges

Teste le module `Solve`.
-}

module SolveSpec ( spec ) where

import Data.Either ( rights )
import qualified Data.IntMap as I
import qualified Data.Set as S
import qualified Data.Sequence as Sq
import Test.Hspec
import Puzzle
import Solve

seqHead :: Sq.Seq a -> Maybe a
seqHead Sq.Empty = Nothing
seqHead (x Sq.:<| _) = Just x

seqTail :: Sq.Seq a -> Maybe a
seqTail Sq.Empty = Nothing
seqTail (_ Sq.:|> x) = Just x

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

  describe "solve" $ do
    -- TODO : on pourrait intégrer des propriétés à tester (vérifier
    -- que le premier élément est toujours le puzzle initial, que le
    -- dernier est toujours la solution, qu'on passe toujours de
    -- proche en proche).
    it "should return `Nothing` when the puzzle may not be solved" $
      (fmap solve $ mkPuzzle (I.fromList [(4, Yellow)]) (S.fromList [F58, F78])) `shouldBe` (Right Nothing)

    it "should return an optimal solution (1)" $
      (fmap solve $ mkPuzzle (I.fromList [(3, Green)]) (S.fromList [F01])) `shouldBe` (Right $ Just $ Sq.fromList $ rights [mkPuzzle (I.fromList [(3, Green)]) (S.fromList [F01]),
                                                                                                                            mkPuzzle (I.fromList [(0, Green)]) (S.fromList [F01])])

    it "should return an optimal solution (2)" $
      (fmap solve $ mkPuzzle (I.fromList [(6, Green)]) (S.fromList [F01, F34])) `shouldBe` (Right $ Just $ Sq.fromList $ rights [mkPuzzle (I.fromList [(6, Green)]) (S.fromList [F01, F34]),
                                                                                                                                 mkPuzzle (I.fromList [(3, Green)]) (S.fromList [F01, F34]),
                                                                                                                                 mkPuzzle (I.fromList [(0, Green)]) (S.fromList [F01, F34])])

    it "should return an optimal solution (3)" $
      (fmap (fmap length) $ fmap solve $ mkPuzzle (I.fromList [(4, Green)]) (S.fromList [])) `shouldBe` (Right $ Just 3)

    it "should return an optimal solution (4)" $
      (fmap solve $ mkPuzzle (I.fromList [(2, Hare), (1, Purple)]) (S.fromList [F03, F14, F25])) `shouldBe` (Right $ Just $ Sq.fromList $ rights [mkPuzzle (I.fromList [(2, Hare), (1, Purple)]) (S.fromList [F03, F14, F25]),
                                                                                                                                                  mkPuzzle (I.fromList [(0, Hare), (1, Purple)]) (S.fromList [F03, F14, F25]),
                                                                                                                                                  mkPuzzle (I.fromList [(0, Hare), (2, Purple)]) (S.fromList [F03, F14, F25]),
                                                                                                                                                  mkPuzzle (I.fromList [(1, Hare), (2, Purple)]) (S.fromList [F03, F14, F25])])

    it "should return a solution in 8 moves for the 1st puzzle in the rules" $
      (fmap (fmap length) $ fmap solve $ mkPuzzle (I.fromList [(6, Blue), (7, Red)]) (S.fromList [F14, F34, F58, F78])) `shouldBe` (Right $ Just 9)

    it "should return a solution in 26 moves for the 2nd puzzle in the rules"$
      (fmap (fmap length) $ fmap solve $ mkPuzzle (I.fromList [(7, Yellow), (8, Blue)]) (S.fromList [F14, F45, F47, F67])) `shouldBe` (Right $ Just 27)

    it "the starting point should the first element of the solution (1)"$
      (fmap ((=<<) seqHead) $ fmap solve $ mkPuzzle (I.fromList [(6, Blue), (7, Red)]) (S.fromList [F14, F34, F58, F78])) `shouldBe` (fmap Just $ mkPuzzle (I.fromList [(6, Blue), (7, Red)]) (S.fromList [F14, F34, F58, F78]))

    it "the starting point should the first element of the solution (2)"$
      (fmap ((=<<) seqHead) $ fmap solve $ mkPuzzle (I.fromList [(7, Yellow), (8, Blue)]) (S.fromList [F14, F45, F47, F67])) `shouldBe` (fmap Just $ mkPuzzle (I.fromList [(7, Yellow), (8, Blue)]) (S.fromList [F14, F45, F47, F67]))

    it "the solved puzzle should be the last element of the solution (1)"$
      (fmap ((=<<) seqTail) $ fmap solve $ mkPuzzle (I.fromList [(6, Blue), (7, Red)]) (S.fromList [F14, F34, F58, F78])) `shouldBe` (fmap Just $ mkPuzzle (I.fromList [(7, Blue), (6, Red)]) (S.fromList [F14, F34, F58, F78]))

    it "the solved puzzle should be the last element of the solution (2)"$
      (fmap ((=<<) seqTail) $ fmap solve $ mkPuzzle (I.fromList [(7, Yellow), (8, Blue)]) (S.fromList [F14, F45, F47, F67])) `shouldBe` (fmap Just $ mkPuzzle (I.fromList [(8, Yellow), (7, Blue)]) (S.fromList [F14, F45, F47, F67]))

    it "should return a solution in 66 moves for the 48th puzzle in the rules" $
      (fmap (fmap length) $ fmap solve $ mkPuzzle (I.fromList [(3, Purple), (4, Hare), (5, Blue), (6, Yellow), (7, Green), (8, Red)]) (S.fromList [F34, F45, F36, F78])) `shouldBe` (Right $ Just 67)

  describe "solve'" $ do
    -- TODO : même remarque que ci-dessus.
    it "should return `Nothing` when the puzzle may not be solved" $
      (fmap solve' $ mkPuzzle (I.fromList [(4, Yellow)]) (S.fromList [F58, F78])) `shouldBe` (Right Nothing)

    it "should return an optimal solution (1)" $
      (fmap solve' $ mkPuzzle (I.fromList [(3, Green)]) (S.fromList [F01])) `shouldBe` (Right $ Just $ Sq.fromList $ rights [mkPuzzle (I.fromList [(3, Green)]) (S.fromList [F01]),
                                                                                                                            mkPuzzle (I.fromList [(0, Green)]) (S.fromList [F01])])

    it "should return an optimal solution (2)" $
      (fmap solve' $ mkPuzzle (I.fromList [(6, Green)]) (S.fromList [F01, F34])) `shouldBe` (Right $ Just $ Sq.fromList $ rights [mkPuzzle (I.fromList [(6, Green)]) (S.fromList [F01, F34]),
                                                                                                                                 mkPuzzle (I.fromList [(3, Green)]) (S.fromList [F01, F34]),
                                                                                                                                 mkPuzzle (I.fromList [(0, Green)]) (S.fromList [F01, F34])])

    it "should return an optimal solution (3)" $
      (fmap (fmap length) $ fmap solve' $ mkPuzzle (I.fromList [(4, Green)]) (S.fromList [])) `shouldBe` (Right $ Just 3)

    it "should return an optimal solution (4)" $
      (fmap solve' $ mkPuzzle (I.fromList [(2, Hare), (1, Purple)]) (S.fromList [F03, F14, F25])) `shouldBe` (Right $ Just $ Sq.fromList $ rights [mkPuzzle (I.fromList [(2, Hare), (1, Purple)]) (S.fromList [F03, F14, F25]),
                                                                                                                                                  mkPuzzle (I.fromList [(0, Hare), (1, Purple)]) (S.fromList [F03, F14, F25]),
                                                                                                                                                  mkPuzzle (I.fromList [(0, Hare), (2, Purple)]) (S.fromList [F03, F14, F25]),
                                                                                                                                                  mkPuzzle (I.fromList [(1, Hare), (2, Purple)]) (S.fromList [F03, F14, F25])])

    it "should return a solution in 8 moves for the 1st puzzle in the rules" $
      (fmap (fmap length) $ fmap solve' $ mkPuzzle (I.fromList [(6, Blue), (7, Red)]) (S.fromList [F14, F34, F58, F78])) `shouldBe` (Right $ Just 9)

    it "should return a solution in 26 moves for the 2nd puzzle in the rules"$
      (fmap (fmap length) $ fmap solve' $ mkPuzzle (I.fromList [(7, Yellow), (8, Blue)]) (S.fromList [F14, F45, F47, F67])) `shouldBe` (Right $ Just 27)

    it "the starting point should the first element of the solution (1)"$
      (fmap ((=<<) seqHead) $ fmap solve' $ mkPuzzle (I.fromList [(6, Blue), (7, Red)]) (S.fromList [F14, F34, F58, F78])) `shouldBe` (fmap Just $ mkPuzzle (I.fromList [(6, Blue), (7, Red)]) (S.fromList [F14, F34, F58, F78]))

    it "the starting point should the first element of the solution (2)"$
      (fmap ((=<<) seqHead) $ fmap solve' $ mkPuzzle (I.fromList [(7, Yellow), (8, Blue)]) (S.fromList [F14, F45, F47, F67])) `shouldBe` (fmap Just $ mkPuzzle (I.fromList [(7, Yellow), (8, Blue)]) (S.fromList [F14, F45, F47, F67]))

    it "the solved puzzle should be the last element of the solution (1)"$
      (fmap ((=<<) seqTail) $ fmap solve' $ mkPuzzle (I.fromList [(6, Blue), (7, Red)]) (S.fromList [F14, F34, F58, F78])) `shouldBe` (fmap Just $ mkPuzzle (I.fromList [(7, Blue), (6, Red)]) (S.fromList [F14, F34, F58, F78]))

    it "the solved puzzle should be the last element of the solution (2)"$
      (fmap ((=<<) seqTail) $ fmap solve' $ mkPuzzle (I.fromList [(7, Yellow), (8, Blue)]) (S.fromList [F14, F45, F47, F67])) `shouldBe` (fmap Just $ mkPuzzle (I.fromList [(8, Yellow), (7, Blue)]) (S.fromList [F14, F45, F47, F67]))

    it "should return a solution in 66 moves for the 48th puzzle in the rules" $
      (fmap (fmap length) $ fmap solve' $ mkPuzzle (I.fromList [(3, Purple), (4, Hare), (5, Blue), (6, Yellow), (7, Green), (8, Red)]) (S.fromList [F34, F45, F36, F78])) `shouldBe` (Right $ Just 67)
