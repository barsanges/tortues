{- |
   Module      : ExploreSpec
   Copyright   : Copyright (C) 2025 barsanges

Teste le module `Explore`.
-}

module ExploreSpec ( spec ) where

import qualified Data.IntMap as I
import qualified Data.Map as M
import qualified Data.Set as S
import Test.Hspec
import Puzzle
import Explore

unsafeFromRight :: Either a b -> b
unsafeFromRight (Right x) = x
unsafeFromRight (Left _) = error "use unsafeFromRight at your own risk!"

spec :: Spec
spec = do
  describe "explore" $ do
    it "should return all possible configurations for a puzzle (1)" $
      (explore (\ _ _ -> False) $ mkSolvedPuzzle (S.fromList [Purple]) (Just F01) (Just F14) (Just F25) Nothing) `shouldBe` (M.fromList $ [ (unsafeFromRight $ mkPuzzle (I.fromList [(2, Purple)]) (S.fromList [F01, F14, F25]), 0)
                                                                                                                                          , (unsafeFromRight $ mkPuzzle (I.fromList [(1, Purple)]) (S.fromList [F01, F14, F25]), 1)])

    it "should return all possible configurations for a puzzle (2)" $
      (explore (\ _ _ -> False) $ mkSolvedPuzzle (S.fromList [Hare, Purple]) (Just F03) (Just F14) (Just F25) Nothing) `shouldBe` (M.fromList $ [ (unsafeFromRight $ mkPuzzle (I.fromList [(1, Hare), (2, Purple)]) (S.fromList [F03, F14, F25]), 0)
                                                                                                                                                , (unsafeFromRight $ mkPuzzle (I.fromList [(0, Hare), (2, Purple)]) (S.fromList [F03, F14, F25]), 1)
                                                                                                                                                , (unsafeFromRight $ mkPuzzle (I.fromList [(0, Hare), (1, Purple)]) (S.fromList [F03, F14, F25]), 2)
                                                                                                                                                , (unsafeFromRight $ mkPuzzle (I.fromList [(2, Hare), (1, Purple)]) (S.fromList [F03, F14, F25]), 3)
                                                                                                                                                , (unsafeFromRight $ mkPuzzle (I.fromList [(2, Hare), (0, Purple)]) (S.fromList [F03, F14, F25]), 4)
                                                                                                                                                , (unsafeFromRight $ mkPuzzle (I.fromList [(1, Hare), (0, Purple)]) (S.fromList [F03, F14, F25]), 5)])

    it "should return all possible configurations for a puzzle (3)" $
      (fmap (explore (\ _ _ -> False)) $ mkPuzzle (I.fromList [(1, Purple)]) (S.fromList [F01, F14, F25])) `shouldBe` (Right $ M.fromList [ (unsafeFromRight $ mkPuzzle (I.fromList [(1, Purple)]) (S.fromList [F01, F14, F25]), 0)
                                                                                                                                          , (unsafeFromRight $ mkPuzzle (I.fromList [(2, Purple)]) (S.fromList [F01, F14, F25]), 1)])

    it "should return all possible configurations for a puzzle up to a certain depth (1)" $
      (explore (\ i _ -> i >=3) $ mkSolvedPuzzle (S.fromList [Hare, Purple]) (Just F03) (Just F14) (Just F25) Nothing) `shouldBe` (M.fromList $ [ (unsafeFromRight $ mkPuzzle (I.fromList [(1, Hare), (2, Purple)]) (S.fromList [F03, F14, F25]), 0)
                                                                                                                                                , (unsafeFromRight $ mkPuzzle (I.fromList [(0, Hare), (2, Purple)]) (S.fromList [F03, F14, F25]), 1)
                                                                                                                                                , (unsafeFromRight $ mkPuzzle (I.fromList [(0, Hare), (1, Purple)]) (S.fromList [F03, F14, F25]), 2)
                                                                                                                                                , (unsafeFromRight $ mkPuzzle (I.fromList [(2, Hare), (1, Purple)]) (S.fromList [F03, F14, F25]), 3)])

    it "should return all possible configurations for a puzzle up to a certain depth (2)" $
      (explore (\ i _ -> i == 2) $ mkSolvedPuzzle (S.fromList [Hare, Purple]) (Just F03) (Just F14) (Just F25) Nothing) `shouldBe` (M.fromList $ [ (unsafeFromRight $ mkPuzzle (I.fromList [(1, Hare), (2, Purple)]) (S.fromList [F03, F14, F25]), 0)
                                                                                                                                                 , (unsafeFromRight $ mkPuzzle (I.fromList [(0, Hare), (2, Purple)]) (S.fromList [F03, F14, F25]), 1)
                                                                                                                                                 , (unsafeFromRight $ mkPuzzle (I.fromList [(0, Hare), (1, Purple)]) (S.fromList [F03, F14, F25]), 2)])

    it "should stop when a certain condition is met (1)" $
      (explore (\ _ x -> check x) $ mkSolvedPuzzle (S.fromList [Hare, Purple]) (Just F03) (Just F14) (Just F25) Nothing) `shouldBe` (M.fromList $ [(mkSolvedPuzzle (S.fromList [Hare, Purple]) (Just F03) (Just F14) (Just F25) Nothing, 0)])

    it "should stop when a certain condition is met (2)" $
      (fmap (explore (\ _ x -> check x)) $ mkPuzzle (I.fromList [(2, Hare), (1, Purple)]) (S.fromList [F03, F14, F25])) `shouldBe` (Right $ M.fromList $ [ (unsafeFromRight $ mkPuzzle (I.fromList [(2, Hare), (1, Purple)]) (S.fromList [F03, F14, F25]), 0)
                                                                                                                                                         , (unsafeFromRight $ mkPuzzle (I.fromList [(2, Hare), (0, Purple)]) (S.fromList [F03, F14, F25]), 1)
                                                                                                                                                         , (unsafeFromRight $ mkPuzzle (I.fromList [(0, Hare), (1, Purple)]) (S.fromList [F03, F14, F25]), 1)
                                                                                                                                                         , (unsafeFromRight $ mkPuzzle (I.fromList [(1, Hare), (0, Purple)]) (S.fromList [F03, F14, F25]), 2)
                                                                                                                                                         , (unsafeFromRight $ mkPuzzle (I.fromList [(0, Hare), (2, Purple)]) (S.fromList [F03, F14, F25]), 2)
                                                                                                                                                         , (unsafeFromRight $ mkPuzzle (I.fromList [(1, Hare), (2, Purple)]) (S.fromList [F03, F14, F25]), 3)])
