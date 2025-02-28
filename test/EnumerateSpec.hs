{- |
   Module      : EnumerateSpec
   Copyright   : Copyright (C) 2025 barsanges

Teste le module `Enumerate`.
-}

module EnumerateSpec ( spec ) where

import Test.Hspec
import Enumerate

spec :: Spec
spec = do
  describe "allPossibleFigures" $ do
    it "should contain 64 elements" $
      (length allPossibleFigures) `shouldBe` (1 + 6 + 15 + 20 + 15 + 6 + 1)

  describe "allPossibleFences" $ do
    it "should contain 794 elements" $
      (length allPossibleFences) `shouldBe` (1 + 12 + 66 + 220 + 495)
