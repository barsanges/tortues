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
  describe "figuresConfigurations" $ do
    it "should contain 57 elements" $
      (length figuresConfigurations) `shouldBe` (15 + 20 + 15 + 6 + 1)

  describe "fencesConfigurations" $ do
    it "should contain 715 elements" $
      (length fencesConfigurations) `shouldBe` (220 + 495)
