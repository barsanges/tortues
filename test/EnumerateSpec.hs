{- |
   Module      : EnumerateSpec
   Copyright   : Copyright (C) 2025 barsanges

Teste le module `Enumerate`.
-}

module EnumerateSpec ( spec ) where

import Test.Hspec
import Enumerate
import Puzzle

spec :: Spec
spec = do
  describe "figuresConfigurations" $ do
    it "should contain 57 elements" $
      (length figuresConfigurations) `shouldBe` (15 + 20 + 15 + 6 + 1)

  describe "nubSymmetric" $ do
    it "should do nothing if the list is empty" $
      (nubSymmetric []) `shouldBe` []

    it "should not change the content of the list if no elements are symmetric" $
      -- The order may change, however!
      (nubSymmetric [(Just F58, Nothing, Just F01, Nothing)]) `shouldBe` [(Just F58, Just F01, Nothing, Nothing)]

    it "should remove all symmetric elements (1)" $
      (nubSymmetric [(Nothing, Just F01, Just F34, Nothing),
                     (Nothing, Just F45, Nothing, Just F12)]) `shouldBe` [(Just F01, Just F34, Nothing, Nothing)]

    it "should remove all symmetric elements (2)" $
      (nubSymmetric [(Just F25, Just F12, Just F67, Just F36),
                     (Just F25, Just F12, Nothing, Just F03),
                     (Just F03, Nothing, Just F01, Just F25),
                     (Just F01, Just F58, Just F78, Just F03)]) `shouldBe` [(Just F25, Just F12, Just F67, Just F36),
                                                                            (Just F25, Just F12, Just F03, Nothing)]

    it "should remove only symmetric elements" $
      (nubSymmetric [(Just F03, Nothing, Nothing, Just F25),
                     (Nothing, Nothing, Nothing, Just F58),
                     (Just F47, Just F58, Just F36, Nothing),
                     (Just F36, Nothing, Nothing, Nothing),
                     (Just F14, Just F25, Just F03, Nothing)]) `shouldBe` [(Just F03, Just F25, Nothing, Nothing),
                                                                           (Just F58, Nothing, Nothing, Nothing),
                                                                           (Just F47, Just F58, Just F36, Nothing),
                                                                           (Just F14, Just F25, Just F03, Nothing)]

    it "should handle correctly the two fences in the middle column" $
      (nubSymmetric [(Just F01, Just F14, Just F47, Just F78),
                     (Just F12, Just F14, Just F47, Just F67)]) `shouldBe` [(Just F01, Just F14, Just F47, Just F78)]
