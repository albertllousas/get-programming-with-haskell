module IntroducingTypes.CipherWithBasicTypesSpec where

import Test.Hspec
import IntroducingTypes.CiphersWithBasicTypes

spec :: Spec
spec = do
  describe "Rot ciphers" $ do
    it "shoud encode a letter for any alphabet" $ do
      map (rotN 4) [L1, L2, L3, L4] `shouldBe` [L3, L4, L1, L2]
      map (rotN 3) [Alpha, Beta, Kappa] `shouldBe` [Beta, Kappa, Alpha]
      map (rotN 2) [True, False] `shouldBe` [False, True]

    it "should encode a single char" $ do
      rotChar 'a' `shouldBe` '\557153'

    it "should encode a message in four-letter alphabet" $ do
      let message = [L1,L3,L4,L1,L1,L2]
      fourLetterAlphabetEncoder message `shouldBe` [L3,L1,L2,L3,L3,L4]

    it "should encode a message in three-letter alphabet" $ do
      let message = [Alpha, Beta, Kappa]
      threeLetterAlphabetEncoder message `shouldBe` [Beta, Kappa, Alpha]

    it "should decode a letter for any alphabet" $ do
      map (rotNDecoder 4) [L3, L4, L1, L2] `shouldBe` [L1, L2, L3, L4]
      map (rotNDecoder 3) [Beta, Kappa, Alpha] `shouldBe` [Alpha, Beta, Kappa]
      map (rotNDecoder 2) [False, True] `shouldBe` [True, False]

    it "should decode a message in four-letter alphabet" $ do
      let message = [L3,L1,L2,L3,L3,L4]
      fourLetterAlphabetDecoder message `shouldBe` [L1,L3,L4,L1,L1,L2]

    it "should decode a message in three-letter alphabet" $ do
      let message = [Beta, Kappa, Alpha]
      threeLetterAlphabetDecoder message `shouldBe` [Alpha, Beta, Kappa]

    it "should encode and decode a string message" $ do
      rotDecoder (rotEncoder "hi") `shouldBe` "hi"

    it "should encode and decode a string message using a cipher typeclass" $ do
      encode Rot (decode Rot "Haskell") `shouldBe` "Haskell"

  describe "XOR (exclusive or)" $ do
    it "should apply XOR function two Booleans" $ do
      xorBool True True `shouldBe` False
      xorBool False False `shouldBe` False
      xorBool False True `shouldBe` True
      xorBool True False `shouldBe` True

    it "should apply XOR function on two lists of Booleans" $ do
      xor [True, True, False, False] [False, True, False, False] `shouldBe` [True,False,False,False]

    it "should apply XOR to encode and decode with a secret key" $ do
      let secretKey = [False, True, False, False]
      xor (xor [True, True, False, False] secretKey) secretKey `shouldBe` [True, True, False, False]