module Foundations.OOPWithClosuresSpec where

import Test.Hspec
import Foundations.OOPWithClosures

spec :: Spec
spec = do
  describe "a simple cup of a some drink" $ do
    it "should be able to get the milliliters that contains" $ do
      let cup = cupOf 100
      getMl cup `shouldBe` 100

    it "should be able to add more liquid to the cup" $ do
      let cup = cupOf 100
      let doubleCup = add 100 cup
      getMl doubleCup `shouldBe` 200

    it "should be able to drink from the cup" $ do
      let cup = cupOf 100
      let afterASip = drink 20 cup
      getMl afterASip `shouldBe` 80

    it "should be able to drink from the cup when we do more sips than liquid" $ do
      let cup = cupOf 100
      let afterASip = drink 120 cup
      getMl afterASip `shouldBe` 0

  describe "fighting robots" $ do
    it "should be able to get the attributes of a robot" $ do
      let killer = robot ("k1ll3r", 400, 500)
      getName killer `shouldBe` "k1ll3r"
      getAttack killer `shouldBe` 400
      getHp killer `shouldBe` 500

    it "should be able to set the attributes of a robot" $ do
      let killer = robot ("k1ll3r", 400, 500)
      let newKiller = setHp 200 (setAttack 100 (setName "killer" killer))
      getName newKiller `shouldBe` "killer"
      getAttack newKiller `shouldBe` 100
      getHp newKiller `shouldBe` 200

    it "should print a robot" $ do
      let killer = robot ("k1ll3r", 400, 500)
      printRobot killer `shouldBe` "robot(name:k1ll3r, attack:400, hp:500)"

    it "should damage a robot" $ do
      let killer = robot ("k1ll3r", 400, 500)
      let damagedKiller = damage killer 100
      getHp damagedKiller `shouldBe` 400

    it "should fight" $ do
      let killer = robot ("k1ll3r", 50, 500)
      let giant = robot ("giant", 100, 600)
      let killerAfterRound1 = fight giant killer
      let giantAfterRound1 = fight killerAfterRound1 giant
      let killerAfterRound2 = fight giantAfterRound1 killerAfterRound1
      let giantAfterRound2 = fight killerAfterRound2 giantAfterRound1
      getHp killerAfterRound2 `shouldBe` 300
      getHp giantAfterRound2 `shouldBe` 500