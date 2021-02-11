module CreatingTypesSpec where

import Test.Hspec
import CreatingTypes

spec :: Spec
spec = do
  describe "patient" $ do
    it "should get the first name from the patient name" $ do
      let patientName = ("John", "Doe") :: PatientName
      firstName patientName `shouldBe` "John"

    it "should get the last name from the patient name" $ do
      let patientName = ("John", "Doe") :: PatientName
      lastName patientName `shouldBe` "Doe"

    it "should show patient info" $ do
      let patientInfo = showPatientInfo ("John", "Doe") 40 170
      patientInfo `shouldBe` "Doe, John (40yrs. 170cms.)"

    it "should show the sex" $ do
      showSex Male `shouldBe` "M"
      showSex Female `shouldBe` "F"

    it "should show rh" $ do
      showRh Positive `shouldBe` "P"
      showRh Negative `shouldBe` "N"

    it "should show ABO" $ do
      showABO A `shouldBe` "A"
      showABO B `shouldBe` "B"
      showABO AB `shouldBe` "AB"
      showABO O `shouldBe` "O"

    it "should show blood type" $ do
      showBloodType (BloodType A Positive) `shouldBe` "BloodType: A P"

    it "should be able to determine if one blood type can donate to another" $ do
      let universalDonor = BloodType O Positive
      let universalReceiver = BloodType AB Positive
      let a = BloodType A Negative
      let b = BloodType B Negative
      canDonate universalDonor a `shouldBe` True
      canDonate a universalReceiver `shouldBe` True
      canDonate a a `shouldBe` True
      canDonate b b `shouldBe` True
      canDonate a b `shouldBe` False

    it "should a patient name" $ do
      showName (Name "John" "Doe") `shouldBe` "John Doe"
      showName (NameWithMiddle "John" "Jr." "Doe") `shouldBe` "John Jr. Doe"

    it "should show patient info v2" $ do
      let patientInfo = showPatientInfoV2 (Name "John" "Doe") Male 40 170 (BloodType O Positive)
      patientInfo `shouldBe` "John Doe M (40yrs. 170cms.) BloodType: O P"

    it "should show patient info v3" $ do
      let patientInfo = showPatientInfoV3 (Patient (Name "John" "Doe") Male 40 170 (BloodType O Positive))
      patientInfo `shouldBe` "John Doe M (40yrs. 170cms.) BloodType: O P"

    it "should show patient info v4" $ do
      let patientInfo = showPatientInfoV4 PatientRecord { name = Name "John" "Doe", sex = Male, age = 40 , height = 170 , bloodType = BloodType O Positive }
      patientInfo `shouldBe` "John Doe M (40yrs. 170cms.) BloodType: O P"

    it "should be able to determine if one patient can donate blood to another" $ do
      let patient1 = PatientRecord { name = Name "John" "Doe", sex = Male, age = 40 , height = 170 , bloodType = BloodType O Positive }
      let patient2 = patient1 { name = Name "Jane" "Doe", sex = Female }
      canDonateTo patient1 patient2 `shouldBe` True