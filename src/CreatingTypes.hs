{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module CreatingTypes where

-- Type synonyms

type FirstName = String
type LastName = String
type Age = Int
type Height = Int

-- Tuples
type PatientName = (FirstName, LastName)

firstName :: PatientName -> FirstName
firstName patient = fst patient

lastName :: PatientName -> LastName
lastName patient = snd patient

showPatientInfo :: PatientName -> Age -> Height -> String
showPatientInfo (firstName, lastName) age height = name ++ " " ++ ageHeight
   where name = lastName ++ ", " ++ firstName
         ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "cms.)"

-- data constructors
-- Algebraic data types: https://wiki.haskell.org/Algebraic_data_type

-- SUM type
-- Type = DataConstructor or DataConstructor
data Sex = Male | Female

-- show function (without type classes)
showSex :: Sex -> String
showSex Male = "M"
showSex Female = "F"

data RhType = Positive | Negative
data ABOType = A | B | AB | O

-- PRODUCT type
-- Type = DataConstructor TypesToCombine ...
data BloodType = BloodType ABOType RhType

-- show functions (without type classes)
showRh :: RhType -> String
showRh Positive = "P"
showRh Negative = "N"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = "BloodType: " ++ showABO abo ++ " " ++ showRh rh

canDonate :: BloodType -> BloodType -> Bool
canDonate (BloodType O _ ) _ = True -- universal donor
canDonate _ (BloodType AB _ ) = True -- universal receiver
canDonate (BloodType A _ ) (BloodType A _ ) = True
canDonate (BloodType B _ ) (BloodType B _ ) = True
canDonate _ _ = False

type MiddleName = String
-- HYBRID : SUM & PRODUCT type
data Name = Name FirstName LastName | NameWithMiddle FirstName MiddleName LastName

showName :: Name -> String
showName (Name f l ) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

showPatientInfoV2 :: Name -> Sex -> Age -> Height -> BloodType -> String
showPatientInfoV2 name sex age height bloodType = (showName name) ++ " " ++ (showSex sex) ++ " " ++ ageHeight ++ " " ++ (showBloodType bloodType)
  where ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "cms.)"

data Patient = Patient Name Sex Age Height BloodType

showPatientInfoV3 :: Patient -> String
showPatientInfoV3 (Patient name sex age height bloodType) = showPatientInfoV2 name sex age height bloodType

-- Records
data PatientRecord = PatientRecord { name :: Name
                                   , sex :: Sex
                                   , age :: Int
                                   , height :: Int
                                   , bloodType :: BloodType
                                   }

showPatientInfoV4 :: PatientRecord -> String
showPatientInfoV4 record = showPatientInfoV2 (name record) (sex record) (age record) (height record) (bloodType record)

canDonateTo :: PatientRecord -> PatientRecord -> Bool
canDonateTo PatientRecord {bloodType=bt} PatientRecord {bloodType=bt2} = canDonate bt bt2
--canDonateTo patient1Record patient2Record = canDonate (bloodType patient1Record) (bloodType patient2Record) -- diff way

-- tiny types (one constructor with one field, indicates that the runtime representation of the value is identical to the underlying value)
newtype NickName = NickName String

showNickName :: NickName -> String
showNickName (NickName n) = n