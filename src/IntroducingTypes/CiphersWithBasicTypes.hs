module IntroducingTypes.CiphersWithBasicTypes where

-----------
-- ROT13 --
-----------
-- ROT13 cipher: Cryptographic method used by kids, rot stands for rotation and 13 refers to the number of the
-- letters you will rotate a given letter by (e.g 'a' would be encoded to 'n')

data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Eq, Show, Enum, Bounded)

data ThreeLetterAlphabet = Alpha | Beta | Kappa deriving (Eq, Show, Enum, Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize letter = toEnum encryptedLetter
 where halfAlphabet = alphabetSize `div` 2
       offset = fromEnum letter + halfAlphabet
       encryptedLetter = offset `mod` alphabetSize

rotChar :: Char -> Char
rotChar charToEncrypt = rotN sizeOfCharAlphabet charToEncrypt
  where sizeOfCharAlphabet = 1 + fromEnum ( maxBound :: Char )

fourLetterAlphabetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetEncoder lettersToEncode = map rot4 lettersToEncode
  where alphabetSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
        rot4 = (rotN alphabetSize)::FourLetterAlphabet->FourLetterAlphabet

threeLetterAlphabetEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterAlphabetEncoder lettersToEncode = map rot3 lettersToEncode
  where alphabetSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
        rot3 = (rotN alphabetSize)::ThreeLetterAlphabet->ThreeLetterAlphabet

rotNDecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNDecoder alphabetSize encodedLetter = toEnum decodedLetter
  where halfAlphabet = alphabetSize `div` 2
        offset = if even alphabetSize
                 then fromEnum encodedLetter + halfAlphabet
                 else fromEnum encodedLetter + halfAlphabet + 1
        decodedLetter = offset `mod` alphabetSize

threeLetterAlphabetDecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterAlphabetDecoder lettersToDecode = map rot3decoder lettersToDecode
  where alphabetSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
        rot3decoder = (rotNDecoder alphabetSize)

fourLetterAlphabetDecoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetDecoder lettersToDecode = map rot4decoder lettersToDecode
  where alphabetSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
        rot4decoder = (rotNDecoder alphabetSize)

rotEncoder :: String -> String
rotEncoder text = map rotChar text
  where alphabetSize = 1 + fromEnum (maxBound :: Char)
        rotChar = rotN alphabetSize

rotDecoder :: String -> String
rotDecoder text = map rotCharDecoder text
  where alphabetSize = 1 + fromEnum (maxBound :: Char)
        rotCharDecoder = rotNDecoder alphabetSize

xorBool :: Bool -> Bool -> Bool
xorBool True False = True
xorBool False True = True
xorBool _ _ = False

xor :: [Bool] -> [Bool] -> [Bool]
xor l1 l2 = map xorPair (zip l1 l2)
  where xorPair = (\(a, b) -> xorBool a b)




