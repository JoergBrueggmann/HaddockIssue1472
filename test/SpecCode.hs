{-|
Description : test.
Copyright   : (c) Jörg K.-H. W. Brüggmann, 2021-2022
License     : proprietary, to be dual licensed
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SpecCode
    (
        testGroup
    ) where

import qualified Test.Tasty as T
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit as HU

import qualified SpecCodeTestHelper as TH

import qualified Data.Bits as Bts hiding ((.|.), (.&.))
import           Data.Bits ((.|.), (.&.))
import qualified Data.Char as Chr
import qualified List as Lst
import qualified Safer as Sfr

import qualified Octets as Oct

import qualified Code as Cd

--------------------------------------------------------------------------------
-- test group hirachy

{-  * validated: ✅
        * documented: ✅
        * completeness: ✅ -}
testGroup :: T.TestTree
testGroup =
    T.testGroup
        "module Code"
        [
            tgClassCode,
            tgClassSaferEnum, 
            tgUnitroctCI1, 
            tgUnitroctCW1
        ]

{-  * validated: ✅
        * documented: ✅
          * laws             : ✅
          * characteristics  : ✅
          * prefix           : ✅
          * short description: ✅
        * completeness: ✅
            * all laws documented          : ✅
            * all type combinations        : ✅
            * stubbed for default functions: ✅ -}
tgClassCode :: T.TestTree
tgClassCode =
    T.testGroup
        "class Code"
        [
            -- laws
            tgLawRange, -- Law #1
            tgLawNulIsNul, -- Law #2
            tgLawNulIsNormalised, -- Law #3
            tgLawCodesNULNormalised, -- Law #4
            tgLawOrdinalNumberInRangeIfValid, -- Law #5
            tgLawOrdinalNumberMinus1IfNotValid, -- Law #6
            tgLawReplacementIsValid, -- Law #7
            tgLawNulIsValid, -- Law #8
            tgLawAlwaysNormalisedCodeFromInteger, -- Law #9
            tgLawReplacementIsAlwaysNormalised, -- Law #10
            tgLawNormalisedCodeIsEqualNormalisedNormalisedCode, -- Law #11
            tgLawNormalisedCodeIsAlwaysValid, -- Law #12
            -- unit tests
            tgUnitcdNul,
            tgUnitisNul,
            tgUnitisValid,
            tgUnitcdReplacement,
            tgUnitniOrd,
            tgUnitcdFromInteger,
            tgUnitniMinOrd,
            tgUnitniMaxOrd,
            tgUnitcdNormalise,
            tgUnitisNormalised,
            tgUnitchFromCode,
            tgUnitcdFromChar
        ]

{-  * validated: ✅
        NOTE: All that refers to instances of class Safer.Enum, in particular toEnum and fromEnum. Test of functions that depend on toEnum and fromEnum are out of scope.
        * documented: ✅
          * laws             : ✅
          * characteristics  : ✅
          * prefix           : ✅
          * short description: ✅
        * completeness: ✅
            * all laws documented          : ✅
            * all type combinations        : ✅
            * stubbed for default functions: ✅ -}
tgClassSaferEnum :: T.TestTree
tgClassSaferEnum =
    T.testGroup
        "class Safer.Enum"
        [
            -- laws:
            -- unit tests:
            tgUnittoEnum,
            tgUnitfromEnum
        ]

{-  * validated: ✅
        * completeness: ✅
        * independence: ✅
        * edge cases  : ✅
        * conform doc.: ✅ -}
tgUnittoEnum :: T.TestTree
tgUnittoEnum = 
    T.testGroup
        "toEnum"
        (
            QC.testProperty "Stub #p1" (\(nj' :: TH.Int256Plus ) -> True QC.==> (Sfr.toEnum (TH.niFromInt256Plus nj') :: TH.Stub) == Cd.cdFromInteger (TH.niFromInt256Plus nj') ) : 
            tgUnittoEnumUnitTests "Stub #u" (Cd.Px :: Cd.Px TH.Stub) lnjEdgeCasesForStub ++ 
            QC.testProperty "Char #p1" (\(nj' :: TH.IntCharPlus ) -> True QC.==> (Sfr.toEnum (TH.niFromIntCharPlus nj') :: Char) == Cd.cdFromInteger (TH.niFromIntCharPlus nj') ) :
            tgUnittoEnumUnitTests "Char #u" (Cd.Px :: Cd.Px Char) lnjEdgeCasesForChar ++ 
            QC.testProperty "CharUtf8 #p1" (\(nj' :: TH.IntCharPlus ) -> True QC.==> (Sfr.toEnum (TH.niFromIntCharPlus nj') :: Cd.CharUtf8) == Cd.cdFromInteger (TH.niFromIntCharPlus nj') ) :
            tgUnittoEnumUnitTests "CharUtf8 #u" (Cd.Px :: Cd.Px Cd.CharUtf8) lnjEdgeCasesForChar ++ 
            QC.testProperty "CharIso1 #p1" (\(nj' :: TH.Int256Plus ) -> True QC.==> (Sfr.toEnum (TH.niFromInt256Plus nj') :: Cd.CharIso1) == Cd.cdFromInteger (TH.niFromInt256Plus nj') ) :
            tgUnittoEnumUnitTests "CharIso1 #u" (Cd.Px :: Cd.Px Cd.CharIso1) lnjEdgeCasesForOctets ++ 
            QC.testProperty "CharWin1 #p1" (\(nj' :: TH.Int256Plus ) -> True QC.==> (Sfr.toEnum (TH.niFromInt256Plus nj') :: Cd.CharWin1) == Cd.cdFromInteger (TH.niFromInt256Plus nj') ) :
            tgUnittoEnumUnitTests "CharWin1 #u" (Cd.Px :: Cd.Px Cd.CharWin1) lnjEdgeCasesForOctets ++ 
            QC.testProperty "Octets.Octet #p1" (\(nj' :: TH.Int256Plus ) -> True QC.==> (Sfr.toEnum (TH.niFromInt256Plus nj') :: Oct.Octet) == Cd.cdFromInteger (TH.niFromInt256Plus nj') ) :
            tgUnittoEnumUnitTests "Octets.Octet #u" (Cd.Px :: Cd.Px Oct.Octet) lnjEdgeCasesForOctets ++ 
            QC.testProperty "Bool #p1" (\(nj' :: TH.Int256Plus ) -> True QC.==> (Sfr.toEnum (TH.niFromInt256Plus nj') :: Bool) == Cd.cdFromInteger (TH.niFromInt256Plus nj') ) :
            tgUnittoEnumUnitTests "Bool #u" (Cd.Px :: Cd.Px Bool) lnjEdgeCasesForOctets
        )

tgUnittoEnumUnitTests :: forall a. (Eq a, Show a, Cd.Code a, Sfr.Enum a) => String -> Cd.Px a -> [Int] -> [T.TestTree]
tgUnittoEnumUnitTests sTestName _ lni = 
    fmap 
        (\tpl -> 
            HU.testCase (sTestName ++ show (snd tpl)) ((Sfr.toEnum (fromIntegral (fst tpl)) :: a) HU.@?= (Cd.cdFromInteger (fromIntegral (fst tpl)) :: a)))
        (zip lni [1,2..])

{-  * validated: ✅
        * completeness: ✅
        * independence: ✅
        * edge cases  : ✅
        * conform doc.: ✅ -}
tgUnitfromEnum :: T.TestTree
tgUnitfromEnum =
    T.testGroup
        "fromEnum"
        (
            QC.testProperty "Stub #p1" (\(cd :: TH.Stub) -> True QC.==> Sfr.fromEnum cd == Cd.niOrd cd ) :
            tgUnitfromEnumUnitTests "Stub #u" lstbEdgeCases ++
            QC.testProperty "Char #p1" (\(cd :: Char) -> True QC.==> Sfr.fromEnum cd == Cd.niOrd cd ) : 
            tgUnitfromEnumUnitTests "Char #u" lchEdgeCases ++
            QC.testProperty "CharUtf8 #p1" (\(cd :: Cd.CharUtf8) -> True QC.==> Sfr.fromEnum cd == Cd.niOrd cd ) : 
            tgUnitfromEnumUnitTests "CharUtf8 #u" lcu8EdgeCases ++
            QC.testProperty "CharIso1 #p1" (\(cd :: Cd.CharIso1) -> True QC.==> Sfr.fromEnum cd == Cd.niOrd cd ) : 
            tgUnitfromEnumUnitTests "CharIso1 #u" lci1EdgeCases ++
            QC.testProperty "CharWin1 #p1" (\(cd :: Cd.CharWin1) -> True QC.==> Sfr.fromEnum cd == Cd.niOrd cd ) : 
            tgUnitfromEnumUnitTests "CharWin1 #u" lcw1EdgeCases ++
            QC.testProperty "Octets.Octet #p1" (\(cd :: Oct.Octet) -> True QC.==> Sfr.fromEnum cd == Cd.niOrd cd ) : 
            tgUnitfromEnumUnitTests "Octet #u" loctEdgeCases ++
            QC.testProperty "Bool #p1" (\(cd :: Bool) -> True QC.==> Sfr.fromEnum cd == Cd.niOrd cd ) : 
            tgUnitfromEnumUnitTests "Bool #u" lisEdgeCases
        )

tgUnitfromEnumUnitTests :: (Cd.Code a, Show a, Sfr.Enum a) => String -> [a] -> [T.TestTree]
tgUnitfromEnumUnitTests sTestName lx = 
    fmap 
        (\(tpl :: a) -> 
            HU.testCase (sTestName ++ show (snd tpl)) (Sfr.fromEnum (fst tpl) HU.@?= Cd.niOrd (fst tpl)))
        (zip lx [1,2..])

{-  * validated: ✅
        * completeness: ✅
        * independence: ✅
    	    * NOTE: Lst.areAllOrdUnique is validated to be under test.
        * edge cases  : ✅
        * conform doc.: ✅ -}
tgLawRange :: T.TestTree
tgLawRange =
    T.testGroup
        "Law: Range"
        [
            tgCodePropertyLawRange "Stub" (Cd.Px :: Cd.Px TH.Stub),
            tgCodePropertyLawRange "Char" (Cd.Px :: Cd.Px Char),
            tgCodePropertyLawRange "CharUtf8" (Cd.Px :: Cd.Px Cd.CharUtf8),
            tgCodePropertyLawRange "CharIso1" (Cd.Px :: Cd.Px Cd.CharIso1),
            tgCodePropertyLawRange "CharWin1" (Cd.Px :: Cd.Px Cd.CharWin1),
            tgCodePropertyLawRange "Octets.Octet" (Cd.Px :: Cd.Px Oct.Octet),
            tgCodePropertyLawRange "Bool" (Cd.Px :: Cd.Px Bool)
        ]

tgCodePropertyLawRange :: forall a. (Cd.Code a, Show a, Ord a) => String -> Cd.Px a -> T.TestTree
tgCodePropertyLawRange sTypeName px =
    T.testGroup
        sTypeName
        [
            QC.testProperty "#p1" (\nj -> (nj < Cd.niMinOrd px) QC.==> (Cd.cdFromInteger nj == (Cd.cdReplacement :: Bool)) ),
            QC.testProperty "#p2" (\nj -> (nj > 0) QC.==> Cd.cdFromInteger (nj + Cd.niMaxOrd (Cd.Px :: Cd.Px Bool)) == (Cd.cdReplacement :: Bool)),
            HU.testCase "#u1" (Cd.cdFromInteger (-2) HU.@?= (Cd.cdReplacement :: a)),
            HU.testCase "#u2" (Cd.cdFromInteger (-1) HU.@?= (Cd.cdReplacement :: a)),
            HU.testCase "#u3" (Cd.cdFromInteger (Cd.niMaxOrd px + 1) HU.@?= (Cd.cdReplacement :: a)),
            HU.testCase "#u4" (Cd.cdFromInteger (Cd.niMaxOrd px + 2) HU.@?= (Cd.cdReplacement :: a)),
            HU.testCase "#u5" (Lst.areAllOrdUnique (lcdNormalisedSetInRange px) HU.@?= True),
            HU.testCase "#u6" (Sfr.niCount Cd.cdReplacement (lcdNormalisedSetInRange px) HU.@?= 1)
        ]

lcdNormalisedSetInRange :: Cd.Code cd => Cd.Px cd -> [cd]
lcdNormalisedSetInRange px = [ Cd.cdFromInteger ni | ni <- niCodeRange px ]

niCodeRange :: Cd.Code cd => Cd.Px cd -> [Integer]
niCodeRange px = [Cd.niMinOrd px..Cd.niMaxOrd px]

{-  * validated: ✅
        * completeness: ✅
        * independence: ✅
        * edge cases  : N/A
        * conform doc.: ✅ -}
tgLawNulIsNul :: T.TestTree
tgLawNulIsNul =
    T.testGroup
        "Law: NUL is NUL"
        [
            HU.testCase "Stub #u1" (Cd.isNul (Cd.cdNul :: TH.Stub) HU.@?= True),
            HU.testCase "Char #u1" (Cd.isNul (Cd.cdNul :: Char) HU.@?= True),
            HU.testCase "CharUtf8 #u1" (Cd.isNul (Cd.cdNul :: Cd.CharUtf8) HU.@?= True),
            HU.testCase "CharIso1 #u1" (Cd.isNul (Cd.cdNul :: Cd.CharIso1) HU.@?= True),
            HU.testCase "CharWin1 #u1" (Cd.isNul (Cd.cdNul :: Cd.CharWin1) HU.@?= True),
            HU.testCase "Octets.Octet #u1" (Cd.isNul (Cd.cdNul :: Oct.Octet) HU.@?= True),
            HU.testCase "Bool #u1" (Cd.isNul (Cd.cdNul :: Bool) HU.@?= True)
        ]

{-  * validated: ✅
        * completeness: ✅
        * independence: ✅
        * edge cases  : N/A
        * conform doc.: ✅ -}
tgLawNulIsNormalised :: T.TestTree
tgLawNulIsNormalised =
    T.testGroup
        "Law: NUL is normalised"
        [
            HU.testCase "Stub #u1" (Cd.cdNormalise (Cd.cdNul :: TH.Stub) HU.@?= Cd.cdNul),
            HU.testCase "Char #u1" (Cd.cdNormalise (Cd.cdNul :: Char) HU.@?= Cd.cdNul),
            HU.testCase "CharUtf8 #u1" (Cd.cdNormalise (Cd.cdNul :: Cd.CharUtf8) HU.@?= Cd.cdNul),
            HU.testCase "CharIso1 #u1" (Cd.cdNormalise (Cd.cdNul :: Cd.CharIso1) HU.@?= Cd.cdNul),
            HU.testCase "CharWin1 #u1" (Cd.cdNormalise (Cd.cdNul :: Cd.CharWin1) HU.@?= Cd.cdNul),
            HU.testCase "Octets.Octet #u1" (Cd.cdNormalise (Cd.cdNul :: Oct.Octet) HU.@?= Cd.cdNul),
            HU.testCase "Bool #u1" (Cd.cdNormalise (Cd.cdNul :: Bool) HU.@?= Cd.cdNul)
        ]

{-  * validated: ✅
        * completeness: ✅
        * independence: ✅
        * edge cases  : N/A
        * conform doc.: ✅ -}
tgLawCodesNULNormalised :: T.TestTree
tgLawCodesNULNormalised =
    T.testGroup
        "Law: codes that are tested NUL are also tested NUL if normalised"
        [
            QC.testProperty "Stub #p1" (\cd -> Cd.isNul (cd :: TH.Stub) QC.==> Cd.isNul (Cd.cdNormalise cd) ),
            HU.testCase     "Stub #u1" (Cd.isNul (Cd.cdNul :: TH.Stub) && Cd.isNul (Cd.cdNormalise (Cd.cdNul :: TH.Stub)) HU.@?= True),
            HU.testCase     "Stub #u2" (Cd.isNul (TH.Stub 25) && Cd.isNul (Cd.cdNormalise (TH.Stub 25)) HU.@?= True),
            HU.testCase     "Stub #u3" (Cd.isNul (TH.Stub 75) && Cd.isNul (Cd.cdNormalise (TH.Stub 75)) HU.@?= True),
            QC.testProperty "Char #p1" (\cd' -> Cd.isNul (TH.fromT' (cd' :: (TH.T' Char))) QC.==> Cd.isNul (Cd.cdNormalise (TH.fromT' cd')) ),
            HU.testCase     "Char #u1" (Cd.isNul (Cd.cdNul :: Char) && Cd.isNul (Cd.cdNormalise (Cd.cdNul :: Char)) HU.@?= True),
            QC.testProperty "CharUtf8 #p1" (\cd -> Cd.isNul (cd :: Cd.CharUtf8) QC.==> Cd.isNul (Cd.cdNormalise cd) ),
            HU.testCase     "CharUtf8 #u1" (Cd.isNul (Cd.cdNul :: Cd.CharUtf8) && Cd.isNul (Cd.cdNormalise (Cd.cdNul :: Cd.CharUtf8)) HU.@?= True),
            HU.testCase     "CharUtf8 #u2" (Cd.isNul (Cd.CU8TwoBytes 0b11000000 0b10000000) && Cd.isNul (Cd.cdNormalise (Cd.CU8TwoBytes 0b11000000 0b10000000)) HU.@?= True),
            HU.testCase     "CharUtf8 #u3" (Cd.isNul (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b10000000) && Cd.isNul (Cd.cdNormalise (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b10000000)) HU.@?= True),
            HU.testCase     "CharUtf8 #u4" (Cd.isNul (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b10000000) && Cd.isNul (Cd.cdNormalise (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b10000000)) HU.@?= True),
            HU.testCase     "CharUtf8 #u5" (Cd.isNul (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b10000000) && Cd.isNul (Cd.cdNormalise (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b10000000)) HU.@?= True),
            QC.testProperty "CharIso1 #p1" (\cd -> Cd.isNul (cd :: Cd.CharIso1) QC.==> Cd.isNul (Cd.cdNormalise cd)),
            HU.testCase     "CharIso1 #u1" (Cd.isNul (Cd.cdNul :: Cd.CharIso1) && Cd.isNul (Cd.cdNormalise (Cd.cdNul :: Cd.CharIso1)) HU.@?= True),
            HU.testCase     "CharIso1 #u2" (Cd.isNul (Cd.CharIso1 0x00) && Cd.isNul (Cd.cdNormalise (Cd.CharIso1 0x00)) HU.@?= True),
            QC.testProperty "CharWin1 #p1" (\cd -> Cd.isNul (cd :: Cd.CharWin1) QC.==> Cd.isNul (Cd.cdNormalise cd) ),
            HU.testCase     "CharWin1 #u1" (Cd.isNul (Cd.cdNul :: Cd.CharWin1) && Cd.isNul (Cd.cdNormalise (Cd.cdNul :: Cd.CharWin1)) HU.@?= True),
            HU.testCase     "CharWin1 #u2" (Cd.isNul (Cd.CharWin1 0x00) && Cd.isNul (Cd.cdNormalise (Cd.CharWin1 0x00)) HU.@?= True),
            QC.testProperty "Octets.Octet #p1" (\cd' -> Cd.isNul (TH.fromT' (cd' :: (TH.T' Oct.Octet))) QC.==> Cd.isNul (Cd.cdNormalise (TH.fromT' cd')) ),
            HU.testCase     "Octets.Octet #u1" (Cd.isNul (Cd.cdNul :: Oct.Octet) && Cd.isNul (Cd.cdNormalise (Cd.cdNul :: Oct.Octet)) HU.@?= True),
            HU.testCase     "Octets.Octet #u2" (Cd.isNul (0x00 :: Oct.Octet) && Cd.isNul (Cd.cdNormalise (0x00 :: Oct.Octet)) HU.@?= True),
            QC.testProperty "Bool #p1" (\cd -> Cd.isNul (cd :: Bool) QC.==> Cd.isNul (Cd.cdNormalise cd) ),
            HU.testCase     "Bool #u1" (Cd.isNul (Cd.cdNul :: Bool) && Cd.isNul (Cd.cdNormalise (Cd.cdNul :: Bool)) HU.@?= True),
            HU.testCase     "Bool #u2" (Cd.isNul False && Cd.isNul (Cd.cdNormalise False) HU.@?= True)
        ]

{-  * validated: ✅
        * completeness: ✅
        * independence: ✅
        * edge cases  : ✅
        * conform doc.: ✅ -}
tgLawOrdinalNumberInRangeIfValid :: T.TestTree
tgLawOrdinalNumberInRangeIfValid =
    T.testGroup
        "Law: ordinal number in range if valid"
        [
            QC.testProperty "Stub #p1" (\cd -> Cd.isValid (cd :: TH.Stub) QC.==> (Cd.niOrd cd >= Cd.niMinOrd (Cd.Px :: Cd.Px TH.Stub) && Cd.niOrd cd <= Cd.niMaxOrd (Cd.Px :: Cd.Px TH.Stub)) ),
            HU.testCase     "Stub #u1" ((Cd.niOrd (TH.Stub 0) >= Cd.niMinOrd (Cd.Px :: Cd.Px TH.Stub) && Cd.niOrd (TH.Stub 0) <= Cd.niMaxOrd (Cd.Px :: Cd.Px TH.Stub)) HU.@?= True),
            HU.testCase     "Stub #u2" ((Cd.niOrd (TH.Stub 1) >= Cd.niMinOrd (Cd.Px :: Cd.Px TH.Stub) && Cd.niOrd (TH.Stub 1) <= Cd.niMaxOrd (Cd.Px :: Cd.Px TH.Stub)) HU.@?= True),
            HU.testCase     "Stub #u3" ((Cd.niOrd (TH.Stub 2) >= Cd.niMinOrd (Cd.Px :: Cd.Px TH.Stub) && Cd.niOrd (TH.Stub 2) <= Cd.niMaxOrd (Cd.Px :: Cd.Px TH.Stub)) HU.@?= True),
            HU.testCase     "Stub #u4" ((Cd.niOrd (TH.Stub 25) >= Cd.niMinOrd (Cd.Px :: Cd.Px TH.Stub) && Cd.niOrd (TH.Stub 25) <= Cd.niMaxOrd (Cd.Px :: Cd.Px TH.Stub)) HU.@?= True),
            HU.testCase     "Stub #u5" ((Cd.niOrd (TH.Stub 31) >= Cd.niMinOrd (Cd.Px :: Cd.Px TH.Stub) && Cd.niOrd (TH.Stub 31) <= Cd.niMaxOrd (Cd.Px :: Cd.Px TH.Stub)) HU.@?= True),
            HU.testCase     "Stub #u6" ((Cd.niOrd (TH.Stub 75) >= Cd.niMinOrd (Cd.Px :: Cd.Px TH.Stub) && Cd.niOrd (TH.Stub 75) <= Cd.niMaxOrd (Cd.Px :: Cd.Px TH.Stub)) HU.@?= True),
            HU.testCase     "Stub #u7" ((Cd.niOrd (TH.Stub 98) >= Cd.niMinOrd (Cd.Px :: Cd.Px TH.Stub) && Cd.niOrd (TH.Stub 98) <= Cd.niMaxOrd (Cd.Px :: Cd.Px TH.Stub)) HU.@?= True),
            HU.testCase     "Stub #u8" ((Cd.niOrd (TH.Stub 99) >= Cd.niMinOrd (Cd.Px :: Cd.Px TH.Stub) && Cd.niOrd (TH.Stub 99) <= Cd.niMaxOrd (Cd.Px :: Cd.Px TH.Stub)) HU.@?= True),
            QC.testProperty "Char #p1" (\cd' -> Cd.isValid (TH.fromT' (cd' :: (TH.T' Char))) QC.==> (Cd.niOrd (TH.fromT' cd') >= Cd.niMinOrd (Cd.Px :: Cd.Px Char) && Cd.niOrd (TH.fromT' cd') <= Cd.niMaxOrd (Cd.Px :: Cd.Px Char)) ),
            HU.testCase     "Char #u1" ((Cd.niOrd '\x0' >= Cd.niMinOrd (Cd.Px :: Cd.Px Char) && Cd.niOrd '\x0' <= Cd.niMaxOrd (Cd.Px :: Cd.Px Char)) HU.@?= True),
            HU.testCase     "Char #u2" ((Cd.niOrd '\x1' >= Cd.niMinOrd (Cd.Px :: Cd.Px Char) && Cd.niOrd '\x1' <= Cd.niMaxOrd (Cd.Px :: Cd.Px Char)) HU.@?= True),
            HU.testCase     "Char #u3" ((Cd.niOrd '\x2' >= Cd.niMinOrd (Cd.Px :: Cd.Px Char) && Cd.niOrd '\x2' <= Cd.niMaxOrd (Cd.Px :: Cd.Px Char)) HU.@?= True),
            HU.testCase     "Char #u4" ((Cd.niOrd 'A' >= Cd.niMinOrd (Cd.Px :: Cd.Px Char) && Cd.niOrd 'A' <= Cd.niMaxOrd (Cd.Px :: Cd.Px Char)) HU.@?= True),
            HU.testCase     "Char #u5" ((Cd.niOrd 'B' >= Cd.niMinOrd (Cd.Px :: Cd.Px Char) && Cd.niOrd 'B' <= Cd.niMaxOrd (Cd.Px :: Cd.Px Char)) HU.@?= True),
            HU.testCase     "Char #u6" ((Cd.niOrd '\x10FFFD' >= Cd.niMinOrd (Cd.Px :: Cd.Px Char) && Cd.niOrd '\x10FFFD' <= Cd.niMaxOrd (Cd.Px :: Cd.Px Char)) HU.@?= True),
            HU.testCase     "Char #u7" ((Cd.niOrd '\x10FFFE' >= Cd.niMinOrd (Cd.Px :: Cd.Px Char) && Cd.niOrd '\x10FFFE' <= Cd.niMaxOrd (Cd.Px :: Cd.Px Char)) HU.@?= True),
            HU.testCase     "Char #u8" ((Cd.niOrd '\x10FFFF' >= Cd.niMinOrd (Cd.Px :: Cd.Px Char) && Cd.niOrd '\x10FFFF' <= Cd.niMaxOrd (Cd.Px :: Cd.Px Char)) HU.@?= True),
            QC.testProperty "CharUtf8 #p1" (\cd -> Cd.isValid (cd :: Cd.CharUtf8) QC.==> (Cd.niOrd cd >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharUtf8) && Cd.niOrd cd <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharUtf8)) ),
            HU.testCase     "CharUtf8 #u1" ((Cd.niOrd (Cd.CU8OneByte 0x00) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharUtf8) && Cd.niOrd (Cd.CU8OneByte 0x00) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharUtf8)) HU.@?= True),
            HU.testCase     "CharUtf8 #u2" ((Cd.niOrd (Cd.CU8OneByte 0x01) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharUtf8) && Cd.niOrd (Cd.CU8OneByte 0x01) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharUtf8)) HU.@?= True),
            HU.testCase     "CharUtf8 #u3" ((Cd.niOrd (Cd.CU8OneByte 0x02) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharUtf8) && Cd.niOrd (Cd.CU8OneByte 0x02) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharUtf8)) HU.@?= True),
            HU.testCase     "CharUtf8 #u4" ((Cd.niOrd (Cd.CU8OneByte 0x7E) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharUtf8) && Cd.niOrd (Cd.CU8OneByte 0x7E) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharUtf8)) HU.@?= True),
            HU.testCase     "CharUtf8 #u5" ((Cd.niOrd (Cd.CU8OneByte 0x7F) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharUtf8) && Cd.niOrd (Cd.CU8OneByte 0x7F) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharUtf8)) HU.@?= True),
            HU.testCase     "CharUtf8 #u6" ((Cd.niOrd (Cd.CU8TwoBytes 0b11000000 0b10000000) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharUtf8) && Cd.niOrd (Cd.CU8TwoBytes 0b11000000 0b10000000) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharUtf8)) HU.@?= True),
            HU.testCase     "CharUtf8 #u7" ((Cd.niOrd (Cd.CU8TwoBytes 0b11000000 0b10000001) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharUtf8) && Cd.niOrd (Cd.CU8TwoBytes 0b11000000 0b10000001) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharUtf8)) HU.@?= True),
            HU.testCase     "CharUtf8 #u8" ((Cd.niOrd (Cd.CU8TwoBytes 0b11011111 0b10111110) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharUtf8) && Cd.niOrd (Cd.CU8TwoBytes 0b11011111 0b10111110) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharUtf8)) HU.@?= True),
            HU.testCase     "CharUtf8 #u9" ((Cd.niOrd (Cd.CU8TwoBytes 0b11011111 0b10111111) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharUtf8) && Cd.niOrd (Cd.CU8TwoBytes 0b11011111 0b10111111) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharUtf8)) HU.@?= True),
            HU.testCase    "CharUtf8 #u10" ((Cd.niOrd (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b10000000) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharUtf8) && Cd.niOrd (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b10000000) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharUtf8)) HU.@?= True),
            HU.testCase    "CharUtf8 #u11" ((Cd.niOrd (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b10000001) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharUtf8) && Cd.niOrd (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b10000001) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharUtf8)) HU.@?= True),
            HU.testCase    "CharUtf8 #u12" ((Cd.niOrd (Cd.CU8ThreeBytes 0b11101111 0b10111111 0b10111110) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharUtf8) && Cd.niOrd (Cd.CU8ThreeBytes 0b11101111 0b10111111 0b10111111) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharUtf8)) HU.@?= True),
            HU.testCase    "CharUtf8 #u13" ((Cd.niOrd (Cd.CU8ThreeBytes 0b11101111 0b10111111 0b10111111) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharUtf8) && Cd.niOrd (Cd.CU8ThreeBytes 0b11101111 0b10111111 0b10111111) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharUtf8)) HU.@?= True),
            HU.testCase    "CharUtf8 #u14" ((Cd.niOrd (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b10000000) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharUtf8) && Cd.niOrd (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b10000000) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharUtf8)) HU.@?= True),
            HU.testCase    "CharUtf8 #u15" ((Cd.niOrd (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b10000001) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharUtf8) && Cd.niOrd (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b10000001) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharUtf8)) HU.@?= True),
            HU.testCase    "CharUtf8 #u16" ((Cd.niOrd (Cd.CU8FourBytes 0b11110100 0b10001111 0b10111111 0b10111110) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharUtf8) && Cd.niOrd (Cd.CU8FourBytes 0b11110100 0b10001111 0b10111111 0b10111110) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharUtf8)) HU.@?= True),
            HU.testCase    "CharUtf8 #u17" ((Cd.niOrd (Cd.CU8FourBytes 0b11110100 0b10001111 0b10111111 0b10111111) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharUtf8) && Cd.niOrd (Cd.CU8FourBytes 0b11110100 0b10001111 0b10111111 0b10111111) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharUtf8)) HU.@?= True),
            QC.testProperty "CharIso1 #p1" (\cd -> Cd.isValid (cd :: Cd.CharIso1) QC.==> (Cd.niOrd cd >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharIso1) && Cd.niOrd cd <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharIso1)) ),
            HU.testCase     "CharIso1 #u1" ((Cd.niOrd (Cd.CharIso1 0x00) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharIso1) && Cd.niOrd (Cd.CharIso1 0x00) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharIso1)) HU.@?= True),
            HU.testCase     "CharIso1 #u2" ((Cd.niOrd (Cd.CharIso1 0x01) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharIso1) && Cd.niOrd (Cd.CharIso1 0x01) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharIso1)) HU.@?= True),
            HU.testCase     "CharIso1 #u3" ((Cd.niOrd (Cd.CharIso1 0x02) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharIso1) && Cd.niOrd (Cd.CharIso1 0x02) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharIso1)) HU.@?= True),
            HU.testCase     "CharIso1 #u4" ((Cd.niOrd (Cd.CharIso1 0x7E) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharIso1) && Cd.niOrd (Cd.CharIso1 0x7E) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharIso1)) HU.@?= True),
            HU.testCase     "CharIso1 #u5" ((Cd.niOrd (Cd.CharIso1 0x7F) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharIso1) && Cd.niOrd (Cd.CharIso1 0x7F) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharIso1)) HU.@?= True),
            HU.testCase     "CharIso1 #u6" ((Cd.niOrd (Cd.CharIso1 0xA0) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharIso1) && Cd.niOrd (Cd.CharIso1 0xA0) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharIso1)) HU.@?= True),
            HU.testCase     "CharIso1 #u7" ((Cd.niOrd (Cd.CharIso1 0xA1) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharIso1) && Cd.niOrd (Cd.CharIso1 0xA1) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharIso1)) HU.@?= True),
            HU.testCase     "CharIso1 #u8" ((Cd.niOrd (Cd.CharIso1 0xFE) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharIso1) && Cd.niOrd (Cd.CharIso1 0xFE) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharIso1)) HU.@?= True),
            HU.testCase     "CharIso1 #u9" ((Cd.niOrd (Cd.CharIso1 0xFF) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharIso1) && Cd.niOrd (Cd.CharIso1 0xFF) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharIso1)) HU.@?= True),
            QC.testProperty "CharWin1 #p1" (\cd -> Cd.isValid (cd :: Cd.CharWin1) QC.==> (Cd.niOrd cd >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharWin1) && Cd.niOrd cd <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharWin1)) ),
            HU.testCase     "CharWin1 #u1" ((Cd.niOrd (Cd.CharWin1 0x00) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharWin1) && Cd.niOrd (Cd.CharWin1 0x00) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharWin1)) HU.@?= True),
            HU.testCase     "CharWin1 #u2" ((Cd.niOrd (Cd.CharWin1 0x01) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharWin1) && Cd.niOrd (Cd.CharWin1 0x01) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharWin1)) HU.@?= True),
            HU.testCase     "CharWin1 #u3" ((Cd.niOrd (Cd.CharWin1 0x02) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharWin1) && Cd.niOrd (Cd.CharWin1 0x02) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharWin1)) HU.@?= True),
            HU.testCase     "CharWin1 #u4" ((Cd.niOrd (Cd.CharWin1 0x7F) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharWin1) && Cd.niOrd (Cd.CharWin1 0x7F) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharWin1)) HU.@?= True),
            HU.testCase     "CharWin1 #u5" ((Cd.niOrd (Cd.CharWin1 0x80) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharWin1) && Cd.niOrd (Cd.CharWin1 0x80) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharWin1)) HU.@?= True),
            HU.testCase     "CharWin1 #u6" ((Cd.niOrd (Cd.CharWin1 0x82) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharWin1) && Cd.niOrd (Cd.CharWin1 0x82) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharWin1)) HU.@?= True),
            HU.testCase     "CharWin1 #u7" ((Cd.niOrd (Cd.CharWin1 0x83) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharWin1) && Cd.niOrd (Cd.CharWin1 0x83) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharWin1)) HU.@?= True),
            HU.testCase     "CharWin1 #u8" ((Cd.niOrd (Cd.CharWin1 0x8B) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharWin1) && Cd.niOrd (Cd.CharWin1 0x8B) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharWin1)) HU.@?= True),
            HU.testCase     "CharWin1 #u9" ((Cd.niOrd (Cd.CharWin1 0x8C) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharWin1) && Cd.niOrd (Cd.CharWin1 0x8C) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharWin1)) HU.@?= True),
            HU.testCase    "CharWin1 #u10" ((Cd.niOrd (Cd.CharWin1 0x8E) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharWin1) && Cd.niOrd (Cd.CharWin1 0x8E) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharWin1)) HU.@?= True),
            HU.testCase    "CharWin1 #u11" ((Cd.niOrd (Cd.CharWin1 0x91) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharWin1) && Cd.niOrd (Cd.CharWin1 0x91) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharWin1)) HU.@?= True),
            HU.testCase    "CharWin1 #u12" ((Cd.niOrd (Cd.CharWin1 0x91) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharWin1) && Cd.niOrd (Cd.CharWin1 0x91) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharWin1)) HU.@?= True),
            HU.testCase    "CharWin1 #u13" ((Cd.niOrd (Cd.CharWin1 0x92) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharWin1) && Cd.niOrd (Cd.CharWin1 0x92) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharWin1)) HU.@?= True),
            HU.testCase    "CharWin1 #u14" ((Cd.niOrd (Cd.CharWin1 0x9B) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharWin1) && Cd.niOrd (Cd.CharWin1 0x9B) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharWin1)) HU.@?= True),
            HU.testCase    "CharWin1 #u15" ((Cd.niOrd (Cd.CharWin1 0x9C) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharWin1) && Cd.niOrd (Cd.CharWin1 0x9C) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharWin1)) HU.@?= True),
            HU.testCase    "CharWin1 #u16" ((Cd.niOrd (Cd.CharWin1 0x9E) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharWin1) && Cd.niOrd (Cd.CharWin1 0x9E) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharWin1)) HU.@?= True),
            HU.testCase    "CharWin1 #u17" ((Cd.niOrd (Cd.CharWin1 0x9F) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharWin1) && Cd.niOrd (Cd.CharWin1 0x9F) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharWin1)) HU.@?= True),
            HU.testCase    "CharWin1 #u18" ((Cd.niOrd (Cd.CharWin1 0xFE) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharWin1) && Cd.niOrd (Cd.CharWin1 0xFE) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharWin1)) HU.@?= True),
            HU.testCase    "CharWin1 #u19" ((Cd.niOrd (Cd.CharWin1 0xFF) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharWin1) && Cd.niOrd (Cd.CharWin1 0xFF) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharWin1)) HU.@?= True),
            QC.testProperty "Octets.Octet #p1" (\cd -> Cd.isValid (cd :: Oct.Octet) QC.==> (Cd.niOrd cd >= Cd.niMinOrd (Cd.Px :: Cd.Px Oct.Octet) && Cd.niOrd cd <= Cd.niMaxOrd (Cd.Px :: Cd.Px Oct.Octet)) ),
            HU.testCase     "Octets.Octet #u1" ((Cd.niOrd (0x00 :: Oct.Octet) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.Octet) && Cd.niOrd (0x00 :: Oct.Octet) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.Octet)) HU.@?= True),
            HU.testCase     "Octets.Octet #u2" ((Cd.niOrd (0x01 :: Oct.Octet) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.Octet) && Cd.niOrd (0x01 :: Oct.Octet) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.Octet)) HU.@?= True),
            HU.testCase     "Octets.Octet #u3" ((Cd.niOrd (0x02 :: Oct.Octet) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.Octet) && Cd.niOrd (0x02 :: Oct.Octet) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.Octet)) HU.@?= True),
            HU.testCase     "Octets.Octet #u4" ((Cd.niOrd (0x7E :: Oct.Octet) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.Octet) && Cd.niOrd (0x7E :: Oct.Octet) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.Octet)) HU.@?= True),
            HU.testCase     "Octets.Octet #u5" ((Cd.niOrd (0x7F :: Oct.Octet) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.Octet) && Cd.niOrd (0x7F :: Oct.Octet) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.Octet)) HU.@?= True),
            HU.testCase     "Octets.Octet #u6" ((Cd.niOrd (0x80 :: Oct.Octet) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.Octet) && Cd.niOrd (0x80 :: Oct.Octet) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.Octet)) HU.@?= True),
            HU.testCase     "Octets.Octet #u7" ((Cd.niOrd (0x81 :: Oct.Octet) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.Octet) && Cd.niOrd (0x81 :: Oct.Octet) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.Octet)) HU.@?= True),
            HU.testCase     "Octets.Octet #u8" ((Cd.niOrd (0xFE :: Oct.Octet) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.Octet) && Cd.niOrd (0xFE :: Oct.Octet) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.Octet)) HU.@?= True),
            HU.testCase     "Octets.Octet #u9" ((Cd.niOrd (0xFF :: Oct.Octet) >= Cd.niMinOrd (Cd.Px :: Cd.Px Cd.Octet) && Cd.niOrd (0xFF :: Oct.Octet) <= Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.Octet)) HU.@?= True),
            QC.testProperty "Bool #p1" (\cd -> Cd.isValid (cd :: Bool) QC.==> (Cd.niOrd cd >= Cd.niMinOrd (Cd.Px :: Cd.Px Bool) && Cd.niOrd cd <= Cd.niMaxOrd (Cd.Px :: Cd.Px Bool)) ),
            HU.testCase     "Bool #u1" ((Cd.niOrd False >= Cd.niMinOrd (Cd.Px :: Cd.Px Bool) && Cd.niOrd False <= Cd.niMaxOrd (Cd.Px :: Cd.Px Bool)) HU.@?= True),
            HU.testCase     "Bool #u2" ((Cd.niOrd True >= Cd.niMinOrd (Cd.Px :: Cd.Px Bool) && Cd.niOrd True <= Cd.niMaxOrd (Cd.Px :: Cd.Px Bool)) HU.@?= True)
        ]

{-  * validated: ✅
        * completeness: ✅
        * independence: ✅
        * edge cases  : ✅
        * conform doc.: ✅ -}
tgLawOrdinalNumberMinus1IfNotValid :: T.TestTree
tgLawOrdinalNumberMinus1IfNotValid =
    T.testGroup
        "Law: ordinal number -1 if not valid"
        [
            QC.testProperty "Stub #p1" (\cd -> not (Cd.isValid (cd :: TH.Stub)) QC.==> (Cd.niOrd cd == (-1)) ),
            HU.testCase     "Stub #u1" (Cd.niOrd (TH.Stub (-2))  HU.@?= (-1)),
            HU.testCase     "Stub #u2" (Cd.niOrd (TH.Stub (-1))  HU.@?= (-1)),
            HU.testCase     "Stub #u3" (Cd.niOrd (TH.Stub 100)  HU.@?= (-1)),
            HU.testCase     "Stub #u4" (Cd.niOrd (TH.Stub 101)  HU.@?= (-1)),
            QC.testProperty "Char #p1" (\cd' -> True QC.==> Cd.isValid (TH.fromT' (cd' :: (TH.T' Char)))), -- all Chars are valid!
            QC.testProperty "CharUtf8 #p1" (\cd -> not (Cd.isValid (cd :: Cd.CharUtf8)) QC.==> (Cd.niOrd cd == (-1)) ),
            HU.testCase   "CharUtf8 #u1" (Cd.niOrd (Cd.CU8OneByte 0x80) HU.@?= (-1)),
            HU.testCase   "CharUtf8 #u2" (Cd.niOrd (Cd.CU8OneByte 0x81) HU.@?= (-1)),
            HU.testCase   "CharUtf8 #u3" (Cd.niOrd (Cd.CU8OneByte 0xFF) HU.@?= (-1)),
            HU.testCase   "CharUtf8 #u4" (Cd.niOrd (Cd.CU8OneByte 0b10111111) HU.@?= (-1)),
            HU.testCase   "CharUtf8 #u5" (Cd.niOrd (Cd.CU8OneByte 0b11000000) HU.@?= (-1)),
            HU.testCase   "CharUtf8 #u6" (Cd.niOrd (Cd.CU8OneByte 0b11000001) HU.@?= (-1)),
            HU.testCase   "CharUtf8 #u7" (Cd.niOrd (Cd.CU8OneByte 0xFE) HU.@?= (-1)),
            HU.testCase   "CharUtf8 #u8" (Cd.niOrd (Cd.CU8OneByte 0xFF) HU.@?= (-1)),
            HU.testCase   "CharUtf8 #u9" (Cd.niOrd (Cd.CU8TwoBytes 0b10100000 0b10000000) HU.@?= (-1)),
            HU.testCase  "CharUtf8 #u10" (Cd.niOrd (Cd.CU8TwoBytes 0b11100000 0b10000000) HU.@?= (-1)),
            HU.testCase  "CharUtf8 #u11" (Cd.niOrd (Cd.CU8TwoBytes 0b11000000 0b11000000) HU.@?= (-1)),
            HU.testCase  "CharUtf8 #u12" (Cd.niOrd (Cd.CU8TwoBytes 0b11000000 0b01000000) HU.@?= (-1)),
            HU.testCase  "CharUtf8 #u13" (Cd.niOrd (Cd.CU8ThreeBytes 0b11010000 0b10000000 0b10000000) HU.@?= (-1)),
            HU.testCase  "CharUtf8 #u14" (Cd.niOrd (Cd.CU8ThreeBytes 0b11110000 0b10000000 0b10000000) HU.@?= (-1)),
            HU.testCase  "CharUtf8 #u15" (Cd.niOrd (Cd.CU8ThreeBytes 0b11100000 0b01000000 0b10000000) HU.@?= (-1)),
            HU.testCase  "CharUtf8 #u16" (Cd.niOrd (Cd.CU8ThreeBytes 0b11100000 0b11000000 0b10000000) HU.@?= (-1)),
            HU.testCase  "CharUtf8 #u17" (Cd.niOrd (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b01000000) HU.@?= (-1)),
            HU.testCase  "CharUtf8 #u18" (Cd.niOrd (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b11000000) HU.@?= (-1)),
            HU.testCase  "CharUtf8 #u19" (Cd.niOrd (Cd.CU8FourBytes 0b00000000 0b00000000 0b00000000 0b00000000) HU.@?= (-1)),
            HU.testCase  "CharUtf8 #u20" (Cd.niOrd (Cd.CU8FourBytes 0b00000000 0b00000000 0b00000000 0b00000001) HU.@?= (-1)),
            HU.testCase  "CharUtf8 #u21" (Cd.niOrd (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b00000000) HU.@?= (-1)),
            HU.testCase  "CharUtf8 #u22" (Cd.niOrd (Cd.CU8FourBytes 0b11110000 0b10000000 0b00000000 0b10000000) HU.@?= (-1)),
            HU.testCase  "CharUtf8 #u23" (Cd.niOrd (Cd.CU8FourBytes 0b11110000 0b00000000 0b10000000 0b10000000) HU.@?= (-1)),
            HU.testCase  "CharUtf8 #u24" (Cd.niOrd (Cd.CU8FourBytes 0b00000000 0b10000000 0b10000000 0b10000000) HU.@?= (-1)),
            HU.testCase  "CharUtf8 #u25" (Cd.niOrd (Cd.CU8FourBytes 0b11110100 0b10010000 0b10000000 0b10000000) HU.@?= (-1)),
            HU.testCase  "CharUtf8 #u26" (Cd.niOrd (Cd.CU8FourBytes 0b11111111 0b11111111 0b11111111 0b11111110) HU.@?= (-1)),
            HU.testCase  "CharUtf8 #u27" (Cd.niOrd (Cd.CU8FourBytes 0b11111111 0b11111111 0b11111111 0b11111111) HU.@?= (-1)),
            QC.testProperty "CharIso1 #p1" (\cd -> not (Cd.isValid (cd :: Cd.CharIso1)) QC.==> (Cd.niOrd cd == (-1)) ),
            HU.testCase  "CharIso1 #u1" (Cd.niOrd (Cd.CharIso1 0x80) HU.@?= (-1)),
            HU.testCase  "CharIso1 #u2" (Cd.niOrd (Cd.CharIso1 0x81) HU.@?= (-1)),
            HU.testCase  "CharIso1 #u3" (Cd.niOrd (Cd.CharIso1 0x9E) HU.@?= (-1)),
            HU.testCase  "CharIso1 #u4" (Cd.niOrd (Cd.CharIso1 0x9F) HU.@?= (-1)),
            QC.testProperty "CharWin1 #p1" (\cd -> not (Cd.isValid (cd :: Cd.CharWin1)) QC.==> (Cd.niOrd cd == (-1)) ),
            HU.testCase  "CharWin1 #u1" (Cd.niOrd (Cd.CharWin1 0x81) HU.@?= (-1)),
            HU.testCase  "CharWin1 #u2" (Cd.niOrd (Cd.CharWin1 0x8D) HU.@?= (-1)),
            HU.testCase  "CharWin1 #u3" (Cd.niOrd (Cd.CharWin1 0x8F) HU.@?= (-1)),
            HU.testCase  "CharWin1 #u4" (Cd.niOrd (Cd.CharWin1 0x90) HU.@?= (-1)),
            HU.testCase  "CharWin1 #u5" (Cd.niOrd (Cd.CharWin1 0x9D) HU.@?= (-1)),
            QC.testProperty "Octets.Octet #p1" (\cd -> True QC.==> Cd.isValid (cd :: Oct.Octet)),  -- all Octets are valid!
            QC.testProperty "Bool #p1" (\cd -> True QC.==> Cd.isValid (cd :: Bool))  -- all Bools are valid!
        ]

{-  * validated: ✅
        * completeness: ✅
        * independence: ✅
        * edge cases  : ✅
        * conform doc.: ✅ -}
tgLawReplacementIsValid :: T.TestTree
tgLawReplacementIsValid =
    T.testGroup
        "Law: replacement is valid"
        [
            HU.testCase     "Stub" (Cd.isValid (Cd.cdReplacement :: TH.Stub) HU.@?= True),
            HU.testCase     "Char #u1" (Cd.isValid (Cd.cdReplacement :: Char) HU.@?= True),
            HU.testCase     "CharUtf8 #u1" (Cd.isValid (Cd.cdReplacement :: Cd.CharUtf8) HU.@?= True),
            HU.testCase     "CharIso1 #u1" (Cd.isValid (Cd.cdReplacement :: Cd.CharIso1) HU.@?= True),
            HU.testCase     "CharWin1 #u1" (Cd.isValid (Cd.cdReplacement :: Cd.CharWin1) HU.@?= True),
            HU.testCase     "Octets.Octet #u1" (Cd.isValid (Cd.cdReplacement :: Oct.Octet) HU.@?= True),
            HU.testCase     "Bool #u1" (Cd.isValid (Cd.cdReplacement :: Bool) HU.@?= True)
        ]

{-  * validated: ✅
        * completeness: ✅
        * independence: ✅
        * edge cases  : ✅
        * conform doc.: ✅ -}
tgLawNulIsValid :: T.TestTree
tgLawNulIsValid =
    T.testGroup
        "Law: code that is NUL is valid"
        [
            QC.testProperty "Stub #p1" (\cd -> Cd.isValid (cd :: TH.Stub) QC.==> Cd.isValid cd ),
            HU.testCase "Stub #u1" (Cd.isValid (Cd.cdNul :: TH.Stub) HU.@?= True),
            HU.testCase "Stub #u1" (Cd.isValid (TH.Stub  75) HU.@?= True),
            QC.testProperty "Char #p1" (\cd -> Cd.isValid (cd :: Char) QC.==> Cd.isValid cd ),
            HU.testCase "Char #u1" (Cd.isValid (Cd.cdNul :: Char) HU.@?= True),
            QC.testProperty "CharUtf8 #p1" (\cd -> Cd.isValid (cd :: Cd.CharUtf8) QC.==> Cd.isValid cd ),
            HU.testCase "CharUtf8 #u1" (Cd.isValid (Cd.cdNul :: Cd.CharUtf8) HU.@?= True),
            HU.testCase "CharUtf8 #u2" (Cd.isValid (Cd.CU8OneByte 0x00) HU.@?= True),
            HU.testCase "CharUtf8 #u3" (Cd.isValid (Cd.CU8TwoBytes 0b11000000 0b10000000) HU.@?= True),
            HU.testCase "CharUtf8 #u4" (Cd.isValid (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b10000000) HU.@?= True),
            HU.testCase "CharUtf8 #u5" (Cd.isValid (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b10000000) HU.@?= True),
            QC.testProperty "CharIso1 #p1" (\cd -> Cd.isValid (cd :: Cd.CharIso1) QC.==> Cd.isValid cd ),
            HU.testCase "CharIso1 #u1" (Cd.isValid (Cd.cdNul :: Cd.CharIso1) HU.@?= True),
            QC.testProperty "CharWin1 #p1" (\cd -> Cd.isValid (cd :: Cd.CharWin1) QC.==> Cd.isValid cd ),
            HU.testCase "CharWin1 #u1" (Cd.isValid (Cd.cdNul :: Cd.CharWin1) HU.@?= True),
            QC.testProperty "Octets.Octet #p1" (\cd -> Cd.isValid (cd :: Oct.Octet) QC.==> Cd.isValid cd ),
            HU.testCase "Octets.Octet #u1" (Cd.isValid (Cd.cdNul :: Oct.Octet) HU.@?= True),
            QC.testProperty "Bool #p1" (\cd -> Cd.isValid (cd :: Bool) QC.==> Cd.isValid cd ),
            HU.testCase "Bool #u1" (Cd.isValid (Cd.cdNul :: Bool) HU.@?= True)
        ]

{-  * validated: ✅
        * completeness: ✅
        * independence: ✅
        * edge cases  : ✅
        * conform doc.: ✅ -}
tgLawAlwaysNormalisedCodeFromInteger :: T.TestTree
tgLawAlwaysNormalisedCodeFromInteger =
    T.testGroup
        "Law: always normalised code from Integer"
        [
            QC.testProperty "Stub #p1" (\(nj' :: TH.Int256Plus) -> True QC.==> (Cd.cdNormalise (Cd.cdFromInteger (TH.niFromInt256Plus nj')) :: TH.Stub) == Cd.cdFromInteger (TH.niFromInt256Plus nj') ),
            HU.testCase "Stub #u1" (Cd.cdNormalise (Cd.cdFromInteger (Cd.niMinOrd (Cd.Px :: Cd.Px TH.Stub) - 1) :: TH.Stub) HU.@?= Cd.cdNormalise (Cd.cdFromInteger (Cd.niMinOrd (Cd.Px :: Cd.Px TH.Stub) - 1) :: TH.Stub)),
            HU.testCase "Stub #u2" (Cd.cdNormalise (Cd.cdFromInteger (Cd.niMinOrd (Cd.Px :: Cd.Px TH.Stub)) :: TH.Stub) HU.@?= Cd.cdNormalise (Cd.cdFromInteger (Cd.niMinOrd (Cd.Px :: Cd.Px TH.Stub)) :: TH.Stub)),
            HU.testCase "Stub #u3" (Cd.cdNormalise (Cd.cdFromInteger (Cd.niMaxOrd (Cd.Px :: Cd.Px TH.Stub)) :: TH.Stub) HU.@?= Cd.cdNormalise (Cd.cdFromInteger (Cd.niMaxOrd (Cd.Px :: Cd.Px TH.Stub)) :: TH.Stub)),
            HU.testCase "Stub #u4" (Cd.cdNormalise (Cd.cdFromInteger (Cd.niMaxOrd (Cd.Px :: Cd.Px TH.Stub) + 1) :: TH.Stub) HU.@?= Cd.cdNormalise (Cd.cdFromInteger (Cd.niMaxOrd (Cd.Px :: Cd.Px TH.Stub) + 1) :: TH.Stub)),
            QC.testProperty "Char #p1" (\(nj' :: TH.IntCharPlus) -> True QC.==> Cd.cdNormalise (Cd.cdFromInteger (TH.niFromIntCharPlus nj') :: Char) == Cd.cdFromInteger (TH.niFromIntCharPlus nj') ),
            HU.testCase "Char #u1" (Cd.cdNormalise (Cd.cdFromInteger (Cd.niMinOrd (Cd.Px :: Cd.Px Char) - 1) :: Char) HU.@?= Cd.cdNormalise (Cd.cdFromInteger (Cd.niMinOrd (Cd.Px :: Cd.Px Char) - 1) :: Char)),
            HU.testCase "Char #u2" (Cd.cdNormalise (Cd.cdFromInteger (Cd.niMinOrd (Cd.Px :: Cd.Px Char)) :: Char) HU.@?= Cd.cdNormalise (Cd.cdFromInteger (Cd.niMinOrd (Cd.Px :: Cd.Px Char)) :: Char)),
            HU.testCase "Char #u3" (Cd.cdNormalise (Cd.cdFromInteger (Cd.niMaxOrd (Cd.Px :: Cd.Px Char)) :: Char) HU.@?= Cd.cdNormalise (Cd.cdFromInteger (Cd.niMaxOrd (Cd.Px :: Cd.Px Char)) :: Char)),
            HU.testCase "Char #u4" (Cd.cdNormalise (Cd.cdFromInteger (Cd.niMaxOrd (Cd.Px :: Cd.Px Char) + 1) :: Char) HU.@?= Cd.cdNormalise (Cd.cdFromInteger (Cd.niMaxOrd (Cd.Px :: Cd.Px Char) + 1) :: Char)),
            QC.testProperty "CharUtf8 #p1" (\(nj' :: TH.IntCharPlus) -> True QC.==> Cd.cdNormalise (Cd.cdFromInteger (TH.niFromIntCharPlus nj') :: Cd.CharUtf8) == Cd.cdFromInteger (TH.niFromIntCharPlus nj') ),
            HU.testCase "CharUtf8 #u1" (Cd.cdNormalise (Cd.cdFromInteger (Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharUtf8) - 1) :: Cd.CharUtf8) HU.@?= Cd.cdNormalise (Cd.cdFromInteger (Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharUtf8) - 1) :: Cd.CharUtf8)),
            HU.testCase "CharUtf8 #u2" (Cd.cdNormalise (Cd.cdFromInteger (Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharUtf8)) :: Cd.CharUtf8) HU.@?= Cd.cdNormalise (Cd.cdFromInteger (Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharUtf8)) :: Cd.CharUtf8)),
            HU.testCase "CharUtf8 #u3" (Cd.cdNormalise (Cd.cdFromInteger (Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharUtf8)) :: Cd.CharUtf8) HU.@?= Cd.cdNormalise (Cd.cdFromInteger (Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharUtf8)) :: Cd.CharUtf8)),
            HU.testCase "CharUtf8 #u4" (Cd.cdNormalise (Cd.cdFromInteger (Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharUtf8) + 1) :: Cd.CharUtf8) HU.@?= Cd.cdNormalise (Cd.cdFromInteger (Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharUtf8) + 1) :: Cd.CharUtf8)),
            QC.testProperty "CharIso1 #p1" (\(nj' :: TH.Int256Plus) -> True QC.==> Cd.cdNormalise (Cd.cdFromInteger (TH.niFromInt256Plus nj') :: Cd.CharIso1) == Cd.cdFromInteger (TH.niFromInt256Plus nj') ),
            HU.testCase "CharIso1 #u1" (Cd.cdNormalise (Cd.cdFromInteger (Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharIso1) - 1) :: Cd.CharIso1) HU.@?= Cd.cdNormalise (Cd.cdFromInteger (Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharIso1) - 1) :: Cd.CharIso1)),
            HU.testCase "CharIso1 #u2" (Cd.cdNormalise (Cd.cdFromInteger (Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharIso1)) :: Cd.CharIso1) HU.@?= Cd.cdNormalise (Cd.cdFromInteger (Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharIso1)) :: Cd.CharIso1)),
            HU.testCase "CharIso1 #u3" (Cd.cdNormalise (Cd.cdFromInteger (Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharIso1)) :: Cd.CharIso1) HU.@?= Cd.cdNormalise (Cd.cdFromInteger (Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharIso1)) :: Cd.CharIso1)),
            HU.testCase "CharIso1 #u4" (Cd.cdNormalise (Cd.cdFromInteger (Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharIso1) + 1) :: Cd.CharIso1) HU.@?= Cd.cdNormalise (Cd.cdFromInteger (Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharIso1) + 1) :: Cd.CharIso1)),
            QC.testProperty "CharWin1 #p1" (\(nj' :: TH.Int256Plus) -> True QC.==> Cd.cdNormalise (Cd.cdFromInteger (TH.niFromInt256Plus nj') :: Cd.CharWin1) == Cd.cdFromInteger (TH.niFromInt256Plus nj') ),
            HU.testCase "CharWin1 #u1" (Cd.cdNormalise (Cd.cdFromInteger (Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharWin1) - 1) :: Cd.CharWin1) HU.@?= Cd.cdNormalise (Cd.cdFromInteger (Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharWin1) - 1) :: Cd.CharWin1)),
            HU.testCase "CharWin1 #u2" (Cd.cdNormalise (Cd.cdFromInteger (Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharWin1)) :: Cd.CharWin1) HU.@?= Cd.cdNormalise (Cd.cdFromInteger (Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharWin1)) :: Cd.CharWin1)),
            HU.testCase "CharWin1 #u3" (Cd.cdNormalise (Cd.cdFromInteger (Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharWin1)) :: Cd.CharWin1) HU.@?= Cd.cdNormalise (Cd.cdFromInteger (Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharWin1)) :: Cd.CharWin1)),
            HU.testCase "CharWin1 #u4" (Cd.cdNormalise (Cd.cdFromInteger (Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharWin1) + 1) :: Cd.CharWin1) HU.@?= Cd.cdNormalise (Cd.cdFromInteger (Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharWin1) + 1) :: Cd.CharWin1)),
            QC.testProperty "Octets.Octet #p1" (\(nj' :: TH.Int256Plus) -> True QC.==> Cd.cdNormalise (Cd.cdFromInteger (TH.niFromInt256Plus nj') :: Oct.Octet) == Cd.cdFromInteger (TH.niFromInt256Plus nj') ),
            HU.testCase "Octets.Octet #u1" (Cd.cdNormalise (Cd.cdFromInteger (Cd.niMinOrd (Cd.Px :: Cd.Px Oct.Octet) - 1) :: Oct.Octet) HU.@?= Cd.cdNormalise (Cd.cdFromInteger (Cd.niMinOrd (Cd.Px :: Cd.Px Oct.Octet) - 1) :: Oct.Octet)),
            HU.testCase "Octets.Octet #u2" (Cd.cdNormalise (Cd.cdFromInteger (Cd.niMinOrd (Cd.Px :: Cd.Px Oct.Octet)) :: Oct.Octet) HU.@?= Cd.cdNormalise (Cd.cdFromInteger (Cd.niMinOrd (Cd.Px :: Cd.Px Oct.Octet)) :: Oct.Octet)),
            HU.testCase "Octets.Octet #u3" (Cd.cdNormalise (Cd.cdFromInteger (Cd.niMaxOrd (Cd.Px :: Cd.Px Oct.Octet)) :: Oct.Octet) HU.@?= Cd.cdNormalise (Cd.cdFromInteger (Cd.niMaxOrd (Cd.Px :: Cd.Px Oct.Octet)) :: Oct.Octet)),
            HU.testCase "Octets.Octet #u4" (Cd.cdNormalise (Cd.cdFromInteger (Cd.niMaxOrd (Cd.Px :: Cd.Px Oct.Octet) + 1) :: Oct.Octet) HU.@?= Cd.cdNormalise (Cd.cdFromInteger (Cd.niMaxOrd (Cd.Px :: Cd.Px Oct.Octet) + 1) :: Oct.Octet)),
            QC.testProperty "Bool #p1" (\(nj' :: TH.Int256Plus) -> True QC.==> Cd.cdNormalise (Cd.cdFromInteger (TH.niFromInt256Plus nj') :: Bool) == Cd.cdFromInteger (TH.niFromInt256Plus nj') ),
            HU.testCase "Bool #u1" (Cd.cdNormalise (Cd.cdFromInteger (Cd.niMinOrd (Cd.Px :: Cd.Px Bool) - 1) :: Bool) HU.@?= Cd.cdNormalise (Cd.cdFromInteger (Cd.niMinOrd (Cd.Px :: Cd.Px Bool) - 1) :: Bool)),
            HU.testCase "Bool #u2" (Cd.cdNormalise (Cd.cdFromInteger (Cd.niMinOrd (Cd.Px :: Cd.Px Bool)) :: Bool) HU.@?= Cd.cdNormalise (Cd.cdFromInteger (Cd.niMinOrd (Cd.Px :: Cd.Px Bool)) :: Bool)),
            HU.testCase "Bool #u3" (Cd.cdNormalise (Cd.cdFromInteger (Cd.niMaxOrd (Cd.Px :: Cd.Px Bool)) :: Bool) HU.@?= Cd.cdNormalise (Cd.cdFromInteger (Cd.niMaxOrd (Cd.Px :: Cd.Px Bool)) :: Bool)),
            HU.testCase "Bool #u4" (Cd.cdNormalise (Cd.cdFromInteger (Cd.niMaxOrd (Cd.Px :: Cd.Px Bool) + 1) :: Bool) HU.@?= Cd.cdNormalise (Cd.cdFromInteger (Cd.niMaxOrd (Cd.Px :: Cd.Px Bool) + 1) :: Bool))
        ]

{-  * validated: ✅
        * completeness: ✅
        * independence: ✅
        * edge cases  : ✅
        * conform doc.: ✅ -}
tgLawReplacementIsAlwaysNormalised :: T.TestTree
tgLawReplacementIsAlwaysNormalised =
    T.testGroup
        "Law: replacement is always normalised"
        [
            HU.testCase "Stub #u1" (Cd.cdNormalise (Cd.cdReplacement :: TH.Stub) HU.@?= Cd.cdReplacement),
            HU.testCase "Stub #u2" (Cd.isNormalised (Cd.cdReplacement :: TH.Stub) HU.@?= True),
            HU.testCase "Char #u1" (Cd.cdNormalise (Cd.cdReplacement :: Char) HU.@?= Cd.cdReplacement),
            HU.testCase "Char #u2" (Cd.isNormalised (Cd.cdReplacement :: Char) HU.@?= True),
            HU.testCase "CharUtf8 #u1" (Cd.cdNormalise (Cd.cdReplacement :: Cd.CharUtf8) HU.@?= Cd.cdReplacement),
            HU.testCase "CharUtf8 #u2" (Cd.isNormalised (Cd.cdReplacement :: Cd.CharUtf8) HU.@?= True),
            HU.testCase "CharIso1 #u1" (Cd.cdNormalise (Cd.cdReplacement :: Cd.CharIso1) HU.@?= Cd.cdReplacement),
            HU.testCase "CharIso1 #u2" (Cd.isNormalised (Cd.cdReplacement :: Cd.CharIso1) HU.@?= True),
            HU.testCase "CharWin1 #u1" (Cd.cdNormalise (Cd.cdReplacement :: Cd.CharWin1) HU.@?= Cd.cdReplacement),
            HU.testCase "CharWin1 #u2" (Cd.isNormalised (Cd.cdReplacement :: Cd.CharWin1) HU.@?= True),
            HU.testCase "Octets.Octet #u1" (Cd.cdNormalise (Cd.cdReplacement :: Oct.Octet) HU.@?= Cd.cdReplacement),
            HU.testCase "Octets.Octet #u2" (Cd.isNormalised (Cd.cdReplacement :: Oct.Octet) HU.@?= True),
            HU.testCase "Bool #u1" (Cd.cdNormalise (Cd.cdReplacement :: Bool) HU.@?= Cd.cdReplacement),
            HU.testCase "Bool #u2" (Cd.isNormalised (Cd.cdReplacement :: Bool) HU.@?= True)
        ]

{-  * validated: ✅
        * completeness: ✅
        * independence: ✅
        * edge cases  : ✅
        * conform doc.: ✅ -}
tgLawNormalisedCodeIsEqualNormalisedNormalisedCode :: T.TestTree
tgLawNormalisedCodeIsEqualNormalisedNormalisedCode =
    T.testGroup
        "Law: normalised code is equal normalised normalised code"
        (
            QC.testProperty "Stub #p1" (\cd -> True QC.==> Cd.cdNormalise (Cd.cdNormalise (cd :: TH.Stub)) == Cd.cdNormalise cd) :
            tgLawNormalisedCodeIsEqualNormalisedNormalisedCodeUnitTests "Stub #u" lstbEdgeCases ++
            QC.testProperty "Char #p1" (\cd' -> True QC.==> Cd.cdNormalise (Cd.cdNormalise (TH.fromT' cd' :: Char)) == Cd.cdNormalise (TH.fromT' cd')) :
            tgLawNormalisedCodeIsEqualNormalisedNormalisedCodeUnitTests "Char #u" lchEdgeCases ++
            QC.testProperty "CharUtf8 #p1" (\cd -> True QC.==> Cd.cdNormalise (Cd.cdNormalise (cd :: Cd.CharUtf8)) == Cd.cdNormalise cd) :
            tgLawNormalisedCodeIsEqualNormalisedNormalisedCodeUnitTests "CharUtf8 #u" lcu8EdgeCases ++
            QC.testProperty "CharIso1 #p1" (\cd -> True QC.==> Cd.cdNormalise (Cd.cdNormalise (cd :: Cd.CharIso1)) == Cd.cdNormalise cd) :
            tgLawNormalisedCodeIsEqualNormalisedNormalisedCodeUnitTests "CharIso1 #u" lci1EdgeCases ++
            QC.testProperty "CharWin1 #p1" (\cd -> True QC.==> Cd.cdNormalise (Cd.cdNormalise (cd :: Cd.CharWin1)) == Cd.cdNormalise cd) :
            tgLawNormalisedCodeIsEqualNormalisedNormalisedCodeUnitTests "CharWin1 #u" lcw1EdgeCases ++
            QC.testProperty "Octets.Octet #p1" (\cd' -> True QC.==> Cd.cdNormalise (Cd.cdNormalise (TH.fromT' cd' :: Oct.Octet)) == Cd.cdNormalise (TH.fromT' cd')) :
            tgLawNormalisedCodeIsEqualNormalisedNormalisedCodeUnitTests "Octets.Octet #u" loctEdgeCases ++
            QC.testProperty "Bool #p1" (\cd -> True QC.==> Cd.cdNormalise (Cd.cdNormalise (cd :: Bool)) == Cd.cdNormalise cd) : 
            tgLawNormalisedCodeIsEqualNormalisedNormalisedCodeUnitTests "Bool #u" lisEdgeCases
        )

tgLawNormalisedCodeIsEqualNormalisedNormalisedCodeUnitTests :: (Cd.Code a, Show a) => String -> [a] -> [T.TestTree]
tgLawNormalisedCodeIsEqualNormalisedNormalisedCodeUnitTests sTestName lx = 
    fmap 
        (\tpl -> 
            HU.testCase 
                (sTestName ++ show (snd tpl)) 
                (Cd.cdNormalise (Cd.cdNormalise (fst tpl)) HU.@?= Cd.cdNormalise (fst tpl)))  
        (zip lx [1,2..])

{-  * validated: ✅
        * completeness: ✅
        * independence: ✅
        * edge cases  : ✅
        * conform doc.: ✅ -}
tgLawNormalisedCodeIsAlwaysValid :: T.TestTree
tgLawNormalisedCodeIsAlwaysValid =
    T.testGroup
        "Law: normalised code is always valid"
        (
            QC.testProperty "Stub #p1" (\cd -> True QC.==> Cd.isValid (Cd.cdNormalise (cd :: TH.Stub))) : 
            tgLawNormalisedCodeIsAlwaysValidUnitTests "Stub #u" lstbEdgeCases ++
            QC.testProperty "Char #p1" (\cd' -> True QC.==> Cd.isValid (Cd.cdNormalise (TH.fromT' cd' :: Char))) :
            tgLawNormalisedCodeIsAlwaysValidUnitTests "Char #u" lchEdgeCases ++
            QC.testProperty "CharUtf8 #p1" (\cd -> True QC.==> Cd.isValid (Cd.cdNormalise (cd :: Cd.CharUtf8))) : 
            tgLawNormalisedCodeIsAlwaysValidUnitTests "CharUtf8 #u" lcu8EdgeCases ++
            QC.testProperty "CharIso1 #p1" (\cd -> True QC.==> Cd.isValid (Cd.cdNormalise (cd :: Cd.CharIso1))) : 
            tgLawNormalisedCodeIsAlwaysValidUnitTests "CharIso1 #u" lci1EdgeCases ++
            QC.testProperty "CharWin1 #p1" (\cd -> True QC.==> Cd.isValid (Cd.cdNormalise (cd :: Cd.CharWin1))) : 
            tgLawNormalisedCodeIsAlwaysValidUnitTests "CharWin1 #u" lcw1EdgeCases ++
            QC.testProperty "Octets.Octet #p1" (\cd' -> True QC.==> Cd.isValid (Cd.cdNormalise (TH.fromT' cd' :: Oct.Octet))) : 
            tgLawNormalisedCodeIsAlwaysValidUnitTests "Octets.Octet #u" loctEdgeCases ++
            QC.testProperty "Bool #p1" (\cd -> True QC.==> Cd.isValid (Cd.cdNormalise (cd :: Bool))) : 
            tgLawNormalisedCodeIsAlwaysValidUnitTests "Bool #u" lisEdgeCases
        )

tgLawNormalisedCodeIsAlwaysValidUnitTests :: (Cd.Code a, Show a) => String -> [a] -> [T.TestTree]
tgLawNormalisedCodeIsAlwaysValidUnitTests sTestName lx = fmap (\tpl -> HU.testCase  (sTestName ++ show (snd tpl)) (Cd.isValid (Cd.cdNormalise (fst tpl)) HU.@?= True)) (zip lx [1,2..])

{-  * validated: ✅
        * completeness: ✅
        * independence: ✅
        * edge cases  : ✅
        * conform doc.: ✅ -}
tgUnitcdNul :: T.TestTree
tgUnitcdNul =
    T.testGroup
    "cdNul"
    [
        HU.testCase "Stub #u1" ((Cd.cdNul :: TH.Stub) HU.@?= TH.Stub 25),
        HU.testCase "Char #u1" ((Cd.cdNul :: Char) HU.@?= '\0'),
        HU.testCase "CharUtf8 #u1" ((Cd.cdNul :: Cd.CharUtf8) HU.@?= Cd.CU8OneByte 0x00),
        HU.testCase "CharIso1 #u1" ((Cd.cdNul :: Cd.CharIso1) HU.@?= Cd.CharIso1 0x00),
        HU.testCase "CharWin1 #u1" ((Cd.cdNul :: Cd.CharWin1) HU.@?= Cd.CharWin1 0x00),
        HU.testCase "Octets.Octet #u1" ((Cd.cdNul :: Oct.Octet) HU.@?= 0x00),
        HU.testCase "Bool #u1" ((Cd.cdNul :: Bool) HU.@?= False)
    ]

{-  * validated: ✅
        * completeness: ✅
        * independence: ✅
        * edge cases  : ✅
        * conform doc.: ✅ -}
tgUnitisNul :: T.TestTree
tgUnitisNul =
    T.testGroup
    "isNul"
    [
        QC.testProperty "Stub #p1" (\cd -> notElem cd [TH.Stub 25, TH.Stub 75] QC.==> not (Cd.isNul cd)),
        HU.testCase  "Stub #u1" (Cd.isNul (TH.Stub (-2)) HU.@?= False),
        HU.testCase  "Stub #u2" (Cd.isNul (TH.Stub (-1)) HU.@?= False),
        HU.testCase  "Stub #u3" (Cd.isNul (TH.Stub 0) HU.@?= False),
        HU.testCase  "Stub #u4" (Cd.isNul (TH.Stub 1) HU.@?= False),
        HU.testCase  "Stub #u5" (Cd.isNul (TH.Stub 2) HU.@?= False),
        HU.testCase  "Stub #u6" (Cd.isNul (TH.Stub 3) HU.@?= False),
        HU.testCase  "Stub #u7" (Cd.isNul (TH.Stub 23) HU.@?= False),
        HU.testCase  "Stub #u8" (Cd.isNul (TH.Stub 24) HU.@?= False),
        HU.testCase  "Stub #u9" (Cd.isNul (TH.Stub 25) HU.@?= True),
        HU.testCase "Stub #u10" (Cd.isNul (TH.Stub 26) HU.@?= False),
        HU.testCase "Stub #u11" (Cd.isNul (TH.Stub 27) HU.@?= False),
        HU.testCase "Stub #u12" (Cd.isNul (TH.Stub 73) HU.@?= False),
        HU.testCase "Stub #u13" (Cd.isNul (TH.Stub 74) HU.@?= False),
        HU.testCase "Stub #u14" (Cd.isNul (TH.Stub 75) HU.@?= True),
        HU.testCase "Stub #u15" (Cd.isNul (TH.Stub 76) HU.@?= False),
        HU.testCase "Stub #u16" (Cd.isNul (TH.Stub 77) HU.@?= False),
        HU.testCase "Stub #u17" (Cd.isNul (TH.Stub 78) HU.@?= False),
        HU.testCase "Stub #u18" (Cd.isNul (TH.Stub 98) HU.@?= False),
        HU.testCase "Stub #u19" (Cd.isNul (TH.Stub 99) HU.@?= False),
        HU.testCase "Stub #u20" (Cd.isNul (TH.Stub 100) HU.@?= False),
        HU.testCase "Stub #u21" (Cd.isNul (TH.Stub 101) HU.@?= False),
        QC.testProperty "Char #p1" (\cd' -> TH.fromT' (cd' :: (TH.T' Char)) /= '\x0' QC.==> not (Cd.isNul (TH.fromT' cd'))),
        HU.testCase  "Char #u1" (Cd.isNul '\x0' HU.@?= True),
        HU.testCase  "Char #u2" (Cd.isNul '\x1' HU.@?= False),
        HU.testCase  "Char #u3" (Cd.isNul '\x2' HU.@?= False),
        HU.testCase  "Char #u4" (Cd.isNul '\x3' HU.@?= False),
        HU.testCase  "Char #u5" (Cd.isNul '\x10FFFD' HU.@?= False),
        HU.testCase  "Char #u6" (Cd.isNul '\x10FFFE' HU.@?= False),
        HU.testCase  "Char #u7" (Cd.isNul '\x10FFFF' HU.@?= False),
        QC.testProperty "CharUtf8 #p1" (\cd -> notElem cd [Cd.CU8OneByte 0x00, Cd.CU8TwoBytes 0b11000000 0b10000000, Cd.CU8ThreeBytes 0b11100000 0b10000000 0b10000000, Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b10000000] QC.==> not (Cd.isNul cd)),
        HU.testCase  "CharUtf8 #u1" (Cd.isNul (Cd.CU8OneByte 0x00) HU.@?= True),
        HU.testCase  "CharUtf8 #u2" (Cd.isNul (Cd.CU8OneByte 0x01) HU.@?= False),
        HU.testCase  "CharUtf8 #u3" (Cd.isNul (Cd.CU8OneByte 0x02) HU.@?= False),
        HU.testCase  "CharUtf8 #u4" (Cd.isNul (Cd.CU8OneByte 0x03) HU.@?= False),
        HU.testCase  "CharUtf8 #u5" (Cd.isNul (Cd.CU8OneByte 0x7D) HU.@?= False),
        HU.testCase  "CharUtf8 #u6" (Cd.isNul (Cd.CU8OneByte 0x7E) HU.@?= False),
        HU.testCase  "CharUtf8 #u7" (Cd.isNul (Cd.CU8OneByte 0x7F) HU.@?= False),
        HU.testCase  "CharUtf8 #u8" (Cd.isNul (Cd.CU8OneByte 0x80) HU.@?= False),
        HU.testCase  "CharUtf8 #u9" (Cd.isNul (Cd.CU8OneByte 0x81) HU.@?= False),
        HU.testCase "CharUtf8 #u10" (Cd.isNul (Cd.CU8OneByte 0x82) HU.@?= False),
        HU.testCase "CharUtf8 #u11" (Cd.isNul (Cd.CU8OneByte 0xFD) HU.@?= False),
        HU.testCase "CharUtf8 #u12" (Cd.isNul (Cd.CU8OneByte 0xFE) HU.@?= False),
        HU.testCase "CharUtf8 #u13" (Cd.isNul (Cd.CU8OneByte 0xFF) HU.@?= False),
        HU.testCase "CharUtf8 #u14" (Cd.isNul (Cd.CU8TwoBytes 0b11000000 0b01111110) HU.@?= False),
        HU.testCase "CharUtf8 #u15" (Cd.isNul (Cd.CU8TwoBytes 0b11000000 0b01111111) HU.@?= False),
        HU.testCase "CharUtf8 #u16" (Cd.isNul (Cd.CU8TwoBytes 0b11000000 0b10000000) HU.@?= True),
        HU.testCase "CharUtf8 #u17" (Cd.isNul (Cd.CU8TwoBytes 0b11000000 0b10000001) HU.@?= False),
        HU.testCase "CharUtf8 #u18" (Cd.isNul (Cd.CU8TwoBytes 0b11000000 0b10000010) HU.@?= False),
        HU.testCase "CharUtf8 #u19" (Cd.isNul (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b01111110) HU.@?= False),
        HU.testCase "CharUtf8 #u20" (Cd.isNul (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b01111111) HU.@?= False),
        HU.testCase "CharUtf8 #u21" (Cd.isNul (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b10000000) HU.@?= True),
        HU.testCase "CharUtf8 #u22" (Cd.isNul (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b10000001) HU.@?= False),
        HU.testCase "CharUtf8 #u23" (Cd.isNul (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b10000010) HU.@?= False),
        HU.testCase "CharUtf8 #u24" (Cd.isNul (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b01111110) HU.@?= False),
        HU.testCase "CharUtf8 #u25" (Cd.isNul (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b01111111) HU.@?= False),
        HU.testCase "CharUtf8 #u26" (Cd.isNul (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b10000000) HU.@?= True),
        HU.testCase "CharUtf8 #u27" (Cd.isNul (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b10000001) HU.@?= False),
        HU.testCase "CharUtf8 #u28" (Cd.isNul (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b10000010) HU.@?= False),
        QC.testProperty "CharIso1 #p1" (\cd -> cd /= Cd.CharIso1 0x00 QC.==> not (Cd.isNul cd)),
        HU.testCase     "CharIso1 #u1" (Cd.isNul (Cd.CharIso1 0x00) HU.@?= True),
        HU.testCase     "CharIso1 #u2" (Cd.isNul (Cd.CharIso1 0x01) HU.@?= False),
        HU.testCase     "CharIso1 #u3" (Cd.isNul (Cd.CharIso1 0x02) HU.@?= False),
        HU.testCase     "CharIso1 #u4" (Cd.isNul (Cd.CharIso1 0x03) HU.@?= False),
        HU.testCase     "CharIso1 #u5" (Cd.isNul (Cd.CharIso1 0xFD) HU.@?= False),
        HU.testCase     "CharIso1 #u6" (Cd.isNul (Cd.CharIso1 0xFE) HU.@?= False),
        HU.testCase     "CharIso1 #u7" (Cd.isNul (Cd.CharIso1 0xFF) HU.@?= False),
        QC.testProperty "CharWin1 #p1" (\cd -> cd /= Cd.CharWin1 0x00 QC.==> not (Cd.isNul cd)),
        HU.testCase     "CharWin1 #u1" (Cd.isNul (Cd.CharWin1 0x00) HU.@?= True),
        HU.testCase     "CharWin1 #u2" (Cd.isNul (Cd.CharWin1 0x01) HU.@?= False),
        HU.testCase     "CharWin1 #u3" (Cd.isNul (Cd.CharWin1 0x02) HU.@?= False),
        HU.testCase     "CharWin1 #u4" (Cd.isNul (Cd.CharWin1 0x03) HU.@?= False),
        HU.testCase     "CharWin1 #u5" (Cd.isNul (Cd.CharWin1 0xFD) HU.@?= False),
        HU.testCase     "CharWin1 #u6" (Cd.isNul (Cd.CharWin1 0xFE) HU.@?= False),
        HU.testCase     "CharWin1 #u7" (Cd.isNul (Cd.CharWin1 0xFF) HU.@?= False),
        QC.testProperty "Octets.Octet #p1" (\cd -> (cd :: Oct.Octet) /= 0x00 QC.==> not (Cd.isNul cd)),
        HU.testCase     "Octets.Octet #u1" (Cd.isNul (0x00 :: Oct.Octet) HU.@?= True),
        HU.testCase     "Octets.Octet #u2" (Cd.isNul (0x01 :: Oct.Octet) HU.@?= False),
        HU.testCase     "Octets.Octet #u3" (Cd.isNul (0x02 :: Oct.Octet) HU.@?= False),
        HU.testCase     "Octets.Octet #u4" (Cd.isNul (0x03 :: Oct.Octet) HU.@?= False),
        HU.testCase     "Octets.Octet #u5" (Cd.isNul (0xFD :: Oct.Octet) HU.@?= False),
        HU.testCase     "Octets.Octet #u6" (Cd.isNul (0xFE :: Oct.Octet) HU.@?= False),
        HU.testCase     "Octets.Octet #u7" (Cd.isNul (0xFF :: Oct.Octet) HU.@?= False),
        HU.testCase "Bool #u1" (Cd.isNul False HU.@?= True),
        HU.testCase "Bool #u2" (Cd.isNul True HU.@?= False)
    ]

{-  * validated: ✅
        * completeness: ✅
        * independence: ✅
        * edge cases  : ✅
        * conform doc.: ✅ -}
tgUnitisValid :: T.TestTree
tgUnitisValid =
    T.testGroup
    "isValid"
    [
        QC.testProperty "Stub #p1" (\ni -> (ni >= 0 && ni <= 99) QC.==> Cd.isValid (TH.Stub ni)),
        QC.testProperty "Stub #p2" (\ni -> (ni < 0 || ni > 99) QC.==> not (Cd.isValid (TH.Stub ni))),
        HU.testCase     "Stub #u1" (Cd.isValid (TH.Stub (-2)) HU.@?= False),
        HU.testCase     "Stub #u2" (Cd.isValid (TH.Stub (-1)) HU.@?= False),
        HU.testCase     "Stub #u3" (Cd.isValid (TH.Stub 0) HU.@?= True),
        HU.testCase     "Stub #u4" (Cd.isValid (TH.Stub 1) HU.@?= True),
        HU.testCase     "Stub #u5" (Cd.isValid (TH.Stub 2) HU.@?= True),
        HU.testCase     "Stub #u6" (Cd.isValid (TH.Stub 3) HU.@?= True),
        HU.testCase     "Stub #u7" (Cd.isValid (TH.Stub 22) HU.@?= True),
        HU.testCase     "Stub #8u" (Cd.isValid (TH.Stub 23) HU.@?= True),
        HU.testCase     "Stub #u9" (Cd.isValid (TH.Stub 24) HU.@?= True),
        HU.testCase    "Stub #u10" (Cd.isValid (TH.Stub 25) HU.@?= True),
        HU.testCase    "Stub #u11" (Cd.isValid (TH.Stub 26) HU.@?= True),
        HU.testCase    "Stub #u12" (Cd.isValid (TH.Stub 27) HU.@?= True),
        HU.testCase    "Stub #u13" (Cd.isValid (TH.Stub 73) HU.@?= True),
        HU.testCase    "Stub #u14" (Cd.isValid (TH.Stub 74) HU.@?= True),
        HU.testCase    "Stub #u15" (Cd.isValid (TH.Stub 75) HU.@?= True),
        HU.testCase    "Stub #u16" (Cd.isValid (TH.Stub 76) HU.@?= True),
        HU.testCase    "Stub #u17" (Cd.isValid (TH.Stub 77) HU.@?= True),
        HU.testCase    "Stub #u18" (Cd.isValid (TH.Stub 78) HU.@?= True),
        HU.testCase    "Stub #u19" (Cd.isValid (TH.Stub 98) HU.@?= True),
        HU.testCase    "Stub #u20" (Cd.isValid (TH.Stub 99) HU.@?= True),
        HU.testCase    "Stub #u21" (Cd.isValid (TH.Stub 100) HU.@?= False),
        HU.testCase    "Stub #u22" (Cd.isValid (TH.Stub 101) HU.@?= False),
        QC.testProperty "Char #p1" (\cd' -> True QC.==> Cd.isValid (TH.fromT' (cd' :: TH.T' Char))), -- it's unpossible to construct an invalid Char
        HU.testCase  "Char #u1" (Cd.isValid '\x0' HU.@?= True),
        HU.testCase  "Char #u2" (Cd.isValid '\x1' HU.@?= True),
        HU.testCase  "Char #u3" (Cd.isValid '\x2' HU.@?= True),
        HU.testCase  "Char #u4" (Cd.isValid '\x3' HU.@?= True),
        HU.testCase  "Char #u5" (Cd.isValid '\x10FFFD' HU.@?= True),
        HU.testCase  "Char #u6" (Cd.isValid '\x10FFFE' HU.@?= True),
        HU.testCase  "Char #u7" (Cd.isValid '\x10FFFF' HU.@?= True),
        testPropHelp1 "CharUtf8 #p1",
        HU.testCase  "CharUtf8 #1" (Cd.isValid (Cd.CU8OneByte 0x00) HU.@?= True),
        HU.testCase  "CharUtf8 #2" (Cd.isValid (Cd.CU8OneByte 0x01) HU.@?= True),
        HU.testCase  "CharUtf8 #3" (Cd.isValid (Cd.CU8OneByte 0x02) HU.@?= True),
        HU.testCase  "CharUtf8 #4" (Cd.isValid (Cd.CU8OneByte 0x03) HU.@?= True),
        HU.testCase  "CharUtf8 #5" (Cd.isValid (Cd.CU8OneByte 0x7D) HU.@?= True),
        HU.testCase  "CharUtf8 #6" (Cd.isValid (Cd.CU8OneByte 0x7E) HU.@?= True),
        HU.testCase  "CharUtf8 #7" (Cd.isValid (Cd.CU8OneByte 0x7F) HU.@?= True),
        HU.testCase  "CharUtf8 #8" (Cd.isValid (Cd.CU8OneByte 0x80) HU.@?= False),
        HU.testCase  "CharUtf8 #9" (Cd.isValid (Cd.CU8OneByte 0x81) HU.@?= False),
        HU.testCase "CharUtf8 #10" (Cd.isValid (Cd.CU8OneByte 0x82) HU.@?= False),
        HU.testCase "CharUtf8 #11" (Cd.isValid (Cd.CU8OneByte 0xFD) HU.@?= False),
        HU.testCase "CharUtf8 #12" (Cd.isValid (Cd.CU8OneByte 0xFE) HU.@?= False),
        HU.testCase "CharUtf8 #13" (Cd.isValid (Cd.CU8OneByte 0xFF) HU.@?= False),
        HU.testCase "CharUtf8 #14" (Cd.isValid (Cd.CU8TwoBytes 0b11000000 0b01111110) HU.@?= False),
        HU.testCase "CharUtf8 #15" (Cd.isValid (Cd.CU8TwoBytes 0b11000000 0b01111111) HU.@?= False),
        HU.testCase "CharUtf8 #16" (Cd.isValid (Cd.CU8TwoBytes 0b11000000 0b10000000) HU.@?= True),
        HU.testCase "CharUtf8 #27" (Cd.isValid (Cd.CU8TwoBytes 0b11000000 0b10000001) HU.@?= True),
        HU.testCase "CharUtf8 #28" (Cd.isValid (Cd.CU8TwoBytes 0b11000000 0b10000010) HU.@?= True),
        HU.testCase "CharUtf8 #29" (Cd.isValid (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b01111110) HU.@?= False),
        HU.testCase "CharUtf8 #30" (Cd.isValid (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b01111111) HU.@?= False),
        HU.testCase "CharUtf8 #31" (Cd.isValid (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b10000000) HU.@?= True),
        HU.testCase "CharUtf8 #32" (Cd.isValid (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b10000001) HU.@?= True),
        HU.testCase "CharUtf8 #33" (Cd.isValid (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b10000010) HU.@?= True),
        HU.testCase "CharUtf8 #34" (Cd.isValid (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b01111110) HU.@?= False),
        HU.testCase "CharUtf8 #35" (Cd.isValid (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b01111111) HU.@?= False),
        HU.testCase "CharUtf8 #36" (Cd.isValid (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b10000000) HU.@?= True),
        HU.testCase "CharUtf8 #37" (Cd.isValid (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b10000001) HU.@?= True),
        HU.testCase "CharUtf8 #38" (Cd.isValid (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b10000010) HU.@?= True),
        QC.testProperty "CharIso1 #p1" (\cd -> Cd.roctCI1 cd < 0x80 || Cd.roctCI1 cd >= 0xA0 QC.==> Cd.isValid cd),
        QC.testProperty "CharIso1 #p2" (\cd -> Cd.roctCI1 cd >= 0x80 && Cd.roctCI1 cd < 0xA0 QC.==> not (Cd.isValid cd)),
        HU.testCase  "CharIso1 #u1" (Cd.isValid (Cd.CharIso1 0x00) HU.@?= True),
        HU.testCase  "CharIso1 #u2" (Cd.isValid (Cd.CharIso1 0x01) HU.@?= True),
        HU.testCase  "CharIso1 #u3" (Cd.isValid (Cd.CharIso1 0x02) HU.@?= True),
        HU.testCase  "CharIso1 #u4" (Cd.isValid (Cd.CharIso1 0x03) HU.@?= True),
        HU.testCase  "CharIso1 #u5" (Cd.isValid (Cd.CharIso1 0x7C) HU.@?= True),
        HU.testCase  "CharIso1 #u6" (Cd.isValid (Cd.CharIso1 0x7D) HU.@?= True),
        HU.testCase  "CharIso1 #u7" (Cd.isValid (Cd.CharIso1 0x7E) HU.@?= True),
        HU.testCase  "CharIso1 #u8" (Cd.isValid (Cd.CharIso1 0x7F) HU.@?= True),
        HU.testCase  "CharIso1 #u9" (Cd.isValid (Cd.CharIso1 0x80) HU.@?= False),
        HU.testCase "CharIso1 #u10" (Cd.isValid (Cd.CharIso1 0x81) HU.@?= False),
        HU.testCase "CharIso1 #u11" (Cd.isValid (Cd.CharIso1 0x82) HU.@?= False),
        HU.testCase "CharIso1 #u12" (Cd.isValid (Cd.CharIso1 0x9D) HU.@?= False),
        HU.testCase "CharIso1 #u13" (Cd.isValid (Cd.CharIso1 0x9E) HU.@?= False),
        HU.testCase "CharIso1 #u14" (Cd.isValid (Cd.CharIso1 0x9F) HU.@?= False),
        HU.testCase "CharIso1 #u15" (Cd.isValid (Cd.CharIso1 0xA0) HU.@?= True),
        HU.testCase "CharIso1 #u16" (Cd.isValid (Cd.CharIso1 0xA1) HU.@?= True),
        HU.testCase "CharIso1 #u17" (Cd.isValid (Cd.CharIso1 0xA2) HU.@?= True),
        HU.testCase "CharIso1 #u18" (Cd.isValid (Cd.CharIso1 0xFD) HU.@?= True),
        HU.testCase "CharIso1 #u19" (Cd.isValid (Cd.CharIso1 0xFE) HU.@?= True),
        HU.testCase "CharIso1 #u20" (Cd.isValid (Cd.CharIso1 0xFF) HU.@?= True),
        QC.testProperty "CharWin1 #p1" (\cd -> notElem (Cd.roctCW1 cd) [0x81, 0x8D, 0x8F, 0x90, 0x9D] QC.==> Cd.isValid cd),
        QC.testProperty "CharWin1 #p2" (\cd -> elem (Cd.roctCW1 cd) [0x81, 0x8D, 0x8F, 0x90, 0x9D] QC.==> not (Cd.isValid cd)),
        HU.testCase  "CharWin1 #u1" (Cd.isValid (Cd.CharWin1 0x00) HU.@?= True),
        HU.testCase  "CharWin1 #u2" (Cd.isValid (Cd.CharWin1 0x01) HU.@?= True),
        HU.testCase  "CharWin1 #u3" (Cd.isValid (Cd.CharWin1 0x02) HU.@?= True),
        HU.testCase  "CharWin1 #u4" (Cd.isValid (Cd.CharWin1 0x03) HU.@?= True),
        HU.testCase  "CharWin1 #u5" (Cd.isValid (Cd.CharWin1 0x7C) HU.@?= True),
        HU.testCase  "CharWin1 #u6" (Cd.isValid (Cd.CharWin1 0x7E) HU.@?= True),
        HU.testCase  "CharWin1 #u7" (Cd.isValid (Cd.CharWin1 0x7F) HU.@?= True),
        HU.testCase  "CharWin1 #u8" (Cd.isValid (Cd.CharWin1 0x80) HU.@?= True),
        HU.testCase  "CharWin1 #u9" (Cd.isValid (Cd.CharWin1 0x81) HU.@?= False),
        HU.testCase  "CharWin1 #u10" (Cd.isValid (Cd.CharWin1 0x82) HU.@?= True),
        HU.testCase "CharWin1 #u11" (Cd.isValid (Cd.CharWin1 0x83) HU.@?= True),
        HU.testCase "CharWin1 #u12" (Cd.isValid (Cd.CharWin1 0x84) HU.@?= True),
        HU.testCase "CharWin1 #u13" (Cd.isValid (Cd.CharWin1 0x8A) HU.@?= True),
        HU.testCase "CharWin1 #u14" (Cd.isValid (Cd.CharWin1 0x8B) HU.@?= True),
        HU.testCase "CharWin1 #u15" (Cd.isValid (Cd.CharWin1 0x8C) HU.@?= True),
        HU.testCase "CharWin1 #u16" (Cd.isValid (Cd.CharWin1 0x8D) HU.@?= False),
        HU.testCase "CharWin1 #u17" (Cd.isValid (Cd.CharWin1 0x8E) HU.@?= True),
        HU.testCase "CharWin1 #u18" (Cd.isValid (Cd.CharWin1 0x8F) HU.@?= False),
        HU.testCase "CharWin1 #u19" (Cd.isValid (Cd.CharWin1 0x90) HU.@?= False),
        HU.testCase "CharWin1 #u20" (Cd.isValid (Cd.CharWin1 0x91) HU.@?= True),
        HU.testCase "CharWin1 #u21" (Cd.isValid (Cd.CharWin1 0x92) HU.@?= True),
        HU.testCase "CharWin1 #u22" (Cd.isValid (Cd.CharWin1 0x93) HU.@?= True),
        HU.testCase "CharWin1 #u23" (Cd.isValid (Cd.CharWin1 0x9A) HU.@?= True),
        HU.testCase "CharWin1 #u24" (Cd.isValid (Cd.CharWin1 0x9B) HU.@?= True),
        HU.testCase "CharWin1 #u25" (Cd.isValid (Cd.CharWin1 0x9C) HU.@?= True),
        HU.testCase "CharWin1 #u26" (Cd.isValid (Cd.CharWin1 0x9D) HU.@?= False),
        HU.testCase "CharWin1 #u27" (Cd.isValid (Cd.CharWin1 0x9E) HU.@?= True),
        HU.testCase "CharWin1 #u28" (Cd.isValid (Cd.CharWin1 0x9F) HU.@?= True),
        HU.testCase "CharWin1 #u29" (Cd.isValid (Cd.CharWin1 0xA0) HU.@?= True),
        HU.testCase "CharWin1 #u30" (Cd.isValid (Cd.CharWin1 0xFD) HU.@?= True),
        HU.testCase "CharWin1 #u31" (Cd.isValid (Cd.CharWin1 0xFE) HU.@?= True),
        HU.testCase "CharWin1 #u32" (Cd.isValid (Cd.CharWin1 0xFF) HU.@?= True),
        QC.testProperty "Octets.Octet #p1" (\cd -> True QC.==> Cd.isValid (cd :: Oct.Octet)), -- all are valid
        HU.testCase     "Octets.Octet #u1" (Cd.isValid (Cd.CharWin1 0x00) HU.@?= True),
        HU.testCase     "Octets.Octet #u2" (Cd.isValid (Cd.CharWin1 0x01) HU.@?= True),
        HU.testCase     "Octets.Octet #u3" (Cd.isValid (Cd.CharWin1 0x02) HU.@?= True),
        HU.testCase     "Octets.Octet #u4" (Cd.isValid (Cd.CharWin1 0x02) HU.@?= True),
        HU.testCase     "Octets.Octet #u5" (Cd.isValid (Cd.CharWin1 0xFD) HU.@?= True),
        HU.testCase     "Octets.Octet #u6" (Cd.isValid (Cd.CharWin1 0xFE) HU.@?= True),
        HU.testCase     "Octets.Octet #u7" (Cd.isValid (Cd.CharWin1 0xFF) HU.@?= True),
        HU.testCase "Bool #u1" (Cd.isValid False HU.@?= True), -- both and hence all are valid
        HU.testCase "Bool #u2" (Cd.isValid True HU.@?= True)
    ]

testPropHelp1 :: String -> T.TestTree
testPropHelp1 sName =
    QC.testProperty
        sName
        (\cd -> True
            QC.==>
            Cd.isValid cd == isValidCharUtf8' cd )

{-  * validated: ✅
        * completeness: ✅
        * independence: ✅
        * edge cases  : ✅
        * conform doc.: ✅ -}
tgUnitcdReplacement :: T.TestTree
tgUnitcdReplacement =
    T.testGroup
    "cdReplacement"
    [
        HU.testCase "TH.Stub #u1" ((Cd.cdReplacement :: TH.Stub) HU.@?= TH.Stub 13),
        HU.testCase "Char #u1" ((Cd.cdReplacement :: Char) HU.@?= '\xFFFD'),
        HU.testCase "CharUtf8 #u1" ((Cd.cdReplacement :: Cd.CharUtf8) HU.@?= Cd.CU8ThreeBytes 0b11101111 0b10111111 0b10111101),
        HU.testCase "CharIso1 #u1" ((Cd.cdReplacement :: Cd.CharIso1) HU.@?= Cd.CharIso1 0x3F),
        HU.testCase "CharWin1 #u1" ((Cd.cdReplacement :: Cd.CharWin1) HU.@?= Cd.CharWin1 0x3F),
        HU.testCase "Octets.Octet #u1" ((Cd.cdReplacement :: Oct.Octet) HU.@?= 0xFF),
        HU.testCase "Bool #u1" ((Cd.cdReplacement :: Bool) HU.@?= True)
    ]

{-  * validated: ✅
        * completeness: ✅
        * independence: ✅
        * edge cases  : ✅
        * conform doc.: ✅ -}
tgUnitniOrd :: T.TestTree
tgUnitniOrd =
    T.testGroup
    "niOrd"
    [
        QC.testProperty "Stub #p1" (\cd -> TH.rniStb (cd :: TH.Stub) < 0 || TH.rniStb cd > 99 QC.==> Cd.niOrd cd == (-1)),
        QC.testProperty "Stub #p2" (\cd -> TH.rniStb (cd :: TH.Stub) >= 0 && TH.rniStb cd <= 49 QC.==> Cd.niOrd cd == fromIntegral (TH.rniStb cd)),
        QC.testProperty "Stub #p3" (\cd -> TH.rniStb (cd :: TH.Stub) >= 50 && TH.rniStb cd <= 99 QC.==> Cd.niOrd cd == fromIntegral (TH.rniStb cd) - 50),
        HU.testCase  "Stub #u1" (Cd.niOrd (TH.Stub (-2)) HU.@?= (-1)),
        HU.testCase  "Stub #u2" (Cd.niOrd (TH.Stub (-1)) HU.@?= (-1)),
        HU.testCase  "Stub #u3" (Cd.niOrd (TH.Stub 0) HU.@?= 0),
        HU.testCase  "Stub #u4" (Cd.niOrd (TH.Stub 1) HU.@?= 1),
        HU.testCase  "Stub #u5" (Cd.niOrd (TH.Stub 2) HU.@?= 2),
        HU.testCase  "Stub #u6" (Cd.niOrd (TH.Stub 3) HU.@?= 3),
        HU.testCase  "Stub #u7" (Cd.niOrd (TH.Stub 4) HU.@?= 4),
        HU.testCase  "Stub #u8" (Cd.niOrd (TH.Stub 46) HU.@?= 46),
        HU.testCase  "Stub #u9" (Cd.niOrd (TH.Stub 47) HU.@?= 47),
        HU.testCase "Stub #u10" (Cd.niOrd (TH.Stub 48) HU.@?= 48),
        HU.testCase "Stub #u11" (Cd.niOrd (TH.Stub 49) HU.@?= 49),
        HU.testCase "Stub #u12" (Cd.niOrd (TH.Stub 50) HU.@?= 0),
        HU.testCase "Stub #u13" (Cd.niOrd (TH.Stub 51) HU.@?= 1),
        HU.testCase "Stub #u14" (Cd.niOrd (TH.Stub 52) HU.@?= 2),
        HU.testCase "Stub #u15" (Cd.niOrd (TH.Stub 97) HU.@?= 47),
        HU.testCase "Stub #u16" (Cd.niOrd (TH.Stub 98) HU.@?= 48),
        HU.testCase "Stub #u17" (Cd.niOrd (TH.Stub 99) HU.@?= 49),
        HU.testCase "Stub #u18" (Cd.niOrd (TH.Stub 100) HU.@?= (-1)),
        HU.testCase "Stub #u19" (Cd.niOrd (TH.Stub 101) HU.@?= (-1)),
        QC.testProperty "Char #p1" (\cd -> True QC.==> Cd.niOrd (TH.fromT' (cd :: TH.T' Char)) == fromIntegral (Chr.ord (TH.fromT' cd))),
        HU.testCase  "Char #u1" (Cd.niOrd '\x0' HU.@?= 0x0),
        HU.testCase  "Char #u2" (Cd.niOrd '\x1' HU.@?= 0x1),
        HU.testCase  "Char #u3" (Cd.niOrd '\x2' HU.@?= 0x2),
        HU.testCase  "Char #u4" (Cd.niOrd '\x3' HU.@?= 0x3),
        HU.testCase  "Char #u5" (Cd.niOrd '\x7D' HU.@?= 0x7D),
        HU.testCase  "Char #u6" (Cd.niOrd '\x7E' HU.@?= 0x7E),
        HU.testCase  "Char #u7" (Cd.niOrd '\x7F' HU.@?= 0x7F),
        HU.testCase  "Char #u8" (Cd.niOrd '\x80' HU.@?= 0x80),
        HU.testCase  "Char #u9" (Cd.niOrd '\x81' HU.@?= 0x81),
        HU.testCase "Char #u10" (Cd.niOrd '\x82' HU.@?= 0x82),
        HU.testCase "Char #u11" (Cd.niOrd '\xFD' HU.@?= 0xFD),
        HU.testCase "Char #u12" (Cd.niOrd '\xFE' HU.@?= 0xFE),
        HU.testCase "Char #u13" (Cd.niOrd '\xFF' HU.@?= 0xFF),
        HU.testCase "Char #u14" (Cd.niOrd '\x80' HU.@?= 0x80),
        HU.testCase "Char #u15" (Cd.niOrd '\x81' HU.@?= 0x81),
        HU.testCase "Char #u16" (Cd.niOrd '\x82' HU.@?= 0x82),
        HU.testCase "Char #u17" (Cd.niOrd '\x7FD' HU.@?= 0x7FD),
        HU.testCase "Char #u18" (Cd.niOrd '\x7FE' HU.@?= 0x7FE),
        HU.testCase "Char #u19" (Cd.niOrd '\x7FF' HU.@?= 0x7FF),
        HU.testCase "Char #u20" (Cd.niOrd '\x800' HU.@?= 0x800),
        HU.testCase "Char #u21" (Cd.niOrd '\x801' HU.@?= 0x801),
        HU.testCase "Char #u22" (Cd.niOrd '\x802' HU.@?= 0x802),
        HU.testCase "Char #u23" (Cd.niOrd '\xFFFD' HU.@?= 0xFFFD),
        HU.testCase "Char #u24" (Cd.niOrd '\xFFFE' HU.@?= 0xFFFE),
        HU.testCase "Char #u25" (Cd.niOrd '\xFFFF' HU.@?= 0xFFFF),
        HU.testCase "Char #u26" (Cd.niOrd '\x10000' HU.@?= 0x10000),
        HU.testCase "Char #u27" (Cd.niOrd '\x10001' HU.@?= 0x10001),
        HU.testCase "Char #u28" (Cd.niOrd '\x10002' HU.@?= 0x10002),
        HU.testCase "Char #u29" (Cd.niOrd '\x10FFFD' HU.@?= 0x10FFFD),
        HU.testCase "Char #u30" (Cd.niOrd '\x10FFFE' HU.@?= 0x10FFFE),
        HU.testCase "Char #u31" (Cd.niOrd '\x10FFFF' HU.@?= 0x10FFFF),
        HU.testCase  "CharUtf8 #u1" (Cd.niOrd (Cd.CU8OneByte 0b00000000) HU.@?= 0x0),
        HU.testCase  "CharUtf8 #u2" (Cd.niOrd (Cd.CU8OneByte 0b00000001) HU.@?= 0x1),
        HU.testCase  "CharUtf8 #u3" (Cd.niOrd (Cd.CU8OneByte 0b00000010) HU.@?= 0x2),
        HU.testCase  "CharUtf8 #u4" (Cd.niOrd (Cd.CU8OneByte 0b01111101) HU.@?= 0x7D),
        HU.testCase  "CharUtf8 #u5" (Cd.niOrd (Cd.CU8OneByte 0b01111110) HU.@?= 0x7E),
        HU.testCase  "CharUtf8 #u6" (Cd.niOrd (Cd.CU8OneByte 0b01111111) HU.@?= 0x7F),
        HU.testCase  "CharUtf8 #u7" (Cd.niOrd (Cd.CU8OneByte 0b10000000) HU.@?= (-1)),
        HU.testCase  "CharUtf8 #u8" (Cd.niOrd (Cd.CU8OneByte 0b10000001) HU.@?= (-1)),
        HU.testCase  "CharUtf8 #u9" (Cd.niOrd (Cd.CU8OneByte 0b10000010) HU.@?= (-1)),
        HU.testCase "CharUtf8 #u10" (Cd.niOrd (Cd.CU8OneByte 0b11111101) HU.@?= (-1)),
        HU.testCase "CharUtf8 #u11" (Cd.niOrd (Cd.CU8OneByte 0b11111110) HU.@?= (-1)),
        HU.testCase "CharUtf8 #u12" (Cd.niOrd (Cd.CU8OneByte 0b11111111) HU.@?= (-1)),
        HU.testCase "CharUtf8 #u13" (Cd.niOrd (Cd.CU8TwoBytes 0b00000000 0b00000000) HU.@?= (-1)),
        HU.testCase "CharUtf8 #u14" (Cd.niOrd (Cd.CU8TwoBytes 0b00000000 0b10000000) HU.@?= (-1)),
        HU.testCase "CharUtf8 #u15" (Cd.niOrd (Cd.CU8TwoBytes 0b11000000 0b00000000) HU.@?= (-1)),
        HU.testCase "CharUtf8 #u16" (Cd.niOrd (Cd.CU8TwoBytes 0b11000000 0b10000000) HU.@?= 0x0),
        HU.testCase "CharUtf8 #u17" (Cd.niOrd (Cd.CU8TwoBytes 0b11000000 0b10000001) HU.@?= 0x1),
        HU.testCase "CharUtf8 #u18" (Cd.niOrd (Cd.CU8TwoBytes 0b11000000 0b10000010) HU.@?= 0x2),
        HU.testCase "CharUtf8 #u19" (Cd.niOrd (Cd.CU8TwoBytes 0b11011111 0b10111101) HU.@?= 0x7FD),
        HU.testCase "CharUtf8 #u20" (Cd.niOrd (Cd.CU8TwoBytes 0b11011111 0b10111110) HU.@?= 0x7FE),
        HU.testCase "CharUtf8 #u21" (Cd.niOrd (Cd.CU8TwoBytes 0b11011111 0b10111111) HU.@?= 0x7FF),
        HU.testCase "CharUtf8 #u22" (Cd.niOrd (Cd.CU8TwoBytes 0b11111111 0b11111101) HU.@?= (-1)),
        HU.testCase "CharUtf8 #u23" (Cd.niOrd (Cd.CU8TwoBytes 0b11111111 0b11111110) HU.@?= (-1)),
        HU.testCase "CharUtf8 #u24" (Cd.niOrd (Cd.CU8TwoBytes 0b11111111 0b11111111) HU.@?= (-1)),
        HU.testCase "CharUtf8 #u25" (Cd.niOrd (Cd.CU8ThreeBytes 0b00000000 0b00000000 0b00000000) HU.@?= (-1)),
        HU.testCase "CharUtf8 #u26" (Cd.niOrd (Cd.CU8ThreeBytes 0b00000000 0b10000000 0b10000000) HU.@?= (-1)),
        HU.testCase "CharUtf8 #u27" (Cd.niOrd (Cd.CU8ThreeBytes 0b11100000 0b00000000 0b10000000) HU.@?= (-1)),
        HU.testCase "CharUtf8 #u28" (Cd.niOrd (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b00000000) HU.@?= (-1)),
        HU.testCase "CharUtf8 #u29" (Cd.niOrd (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b10000000) HU.@?= 0x0),
        HU.testCase "CharUtf8 #u30" (Cd.niOrd (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b10000001) HU.@?= 0x1),
        HU.testCase "CharUtf8 #u31" (Cd.niOrd (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b10000010) HU.@?= 0x2),
        HU.testCase "CharUtf8 #u32" (Cd.niOrd (Cd.CU8ThreeBytes 0b11101111 0b10111111 0b10111101) HU.@?= 0xFFFD),
        HU.testCase "CharUtf8 #u33" (Cd.niOrd (Cd.CU8ThreeBytes 0b11101111 0b10111111 0b10111110) HU.@?= 0xFFFE),
        HU.testCase "CharUtf8 #u34" (Cd.niOrd (Cd.CU8ThreeBytes 0b11101111 0b10111111 0b10111111) HU.@?= 0xFFFF),
        HU.testCase "CharUtf8 #u35" (Cd.niOrd (Cd.CU8ThreeBytes 0b11111111 0b11111111 0b11111101) HU.@?= (-1)),
        HU.testCase "CharUtf8 #u36" (Cd.niOrd (Cd.CU8ThreeBytes 0b11111111 0b11111111 0b11111110) HU.@?= (-1)),
        HU.testCase "CharUtf8 #u37" (Cd.niOrd (Cd.CU8ThreeBytes 0b11111111 0b11111111 0b11111111) HU.@?= (-1)),
        HU.testCase "CharUtf8 #u38" (Cd.niOrd (Cd.CU8FourBytes 0b00000000 0b00000000 0b00000000 0b00000000) HU.@?= (-1)),
        HU.testCase "CharUtf8 #u39" (Cd.niOrd (Cd.CU8FourBytes 0b00000000 0b00000000 0b00000000 0b00000001) HU.@?= (-1)),
        HU.testCase "CharUtf8 #u40" (Cd.niOrd (Cd.CU8FourBytes 0b00000000 0b00000000 0b00000000 0b00000010) HU.@?= (-1)),
        HU.testCase "CharUtf8 #u41" (Cd.niOrd (Cd.CU8FourBytes 0b00000000 0b10000000 0b10000000 0b10000000) HU.@?= (-1)),
        HU.testCase "CharUtf8 #u42" (Cd.niOrd (Cd.CU8FourBytes 0b11110000 0b00000000 0b10000000 0b10000000) HU.@?= (-1)),
        HU.testCase "CharUtf8 #u43" (Cd.niOrd (Cd.CU8FourBytes 0b11110000 0b10000000 0b00000000 0b10000000) HU.@?= (-1)),
        HU.testCase "CharUtf8 #u44" (Cd.niOrd (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b00000000) HU.@?= (-1)),
        HU.testCase "CharUtf8 #u45" (Cd.niOrd (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b10000000) HU.@?= 0x0),
        HU.testCase "CharUtf8 #u46" (Cd.niOrd (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b10000001) HU.@?= 0x1),
        HU.testCase "CharUtf8 #u47" (Cd.niOrd (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b10000010) HU.@?= 0x2),
        HU.testCase "CharUtf8 #u48" (Cd.niOrd (Cd.CU8FourBytes 0b11110100 0b10001111 0b10111111 0b10111101) HU.@?= 0x10FFFD),
        HU.testCase "CharUtf8 #u49" (Cd.niOrd (Cd.CU8FourBytes 0b11110100 0b10001111 0b10111111 0b10111110) HU.@?= 0x10FFFE),
        HU.testCase "CharUtf8 #u50" (Cd.niOrd (Cd.CU8FourBytes 0b11110100 0b10001111 0b10111111 0b10111111) HU.@?= 0x10FFFF),
        QC.testProperty "CharIso1 #p1" (\cd -> Cd.roctCI1 cd >= 0x00 && Cd.roctCI1 cd <= 0x7F QC.==> Cd.niOrd cd == fromIntegral (Cd.roctCI1 cd)),
        QC.testProperty "CharIso1 #p2" (\cd -> Cd.roctCI1 cd >= 0x80 && Cd.roctCI1 cd <= 0x9F QC.==> Cd.niOrd cd == (-1)),
        QC.testProperty "CharIso1 #p3" (\cd -> Cd.roctCI1 cd >= 0xA0 && Cd.roctCI1 cd <= 0xFF QC.==> Cd.niOrd cd == fromIntegral (Cd.roctCI1 cd) - 0x20),
        HU.testCase  "CharIso1 #u1" (Cd.niOrd (Cd.CharIso1 0x00) HU.@?= 0x00),
        HU.testCase  "CharIso1 #u2" (Cd.niOrd (Cd.CharIso1 0x01) HU.@?= 0x01),
        HU.testCase  "CharIso1 #u3" (Cd.niOrd (Cd.CharIso1 0x02) HU.@?= 0x02),
        HU.testCase  "CharIso1 #u4" (Cd.niOrd (Cd.CharIso1 0x03) HU.@?= 0x03),
        HU.testCase  "CharIso1 #u5" (Cd.niOrd (Cd.CharIso1 0x04) HU.@?= 0x04),
        HU.testCase  "CharIso1 #u6" (Cd.niOrd (Cd.CharIso1 0x7C) HU.@?= 0x7C),
        HU.testCase  "CharIso1 #u7" (Cd.niOrd (Cd.CharIso1 0x7D) HU.@?= 0x7D),
        HU.testCase  "CharIso1 #u8" (Cd.niOrd (Cd.CharIso1 0x7E) HU.@?= 0x7E),
        HU.testCase  "CharIso1 #u9" (Cd.niOrd (Cd.CharIso1 0x7F) HU.@?= 0x7F),
        HU.testCase "CharIso1 #u10" (Cd.niOrd (Cd.CharIso1 0x80) HU.@?= (-1)),
        HU.testCase "CharIso1 #u11" (Cd.niOrd (Cd.CharIso1 0x81) HU.@?= (-1)),
        HU.testCase "CharIso1 #u12" (Cd.niOrd (Cd.CharIso1 0x82) HU.@?= (-1)),
        HU.testCase "CharIso1 #u13" (Cd.niOrd (Cd.CharIso1 0x9D) HU.@?= (-1)),
        HU.testCase "CharIso1 #u14" (Cd.niOrd (Cd.CharIso1 0x9E) HU.@?= (-1)),
        HU.testCase "CharIso1 #u15" (Cd.niOrd (Cd.CharIso1 0x9F) HU.@?= (-1)),
        HU.testCase "CharIso1 #u16" (Cd.niOrd (Cd.CharIso1 0xA0) HU.@?= 0x80),
        HU.testCase "CharIso1 #u17" (Cd.niOrd (Cd.CharIso1 0xA1) HU.@?= 0x81),
        HU.testCase "CharIso1 #u18" (Cd.niOrd (Cd.CharIso1 0xA2) HU.@?= 0x82),
        HU.testCase "CharIso1 #u19" (Cd.niOrd (Cd.CharIso1 0xFD) HU.@?= 0xDD),
        HU.testCase "CharIso1 #u20" (Cd.niOrd (Cd.CharIso1 0xFE) HU.@?= 0xDE),
        HU.testCase "CharIso1 #u21" (Cd.niOrd (Cd.CharIso1 0xFF) HU.@?= 0xDF),
        QC.testProperty "CharWin1 #p1" (\cd -> True QC.==> Cd.niOrd (cd :: Cd.CharWin1) == niOrdCW1Char' cd),
        HU.testCase  "CharWin1 #u1" (Cd.niOrd (Cd.CharWin1 0x00) HU.@?= 0x00),
        HU.testCase  "CharWin1 #u2" (Cd.niOrd (Cd.CharWin1 0x01) HU.@?= 0x01),
        HU.testCase  "CharWin1 #u3" (Cd.niOrd (Cd.CharWin1 0x02) HU.@?= 0x02),
        HU.testCase  "CharWin1 #u4" (Cd.niOrd (Cd.CharWin1 0x03) HU.@?= 0x03),
        HU.testCase  "CharWin1 #u6" (Cd.niOrd (Cd.CharWin1 0x7E) HU.@?= 0x7E),
        HU.testCase  "CharWin1 #u7" (Cd.niOrd (Cd.CharWin1 0x7F) HU.@?= 0x7F),
        HU.testCase  "CharWin1 #u8" (Cd.niOrd (Cd.CharWin1 0x80) HU.@?= 0x80),
        HU.testCase  "CharWin1 #u9" (Cd.niOrd (Cd.CharWin1 0x81) HU.@?= (-1)),
        HU.testCase "CharWin1 #u10" (Cd.niOrd (Cd.CharWin1 0x82) HU.@?= 0x81),
        HU.testCase "CharWin1 #u11" (Cd.niOrd (Cd.CharWin1 0x83) HU.@?= 0x82),
        HU.testCase "CharWin1 #u12" (Cd.niOrd (Cd.CharWin1 0x84) HU.@?= 0x83),
        HU.testCase "CharWin1 #u13" (Cd.niOrd (Cd.CharWin1 0x8A) HU.@?= 0x89),
        HU.testCase "CharWin1 #u14" (Cd.niOrd (Cd.CharWin1 0x8B) HU.@?= 0x8A),
        HU.testCase "CharWin1 #u15" (Cd.niOrd (Cd.CharWin1 0x8C) HU.@?= 0x8B),
        HU.testCase "CharWin1 #u16" (Cd.niOrd (Cd.CharWin1 0x8D) HU.@?= (-1)),
        HU.testCase "CharWin1 #u17" (Cd.niOrd (Cd.CharWin1 0x8E) HU.@?= 0x8C),
        HU.testCase "CharWin1 #u18" (Cd.niOrd (Cd.CharWin1 0x8F) HU.@?= (-1)),
        HU.testCase "CharWin1 #u19" (Cd.niOrd (Cd.CharWin1 0x90) HU.@?= (-1)),
        HU.testCase "CharWin1 #u20" (Cd.niOrd (Cd.CharWin1 0x91) HU.@?= 0x8D),
        HU.testCase "CharWin1 #u21" (Cd.niOrd (Cd.CharWin1 0x92) HU.@?= 0x8E),
        HU.testCase "CharWin1 #u22" (Cd.niOrd (Cd.CharWin1 0x93) HU.@?= 0x8F),
        HU.testCase "CharWin1 #u23" (Cd.niOrd (Cd.CharWin1 0x9A) HU.@?= 0x96),
        HU.testCase "CharWin1 #u24" (Cd.niOrd (Cd.CharWin1 0x9B) HU.@?= 0x97),
        HU.testCase "CharWin1 #u25" (Cd.niOrd (Cd.CharWin1 0x9C) HU.@?= 0x98),
        HU.testCase "CharWin1 #u26" (Cd.niOrd (Cd.CharWin1 0x9D) HU.@?= (-1)),
        HU.testCase "CharWin1 #u27" (Cd.niOrd (Cd.CharWin1 0x9E) HU.@?= 0x99),
        HU.testCase "CharWin1 #u28" (Cd.niOrd (Cd.CharWin1 0x9F) HU.@?= 0x9A),
        HU.testCase "CharWin1 #u29" (Cd.niOrd (Cd.CharWin1 0xA0) HU.@?= 0x9B),
        HU.testCase "CharWin1 #u30" (Cd.niOrd (Cd.CharWin1 0xFD) HU.@?= 0xF8),
        HU.testCase "CharWin1 #u31" (Cd.niOrd (Cd.CharWin1 0xFE) HU.@?= 0xF9),
        HU.testCase "CharWin1 #u32" (Cd.niOrd (Cd.CharWin1 0xFF) HU.@?= 0xFA),
        QC.testProperty "Octets.Octet #p1" (\cd -> True QC.==> Cd.niOrd (cd :: Oct.Octet) == fromIntegral cd),
        HU.testCase  "Octets.Octet #u1" (Cd.niOrd (0x00 :: Oct.Octet) HU.@?= 0x00),
        HU.testCase  "Octets.Octet #u2" (Cd.niOrd (0x01 :: Oct.Octet) HU.@?= 0x01),
        HU.testCase  "Octets.Octet #u3" (Cd.niOrd (0x02 :: Oct.Octet) HU.@?= 0x02),
        HU.testCase  "Octets.Octet #u4" (Cd.niOrd (0x03 :: Oct.Octet) HU.@?= 0x03),
        HU.testCase  "Octets.Octet #u5" (Cd.niOrd (0xFD :: Oct.Octet) HU.@?= 0xFD),
        HU.testCase  "Octets.Octet #u6" (Cd.niOrd (0xFE :: Oct.Octet) HU.@?= 0xFE),
        HU.testCase  "Octets.Octet #u7" (Cd.niOrd (0xFF :: Oct.Octet) HU.@?= 0xFF),
        HU.testCase  "Bool #u1" (Cd.niOrd False HU.@?= 0),
        HU.testCase  "Bool #u2" (Cd.niOrd True HU.@?= 1)
    ]

{-  * validated: ✅
        * completeness: ✅
        * independence: ✅
        * edge cases  : ✅
        * conform doc.: ✅ -}
tgUnitcdFromInteger :: T.TestTree
tgUnitcdFromInteger =
    T.testGroup
    "cdFromInteger"
    [
        HU.testCase  "Stub #u1" (fmap Cd.cdFromInteger [-2..51] HU.@?= fmap TH.Stub ([13,13]++[0..49]++[13,13])),
        QC.testProperty
            "Char #p1"
            (\njChar ->
                True QC.==>
                Cd.cdFromInteger (TH.niFromIntCharPlus njChar) ==
                    (if TH.njFromIntCharPlus njChar >= 0 && TH.njFromIntCharPlus njChar <= 0x10FFFF
                        then Chr.chr (TH.njFromIntCharPlus njChar)
                        else Cd.cdReplacement)),
        HU.testCase  "Char #u1" (Cd.cdFromInteger (-3) HU.@?= (Cd.cdReplacement :: Char)),
        HU.testCase  "Char #u2" (Cd.cdFromInteger (-2) HU.@?= (Cd.cdReplacement :: Char)),
        HU.testCase  "Char #u3" (Cd.cdFromInteger (-1) HU.@?= (Cd.cdReplacement :: Char)),
        HU.testCase  "Char #u4" (Cd.cdFromInteger 0 HU.@?= '\0'),
        HU.testCase  "Char #u5" (Cd.cdFromInteger 0x01 HU.@?= '\x01'),
        HU.testCase  "Char #u6" (Cd.cdFromInteger 0x02 HU.@?= '\x02'),
        HU.testCase  "Char #u7" (Cd.cdFromInteger 0x10FFFD HU.@?= '\x10FFFD'),
        HU.testCase  "Char #u8" (Cd.cdFromInteger 0x10FFFE HU.@?= '\x10FFFE'),
        HU.testCase  "Char #u9" (Cd.cdFromInteger 0x10FFFF HU.@?= '\x10FFFF'),
        QC.testProperty
            "CharUtf8 #p1"
            (\njChar ->
                True QC.==>
                Cd.cdFromInteger (TH.niFromIntCharPlus njChar) ==
                    (if TH.njFromIntCharPlus njChar >= 0 && TH.njFromIntCharPlus njChar <= 0x10FFFF
                        then cu8FromInt' (TH.njFromIntCharPlus njChar)
                        else Cd.cdReplacement)),
        HU.testCase  "CharUtf8 #u1" (Cd.cdFromInteger (-3) HU.@?= (Cd.cdReplacement :: Cd.CharUtf8)),
        HU.testCase  "CharUtf8 #u2" (Cd.cdFromInteger (-2) HU.@?= (Cd.cdReplacement :: Cd.CharUtf8)),
        HU.testCase  "CharUtf8 #u3" (Cd.cdFromInteger (-1) HU.@?= (Cd.cdReplacement :: Cd.CharUtf8)),
        HU.testCase  "CharUtf8 #u4" (fmap Cd.cdFromInteger [0..0x7F] HU.@?= fmap Cd.CU8OneByte [0..0x7F]),
        HU.testCase  "CharUtf8 #u5" (Cd.cdFromInteger 0x80 HU.@?= Cd.CU8TwoBytes 0b11000010 0b10000000),
        HU.testCase  "CharUtf8 #u6" (Cd.cdFromInteger 0x81 HU.@?= Cd.CU8TwoBytes 0b11000010 0b10000001),
        HU.testCase  "CharUtf8 #u7" (Cd.cdFromInteger 0x82 HU.@?= Cd.CU8TwoBytes 0b11000010 0b10000010),
        HU.testCase  "CharUtf8 #u8" (Cd.cdFromInteger 0x7FD HU.@?= Cd.CU8TwoBytes 0b11011111 0b10111101),
        HU.testCase  "CharUtf8 #u9" (Cd.cdFromInteger 0x7FE HU.@?= Cd.CU8TwoBytes 0b11011111 0b10111110),
        HU.testCase "CharUtf8 #u10" (Cd.cdFromInteger 0x7FF HU.@?= Cd.CU8TwoBytes 0b11011111 0b10111111),
        HU.testCase "CharUtf8 #u11" (Cd.cdFromInteger 0x800 HU.@?= Cd.CU8ThreeBytes 0b11100000 0b10100000 0b10000000),
        HU.testCase "CharUtf8 #u12" (Cd.cdFromInteger 0x801 HU.@?= Cd.CU8ThreeBytes 0b11100000 0b10100000 0b10000001),
        HU.testCase "CharUtf8 #u13" (Cd.cdFromInteger 0x802 HU.@?= Cd.CU8ThreeBytes 0b11100000 0b10100000 0b10000010),
        HU.testCase "CharUtf8 #u14" (Cd.cdFromInteger 0xFFFD HU.@?= Cd.CU8ThreeBytes 0b11101111 0b10111111 0b10111101),
        HU.testCase "CharUtf8 #u15" (Cd.cdFromInteger 0xFFFE HU.@?= Cd.CU8ThreeBytes 0b11101111 0b10111111 0b10111110),
        HU.testCase "CharUtf8 #u16" (Cd.cdFromInteger 0xFFFF HU.@?= Cd.CU8ThreeBytes 0b11101111 0b10111111 0b10111111),
        HU.testCase "CharUtf8 #u17" (Cd.cdFromInteger 0x10000 HU.@?= Cd.CU8FourBytes 0b11110000 0b10010000 0b10000000 0b10000000),
        HU.testCase "CharUtf8 #u18" (Cd.cdFromInteger 0x10001 HU.@?= Cd.CU8FourBytes 0b11110000 0b10010000 0b10000000 0b10000001),
        HU.testCase "CharUtf8 #u19" (Cd.cdFromInteger 0x10002 HU.@?= Cd.CU8FourBytes 0b11110000 0b10010000 0b10000000 0b10000010),
        HU.testCase "CharUtf8 #u20" (Cd.cdFromInteger 0x10FFFD HU.@?= Cd.CU8FourBytes 0b11110100 0b10001111 0b10111111 0b10111101),
        HU.testCase "CharUtf8 #u21" (Cd.cdFromInteger 0x10FFFE HU.@?= Cd.CU8FourBytes 0b11110100 0b10001111 0b10111111 0b10111110),
        HU.testCase "CharUtf8 #u22" (Cd.cdFromInteger 0x10FFFF HU.@?= Cd.CU8FourBytes 0b11110100 0b10001111 0b10111111 0b10111111),
        HU.testCase "CharUtf8 #u23" (Cd.cdFromInteger 0x110000 HU.@?= (Cd.cdReplacement :: Cd.CharUtf8)),
        HU.testCase "CharUtf8 #u24" (Cd.cdFromInteger 0x110001 HU.@?= (Cd.cdReplacement :: Cd.CharUtf8)),
        HU.testCase "CharUtf8 #u25" (Cd.cdFromInteger 0x110002 HU.@?= (Cd.cdReplacement :: Cd.CharUtf8)),
        HU.testCase  "CharIso1 #u1" (fmap Cd.cdFromInteger [-100..(-1)] HU.@?= replicate 100 (Cd.cdReplacement :: Cd.CharIso1)),
        HU.testCase  "CharIso1 #u2" (fmap Cd.cdFromInteger [0..0x7F] HU.@?= fmap Cd.CharIso1 [0..0x7F]),
        HU.testCase  "CharIso1 #u3" (fmap Cd.cdFromInteger [0x80..0xDF] HU.@?= fmap Cd.CharIso1 [0xA0..0xFF]),
        HU.testCase  "CharIso1 #u4" (fmap Cd.cdFromInteger [0xE0..0xFF] HU.@?= replicate 0x20 (Cd.cdReplacement :: Cd.CharIso1)),
        HU.testCase  "CharWin1 #u1" (fmap Cd.cdFromInteger [-100..(-1)] HU.@?= replicate 100 (Cd.cdReplacement :: Cd.CharWin1)),
        HU.testCase  "CharWin1 #u2" (fmap Cd.cdFromInteger [0..0x80] HU.@?= fmap Cd.CharWin1 [0..0x80]),
        HU.testCase  "CharWin1 #u3" (fmap Cd.cdFromInteger [0x81..0x8B] HU.@?= fmap Cd.CharWin1 [0x82..0x8C]),
        HU.testCase  "CharWin1 #u4" (fmap Cd.cdFromInteger [0x8C] HU.@?= fmap Cd.CharWin1 [0x8E]),
        HU.testCase  "CharWin1 #u5" (fmap Cd.cdFromInteger [0x8D..0x98] HU.@?= fmap Cd.CharWin1 [0x91..0x9C]),
        HU.testCase  "CharWin1 #u6" (fmap Cd.cdFromInteger [0x99..0xFA] HU.@?= fmap Cd.CharWin1 [0x9E..0xFF]),
        HU.testCase  "CharWin1 #u7" (fmap Cd.cdFromInteger [0xFB..0x1FA] HU.@?= replicate 0x100 (Cd.cdReplacement :: Cd.CharWin1)),
        HU.testCase  "Octets.Octet #u1" (fmap Cd.cdFromInteger [-100..(-1)] HU.@?= replicate 100 (Cd.cdReplacement :: Oct.Octet)),
        HU.testCase  "Octets.Octet #u2" (fmap Cd.cdFromInteger [0..0xFF] HU.@?= ([0..0xFF] :: [Oct.Octet])),
        HU.testCase  "Octets.Octet #u3" (fmap Cd.cdFromInteger [0x100..0x1FF] HU.@?= replicate 0x100 (Cd.cdReplacement :: Oct.Octet)),
        HU.testCase  "Bool #u1" (fmap Cd.cdFromInteger [-100..(-1)] HU.@?= replicate 100 (Cd.cdReplacement :: Bool)),
        HU.testCase  "Bool #u2" (fmap Cd.cdFromInteger [0,1] HU.@?= [False,True]),
        HU.testCase  "Bool #u3" (fmap Cd.cdFromInteger [2..101] HU.@?= replicate 100 (Cd.cdReplacement :: Bool))
    ]

{-  * validated: ✅
        * completeness: ✅
        * independence: ✅
        * edge cases  : ✅
        * conform doc.: ✅ -}
tgUnitniMinOrd :: T.TestTree
tgUnitniMinOrd =
    T.testGroup
    "niMinOrd"
    [
        HU.testCase  "Stub #u1" (Cd.niMinOrd (Cd.Px :: Cd.Px TH.Stub) HU.@?= 0),
        HU.testCase  "Char #u1" (Cd.niMinOrd (Cd.Px :: Cd.Px Char) HU.@?= 0),
        HU.testCase  "CharUtf8 #u1" (Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharUtf8) HU.@?= 0),
        HU.testCase  "CharIso1 #u1" (Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharIso1) HU.@?= 0),
        HU.testCase  "CharWin1 #u1" (Cd.niMinOrd (Cd.Px :: Cd.Px Cd.CharWin1) HU.@?= 0),
        HU.testCase  "Octets.Octet #u1" (Cd.niMinOrd (Cd.Px :: Cd.Px Oct.Octet) HU.@?= 0),
        HU.testCase  "Bool #u1" (Cd.niMinOrd (Cd.Px :: Cd.Px Bool) HU.@?= 0)
    ]

{-  * validated: ✅
        * completeness: ✅
        * independence: ✅
        * edge cases  : ✅
        * conform doc.: ✅ -}
tgUnitniMaxOrd :: T.TestTree
tgUnitniMaxOrd =
    T.testGroup
    "niMaxOrd"
    [
        HU.testCase  "Stub #u1" (Cd.niMaxOrd (Cd.Px :: Cd.Px TH.Stub) HU.@?= 49),
        HU.testCase  "Char #u1" (Cd.niMaxOrd (Cd.Px :: Cd.Px Char) HU.@?= 0x10FFFF),
        HU.testCase  "CharUtf8 #u1" (Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharUtf8) HU.@?= 0x10FFFF),
        HU.testCase  "CharIso1 #u1" (Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharIso1) HU.@?= 0xFF - 0x20),
        HU.testCase  "CharWin1 #u1" (Cd.niMaxOrd (Cd.Px :: Cd.Px Cd.CharWin1) HU.@?= 0xFF - 5),
        HU.testCase  "Octets.Octet #u1" (Cd.niMaxOrd (Cd.Px :: Cd.Px Oct.Octet) HU.@?= 0xFF),
        HU.testCase  "Bool #u1" (Cd.niMaxOrd (Cd.Px :: Cd.Px Bool) HU.@?= 1)
    ]

{-  * validated: ✅
        * completeness: ✅
        * independence: ✅
          * NOTE: The test depends directly or indirectly on cdReplacement, but cdReplacement is completely tested for all types.
        * edge cases  : ✅
        * conform doc.: ✅ -}
tgUnitcdNormalise :: T.TestTree
tgUnitcdNormalise =
    T.testGroup
        "cdNormalise"
        [
            QC.testProperty "Stub #p1" (\cd -> True QC.==> Cd.cdNormalise cd == stbNormalise' cd),
            HU.testCase  "Stub #u1" (Cd.cdNormalise (TH.Stub (-3)) HU.@?= TH.Stub 13),
            HU.testCase  "Stub #u2" (Cd.cdNormalise (TH.Stub (-2)) HU.@?= TH.Stub 13),
            HU.testCase  "Stub #u3" (Cd.cdNormalise (TH.Stub (-1)) HU.@?= TH.Stub 13),
            HU.testCase  "Stub #u4" (Cd.cdNormalise (TH.Stub 0) HU.@?= TH.Stub 0),
            HU.testCase  "Stub #u5" (Cd.cdNormalise (TH.Stub 1) HU.@?= TH.Stub 1),
            HU.testCase  "Stub #u6" (Cd.cdNormalise (TH.Stub 2) HU.@?= TH.Stub 2),
            HU.testCase  "Stub #u7" (Cd.cdNormalise (TH.Stub 47) HU.@?= TH.Stub 47),
            HU.testCase  "Stub #u8" (Cd.cdNormalise (TH.Stub 48) HU.@?= TH.Stub 48),
            HU.testCase  "Stub #u9" (Cd.cdNormalise (TH.Stub 49) HU.@?= TH.Stub 49),
            HU.testCase "Stub #u10" (Cd.cdNormalise (TH.Stub 50) HU.@?= TH.Stub 0),
            HU.testCase "Stub #u11" (Cd.cdNormalise (TH.Stub 51) HU.@?= TH.Stub 1),
            HU.testCase "Stub #u12" (Cd.cdNormalise (TH.Stub 52) HU.@?= TH.Stub 2),
            HU.testCase "Stub #u13" (Cd.cdNormalise (TH.Stub 97) HU.@?= TH.Stub 47),
            HU.testCase "Stub #u14" (Cd.cdNormalise (TH.Stub 98) HU.@?= TH.Stub 48),
            HU.testCase "Stub #u15" (Cd.cdNormalise (TH.Stub 99) HU.@?= TH.Stub 49),
            HU.testCase "Stub #u16" (Cd.cdNormalise (TH.Stub 100) HU.@?= TH.Stub 13),
            HU.testCase "Stub #u17" (Cd.cdNormalise (TH.Stub 101) HU.@?= TH.Stub 13),
            HU.testCase "Stub #u18" (Cd.cdNormalise (TH.Stub 102) HU.@?= TH.Stub 13),
            QC.testProperty "Char #p1" (\cd -> True QC.==> Cd.cdNormalise cd == chNormalise' cd),
            HU.testCase  "Char #u1" (Cd.cdNormalise '\0' HU.@?= '\0'),
            HU.testCase  "Char #u2" (Cd.cdNormalise '\x01' HU.@?= '\x01'),
            HU.testCase  "Char #u3" (Cd.cdNormalise '\x02' HU.@?= '\x02'),
            HU.testCase  "Char #u4" (Cd.cdNormalise '\x03' HU.@?= '\x03'),
            HU.testCase  "Char #u5" (Cd.cdNormalise '\x1D' HU.@?= '\x1D'),
            HU.testCase  "Char #u6" (Cd.cdNormalise '\x1E' HU.@?= '\x1E'),
            HU.testCase  "Char #u7" (Cd.cdNormalise '\x1F' HU.@?= '\x1F'),
            HU.testCase  "Char #u8" (Cd.cdNormalise '\x20' HU.@?= '\x20'),
            HU.testCase  "Char #u9" (Cd.cdNormalise '\x21' HU.@?= '\x21'),
            HU.testCase "Char #u10" (Cd.cdNormalise '\x22' HU.@?= '\x22'),
            HU.testCase "Char #u11" (Cd.cdNormalise '\x7D' HU.@?= '\x7D'),
            HU.testCase "Char #u12" (Cd.cdNormalise '\x7E' HU.@?= '\x7E'),
            HU.testCase "Char #u13" (Cd.cdNormalise '\x7F' HU.@?= '\x7F'),
            HU.testCase "Char #u14" (Cd.cdNormalise '\x80' HU.@?= '\x80'),
            HU.testCase "Char #u15" (Cd.cdNormalise '\x81' HU.@?= '\x81'),
            HU.testCase "Char #u16" (Cd.cdNormalise '\x82' HU.@?= '\x82'),
            HU.testCase "Char #u17" (Cd.cdNormalise '\xFD' HU.@?= '\xFD'),
            HU.testCase "Char #u18" (Cd.cdNormalise '\xFE' HU.@?= '\xFE'),
            HU.testCase "Char #u19" (Cd.cdNormalise '\xFF' HU.@?= '\xFF'),
            HU.testCase "Char #u20" (Cd.cdNormalise '\x100' HU.@?= '\x100'),
            HU.testCase "Char #u21" (Cd.cdNormalise '\x101' HU.@?= '\x101'),
            HU.testCase "Char #u22" (Cd.cdNormalise '\x102' HU.@?= '\x102'),
            HU.testCase "Char #u23" (Cd.cdNormalise '\xFFFD' HU.@?= '\xFFFD'),
            HU.testCase "Char #u24" (Cd.cdNormalise '\xFFFE' HU.@?= '\xFFFE'),
            HU.testCase "Char #u25" (Cd.cdNormalise '\xFFFF' HU.@?= '\xFFFF'),
            HU.testCase "Char #u26" (Cd.cdNormalise '\x10000' HU.@?= '\x10000'),
            HU.testCase "Char #u27" (Cd.cdNormalise '\x10001' HU.@?= '\x10001'),
            HU.testCase "Char #u28" (Cd.cdNormalise '\x10002' HU.@?= '\x10002'),
            HU.testCase "Char #u29" (Cd.cdNormalise '\xFFFFD' HU.@?= '\xFFFFD'),
            HU.testCase "Char #u30" (Cd.cdNormalise '\xFFFFE' HU.@?= '\xFFFFE'),
            HU.testCase "Char #u31" (Cd.cdNormalise '\xFFFFF' HU.@?= '\xFFFFF'),
            HU.testCase "Char #u32" (Cd.cdNormalise '\x100000' HU.@?= '\x100000'),
            HU.testCase "Char #u33" (Cd.cdNormalise '\x100001' HU.@?= '\x100001'),
            HU.testCase "Char #u34" (Cd.cdNormalise '\x100002' HU.@?= '\x100002'),
            HU.testCase "Char #u35" (Cd.cdNormalise '\x10FFFD' HU.@?= '\x10FFFD'),
            HU.testCase "Char #u36" (Cd.cdNormalise '\x10FFFE' HU.@?= '\x10FFFE'),
            HU.testCase "Char #u37" (Cd.cdNormalise '\x10FFFF' HU.@?= '\x10FFFF'),
            QC.testProperty "CharUtf8 #p1" (\cd -> True QC.==> Cd.cdNormalise cd == cu8Normalise' cd),
            HU.testCase  "CharUtf8 #u1" (Cd.cdNormalise (Cd.CU8OneByte 0x00) HU.@?= Cd.CU8OneByte 0x00),
            HU.testCase  "CharUtf8 #u2" (Cd.cdNormalise (Cd.CU8OneByte 0x01) HU.@?= Cd.CU8OneByte 0x01),
            HU.testCase  "CharUtf8 #u3" (Cd.cdNormalise (Cd.CU8OneByte 0x02) HU.@?= Cd.CU8OneByte 0x02),
            HU.testCase  "CharUtf8 #u4" (Cd.cdNormalise (Cd.CU8OneByte 0x7D) HU.@?= Cd.CU8OneByte 0x7D),
            HU.testCase  "CharUtf8 #u5" (Cd.cdNormalise (Cd.CU8OneByte 0x7E) HU.@?= Cd.CU8OneByte 0x7E),
            HU.testCase  "CharUtf8 #u6" (Cd.cdNormalise (Cd.CU8OneByte 0x7F) HU.@?= Cd.CU8OneByte 0x7F),
            HU.testCase  "CharUtf8 #u7" (Cd.cdNormalise (Cd.CU8OneByte 0x80) HU.@?= Cd.cdReplacement),
            HU.testCase  "CharUtf8 #u8" (Cd.cdNormalise (Cd.CU8OneByte 0x81) HU.@?= Cd.cdReplacement),
            HU.testCase  "CharUtf8 #u9" (Cd.cdNormalise (Cd.CU8OneByte 0x82) HU.@?= Cd.cdReplacement),
            HU.testCase "CharUtf8 #u10" (Cd.cdNormalise (Cd.CU8OneByte 0xFD) HU.@?= Cd.cdReplacement),
            HU.testCase "CharUtf8 #u11" (Cd.cdNormalise (Cd.CU8OneByte 0xFE) HU.@?= Cd.cdReplacement),
            HU.testCase "CharUtf8 #u12" (Cd.cdNormalise (Cd.CU8OneByte 0xFF) HU.@?= Cd.cdReplacement),
            HU.testCase "CharUtf8 #u13" (Cd.cdNormalise (Cd.CU8TwoBytes 0b11000000 0b10000000) HU.@?= Cd.CU8OneByte 0x00),
            HU.testCase "CharUtf8 #u13" (Cd.cdNormalise (Cd.CU8TwoBytes 0b11000000 0b10000001) HU.@?= Cd.CU8OneByte 0x01),
            HU.testCase "CharUtf8 #u14" (Cd.cdNormalise (Cd.CU8TwoBytes 0b11000000 0b10000010) HU.@?= Cd.CU8OneByte 0x02),
            HU.testCase "CharUtf8 #u15" (Cd.cdNormalise (Cd.CU8TwoBytes 0b11000001 0b10111101) HU.@?= Cd.CU8OneByte 0x7D),
            HU.testCase "CharUtf8 #u16" (Cd.cdNormalise (Cd.CU8TwoBytes 0b11000001 0b10111110) HU.@?= Cd.CU8OneByte 0x7E),
            HU.testCase "CharUtf8 #u17" (Cd.cdNormalise (Cd.CU8TwoBytes 0b11000001 0b10111111) HU.@?= Cd.CU8OneByte 0x7F),
            HU.testCase "CharUtf8 #u18" (Cd.cdNormalise (Cd.CU8TwoBytes 0b11000010 0b10000000) HU.@?= Cd.CU8TwoBytes 0b11000010 0b10000000),
            HU.testCase "CharUtf8 #u19" (Cd.cdNormalise (Cd.CU8TwoBytes 0b11000010 0b10000001) HU.@?= Cd.CU8TwoBytes 0b11000010 0b10000001),
            HU.testCase "CharUtf8 #u20" (Cd.cdNormalise (Cd.CU8TwoBytes 0b11000010 0b10000010) HU.@?= Cd.CU8TwoBytes 0b11000010 0b10000010),
            HU.testCase "CharUtf8 #u21" (Cd.cdNormalise (Cd.CU8TwoBytes 0b11011111 0b10111101) HU.@?= Cd.CU8TwoBytes 0b11011111 0b10111101),
            HU.testCase "CharUtf8 #u22" (Cd.cdNormalise (Cd.CU8TwoBytes 0b11011111 0b10111110) HU.@?= Cd.CU8TwoBytes 0b11011111 0b10111110),
            HU.testCase "CharUtf8 #u23" (Cd.cdNormalise (Cd.CU8TwoBytes 0b11011111 0b10111111) HU.@?= Cd.CU8TwoBytes 0b11011111 0b10111111),
            HU.testCase "CharUtf8 #u24" (Cd.cdNormalise (Cd.CU8TwoBytes 0b11111111 0b10111111) HU.@?= Cd.cdReplacement),
            HU.testCase "CharUtf8 #u25" (Cd.cdNormalise (Cd.CU8TwoBytes 0b11011111 0b11111111) HU.@?= Cd.cdReplacement),
            HU.testCase "CharUtf8 #u26" (Cd.cdNormalise (Cd.CU8TwoBytes 0b01011111 0b10111111) HU.@?= Cd.cdReplacement),
            HU.testCase "CharUtf8 #u27" (Cd.cdNormalise (Cd.CU8TwoBytes 0b11011111 0b00111111) HU.@?= Cd.cdReplacement),
            HU.testCase "CharUtf8 #u28" (Cd.cdNormalise (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b10000000) HU.@?= Cd.CU8OneByte 0x00),
            HU.testCase "CharUtf8 #u29" (Cd.cdNormalise (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b10000001) HU.@?= Cd.CU8OneByte 0x01),
            HU.testCase "CharUtf8 #u30" (Cd.cdNormalise (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b10000010) HU.@?= Cd.CU8OneByte 0x02),
            HU.testCase "CharUtf8 #u31" (Cd.cdNormalise (Cd.CU8ThreeBytes 0b11100000 0b10000001 0b10111111) HU.@?= Cd.CU8OneByte 0x7F),
            HU.testCase "CharUtf8 #u32" (Cd.cdNormalise (Cd.CU8ThreeBytes 0b11100000 0b10000010 0b10000000) HU.@?= Cd.CU8TwoBytes 0b11000010 0b10000000),
            HU.testCase "CharUtf8 #u33" (Cd.cdNormalise (Cd.CU8ThreeBytes 0b11100000 0b10000010 0b10000001) HU.@?= Cd.CU8TwoBytes 0b11000010 0b10000001),
            HU.testCase "CharUtf8 #u34" (Cd.cdNormalise (Cd.CU8ThreeBytes 0b11100000 0b10000010 0b10000010) HU.@?= Cd.CU8TwoBytes 0b11000010 0b10000010),
            HU.testCase "CharUtf8 #u35" (Cd.cdNormalise (Cd.CU8ThreeBytes 0b11100000 0b10011111 0b10111101) HU.@?= Cd.CU8TwoBytes 0b11011111 0b10111101),
            HU.testCase "CharUtf8 #u36" (Cd.cdNormalise (Cd.CU8ThreeBytes 0b11100000 0b10011111 0b10111110) HU.@?= Cd.CU8TwoBytes 0b11011111 0b10111110),
            HU.testCase "CharUtf8 #u37" (Cd.cdNormalise (Cd.CU8ThreeBytes 0b11100000 0b10011111 0b10111111) HU.@?= Cd.CU8TwoBytes 0b11011111 0b10111111),
            HU.testCase "CharUtf8 #u38" (Cd.cdNormalise (Cd.CU8ThreeBytes 0b11100000 0b10100000 0b10000000) HU.@?= Cd.CU8ThreeBytes 0b11100000 0b10100000 0b10000000),
            HU.testCase "CharUtf8 #u39" (Cd.cdNormalise (Cd.CU8ThreeBytes 0b11101111 0b10111111 0b10111101) HU.@?= Cd.CU8ThreeBytes 0b11101111 0b10111111 0b10111101),
            HU.testCase "CharUtf8 #u40" (Cd.cdNormalise (Cd.CU8ThreeBytes 0b11101111 0b10111111 0b10111110) HU.@?= Cd.CU8ThreeBytes 0b11101111 0b10111111 0b10111110),
            HU.testCase "CharUtf8 #u41" (Cd.cdNormalise (Cd.CU8ThreeBytes 0b11101111 0b10111111 0b10111111) HU.@?= Cd.CU8ThreeBytes 0b11101111 0b10111111 0b10111111),
            HU.testCase "CharUtf8 #u42" (Cd.cdNormalise (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b11000000) HU.@?= Cd.cdReplacement),
            HU.testCase "CharUtf8 #u43" (Cd.cdNormalise (Cd.CU8ThreeBytes 0b11100000 0b11000000 0b10000000) HU.@?= Cd.cdReplacement),
            HU.testCase "CharUtf8 #u44" (Cd.cdNormalise (Cd.CU8ThreeBytes 0b11110000 0b10000000 0b10000000) HU.@?= Cd.cdReplacement),
            HU.testCase "CharUtf8 #u45" (Cd.cdNormalise (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b00000000) HU.@?= Cd.cdReplacement),
            HU.testCase "CharUtf8 #u46" (Cd.cdNormalise (Cd.CU8ThreeBytes 0b11100000 0b00000000 0b10000000) HU.@?= Cd.cdReplacement),
            HU.testCase "CharUtf8 #u47" (Cd.cdNormalise (Cd.CU8ThreeBytes 0b01100000 0b10000000 0b10000000) HU.@?= Cd.cdReplacement),
            HU.testCase "CharUtf8 #u48" (Cd.cdNormalise (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b10000000) HU.@?= Cd.CU8OneByte 0x00),
            HU.testCase "CharUtf8 #u49" (Cd.cdNormalise (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b10000001) HU.@?= Cd.CU8OneByte 0x01),
            HU.testCase "CharUtf8 #u50" (Cd.cdNormalise (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b10000010) HU.@?= Cd.CU8OneByte 0x02),
            HU.testCase "CharUtf8 #u51" (Cd.cdNormalise (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000001 0b10111101) HU.@?= Cd.CU8OneByte 0x7D),
            HU.testCase "CharUtf8 #u52" (Cd.cdNormalise (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000001 0b10111110) HU.@?= Cd.CU8OneByte 0x7E),
            HU.testCase "CharUtf8 #u53" (Cd.cdNormalise (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000001 0b10111111) HU.@?= Cd.CU8OneByte 0x7F),
            HU.testCase "CharUtf8 #u54" (Cd.cdNormalise (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000010 0b10000000) HU.@?= Cd.CU8TwoBytes 0b11000010 0b10000000),
            HU.testCase "CharUtf8 #u55" (Cd.cdNormalise (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000010 0b10000001) HU.@?= Cd.CU8TwoBytes 0b11000010 0b10000001),
            HU.testCase "CharUtf8 #u56" (Cd.cdNormalise (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000010 0b10000010) HU.@?= Cd.CU8TwoBytes 0b11000010 0b10000010),
            HU.testCase "CharUtf8 #u57" (Cd.cdNormalise (Cd.CU8FourBytes 0b11110000 0b10000000 0b10011111 0b10111101) HU.@?= Cd.CU8TwoBytes 0b11011111 0b10111101),
            HU.testCase "CharUtf8 #u58" (Cd.cdNormalise (Cd.CU8FourBytes 0b11110000 0b10000000 0b10011111 0b10111110) HU.@?= Cd.CU8TwoBytes 0b11011111 0b10111110),
            HU.testCase "CharUtf8 #u59" (Cd.cdNormalise (Cd.CU8FourBytes 0b11110000 0b10000000 0b10011111 0b10111111) HU.@?= Cd.CU8TwoBytes 0b11011111 0b10111111),
            HU.testCase "CharUtf8 #u60" (Cd.cdNormalise (Cd.CU8FourBytes 0b11110000 0b10000000 0b10100000 0b10000000) HU.@?= Cd.CU8ThreeBytes 0b11100000 0b10100000 0b10000000),
            HU.testCase "CharUtf8 #u61" (Cd.cdNormalise (Cd.CU8FourBytes 0b11110000 0b10000000 0b10100000 0b10000001) HU.@?= Cd.CU8ThreeBytes 0b11100000 0b10100000 0b10000001),
            HU.testCase "CharUtf8 #u62" (Cd.cdNormalise (Cd.CU8FourBytes 0b11110000 0b10000000 0b10100000 0b10000010) HU.@?= Cd.CU8ThreeBytes 0b11100000 0b10100000 0b10000010),
            HU.testCase "CharUtf8 #u63" (Cd.cdNormalise (Cd.CU8FourBytes 0b11110000 0b10001111 0b10111111 0b10111101) HU.@?= Cd.CU8ThreeBytes 0b11101111 0b10111111 0b10111101),
            HU.testCase "CharUtf8 #u64" (Cd.cdNormalise (Cd.CU8FourBytes 0b11110000 0b10001111 0b10111111 0b10111110) HU.@?= Cd.CU8ThreeBytes 0b11101111 0b10111111 0b10111110),
            HU.testCase "CharUtf8 #u65" (Cd.cdNormalise (Cd.CU8FourBytes 0b11110000 0b10001111 0b10111111 0b10111111) HU.@?= Cd.CU8ThreeBytes 0b11101111 0b10111111 0b10111111),
            HU.testCase "CharUtf8 #u66" (Cd.cdNormalise (Cd.CU8FourBytes 0b11110000 0b10010000 0b10000000 0b10000000) HU.@?= Cd.CU8FourBytes 0b11110000 0b10010000 0b10000000 0b10000000),
            HU.testCase "CharUtf8 #u67" (Cd.cdNormalise (Cd.CU8FourBytes 0b11110000 0b10010000 0b10000000 0b10000001) HU.@?= Cd.CU8FourBytes 0b11110000 0b10010000 0b10000000 0b10000001),
            HU.testCase "CharUtf8 #u68" (Cd.cdNormalise (Cd.CU8FourBytes 0b11110000 0b10010000 0b10000000 0b10000010) HU.@?= Cd.CU8FourBytes 0b11110000 0b10010000 0b10000000 0b10000010),
            HU.testCase "CharUtf8 #u69" (Cd.cdNormalise (Cd.CU8FourBytes 0b11110100 0b10001111 0b10111111 0b10111101) HU.@?= Cd.CU8FourBytes 0b11110100 0b10001111 0b10111111 0b10111101),
            HU.testCase "CharUtf8 #u70" (Cd.cdNormalise (Cd.CU8FourBytes 0b11110100 0b10001111 0b10111111 0b10111110) HU.@?= Cd.CU8FourBytes 0b11110100 0b10001111 0b10111111 0b10111110),
            HU.testCase "CharUtf8 #u71" (Cd.cdNormalise (Cd.CU8FourBytes 0b11110100 0b10001111 0b10111111 0b10111111) HU.@?= Cd.CU8FourBytes 0b11110100 0b10001111 0b10111111 0b10111111),
            HU.testCase "CharUtf8 #u72" (Cd.cdNormalise (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b11000000) HU.@?= Cd.cdReplacement),
            HU.testCase "CharUtf8 #u73" (Cd.cdNormalise (Cd.CU8FourBytes 0b11110000 0b10000000 0b11000000 0b10000000) HU.@?= Cd.cdReplacement),
            HU.testCase "CharUtf8 #u74" (Cd.cdNormalise (Cd.CU8FourBytes 0b11110000 0b11000000 0b10000000 0b10000000) HU.@?= Cd.cdReplacement),
            HU.testCase "CharUtf8 #u75" (Cd.cdNormalise (Cd.CU8FourBytes 0b11111000 0b10000000 0b10000000 0b10000000) HU.@?= Cd.cdReplacement),
            HU.testCase "CharUtf8 #u76" (Cd.cdNormalise (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b00000000) HU.@?= Cd.cdReplacement),
            HU.testCase "CharUtf8 #u77" (Cd.cdNormalise (Cd.CU8FourBytes 0b11110000 0b10000000 0b00000000 0b10000000) HU.@?= Cd.cdReplacement),
            HU.testCase "CharUtf8 #u78" (Cd.cdNormalise (Cd.CU8FourBytes 0b11110000 0b00000000 0b10000000 0b10000000) HU.@?= Cd.cdReplacement),
            HU.testCase "CharUtf8 #u79" (Cd.cdNormalise (Cd.CU8FourBytes 0b01110000 0b10000000 0b10000000 0b10000000) HU.@?= Cd.cdReplacement),
            QC.testProperty "CharIso1 #p1" (\cd -> True QC.==> Cd.cdNormalise cd == ci1Normalise' cd),
            HU.testCase  "CharIso1 #u1" (Cd.cdNormalise (Cd.CharIso1 0x00) HU.@?= Cd.CharIso1 0x00),
            HU.testCase  "CharIso1 #u2" (Cd.cdNormalise (Cd.CharIso1 0x01) HU.@?= Cd.CharIso1 0x01),
            HU.testCase  "CharIso1 #u3" (Cd.cdNormalise (Cd.CharIso1 0x02) HU.@?= Cd.CharIso1 0x02),
            HU.testCase  "CharIso1 #u4" (Cd.cdNormalise (Cd.CharIso1 0x7D) HU.@?= Cd.CharIso1 0x7D),
            HU.testCase  "CharIso1 #u5" (Cd.cdNormalise (Cd.CharIso1 0x7E) HU.@?= Cd.CharIso1 0x7E),
            HU.testCase  "CharIso1 #u6" (Cd.cdNormalise (Cd.CharIso1 0x7F) HU.@?= Cd.CharIso1 0x7F),
            HU.testCase  "CharIso1 #u7" (Cd.cdNormalise (Cd.CharIso1 0x80) HU.@?= Cd.cdReplacement),
            HU.testCase  "CharIso1 #u8" (Cd.cdNormalise (Cd.CharIso1 0x81) HU.@?= Cd.cdReplacement),
            HU.testCase  "CharIso1 #u9" (Cd.cdNormalise (Cd.CharIso1 0x82) HU.@?= Cd.cdReplacement),
            HU.testCase "CharIso1 #u10" (Cd.cdNormalise (Cd.CharIso1 0x9D) HU.@?= Cd.cdReplacement),
            HU.testCase "CharIso1 #u11" (Cd.cdNormalise (Cd.CharIso1 0x9E) HU.@?= Cd.cdReplacement),
            HU.testCase "CharIso1 #u12" (Cd.cdNormalise (Cd.CharIso1 0x9F) HU.@?= Cd.cdReplacement),
            HU.testCase "CharIso1 #u13" (Cd.cdNormalise (Cd.CharIso1 0xA0) HU.@?= Cd.CharIso1 0xA0),
            HU.testCase "CharIso1 #u14" (Cd.cdNormalise (Cd.CharIso1 0xA1) HU.@?= Cd.CharIso1 0xA1),
            HU.testCase "CharIso1 #u15" (Cd.cdNormalise (Cd.CharIso1 0xA2) HU.@?= Cd.CharIso1 0xA2),
            HU.testCase "CharIso1 #u16" (Cd.cdNormalise (Cd.CharIso1 0xFD) HU.@?= Cd.CharIso1 0xFD),
            HU.testCase "CharIso1 #u17" (Cd.cdNormalise (Cd.CharIso1 0xFE) HU.@?= Cd.CharIso1 0xFE),
            HU.testCase "CharIso1 #u18" (Cd.cdNormalise (Cd.CharIso1 0xFF) HU.@?= Cd.CharIso1 0xFF),
            QC.testProperty "CharWin1 #p1" (\cd -> True QC.==> Cd.cdNormalise cd == cw1Normalise' cd),
            HU.testCase  "CharWin1 #u1" (Cd.cdNormalise (Cd.CharWin1 0x00) HU.@?= Cd.CharWin1 0x00),
            HU.testCase  "CharWin1 #u2" (Cd.cdNormalise (Cd.CharWin1 0x01) HU.@?= Cd.CharWin1 0x01),
            HU.testCase  "CharWin1 #u3" (Cd.cdNormalise (Cd.CharWin1 0x02) HU.@?= Cd.CharWin1 0x02),
            HU.testCase  "CharWin1 #u4" (Cd.cdNormalise (Cd.CharWin1 0x7E) HU.@?= Cd.CharWin1 0x7E),
            HU.testCase  "CharWin1 #u5" (Cd.cdNormalise (Cd.CharWin1 0x7F) HU.@?= Cd.CharWin1 0x7F),
            HU.testCase  "CharWin1 #u6" (Cd.cdNormalise (Cd.CharWin1 0x80) HU.@?= Cd.CharWin1 0x80),
            HU.testCase  "CharWin1 #u7" (Cd.cdNormalise (Cd.CharWin1 0x81) HU.@?= Cd.cdReplacement),
            HU.testCase  "CharWin1 #u8" (Cd.cdNormalise (Cd.CharWin1 0x82) HU.@?= Cd.CharWin1 0x82),
            HU.testCase  "CharWin1 #u9" (Cd.cdNormalise (Cd.CharWin1 0x83) HU.@?= Cd.CharWin1 0x83),
            HU.testCase "CharWin1 #u10" (Cd.cdNormalise (Cd.CharWin1 0x84) HU.@?= Cd.CharWin1 0x84),
            HU.testCase "CharWin1 #u11" (Cd.cdNormalise (Cd.CharWin1 0x8A) HU.@?= Cd.CharWin1 0x8A),
            HU.testCase "CharWin1 #u12" (Cd.cdNormalise (Cd.CharWin1 0x8B) HU.@?= Cd.CharWin1 0x8B),
            HU.testCase "CharWin1 #u13" (Cd.cdNormalise (Cd.CharWin1 0x8C) HU.@?= Cd.CharWin1 0x8C),
            HU.testCase "CharWin1 #u14" (Cd.cdNormalise (Cd.CharWin1 0x8D) HU.@?= Cd.cdReplacement),
            HU.testCase "CharWin1 #u15" (Cd.cdNormalise (Cd.CharWin1 0x8E) HU.@?= Cd.CharWin1 0x8E),
            HU.testCase "CharWin1 #u16" (Cd.cdNormalise (Cd.CharWin1 0x8F) HU.@?= Cd.cdReplacement),
            HU.testCase "CharWin1 #u17" (Cd.cdNormalise (Cd.CharWin1 0x90) HU.@?= Cd.cdReplacement),
            HU.testCase "CharWin1 #u18" (Cd.cdNormalise (Cd.CharWin1 0x91) HU.@?= Cd.CharWin1 0x91),
            HU.testCase "CharWin1 #u19" (Cd.cdNormalise (Cd.CharWin1 0x92) HU.@?= Cd.CharWin1 0x92),
            HU.testCase "CharWin1 #u20" (Cd.cdNormalise (Cd.CharWin1 0x93) HU.@?= Cd.CharWin1 0x93),
            HU.testCase "CharWin1 #u21" (Cd.cdNormalise (Cd.CharWin1 0x9A) HU.@?= Cd.CharWin1 0x9A),
            HU.testCase "CharWin1 #u22" (Cd.cdNormalise (Cd.CharWin1 0x9B) HU.@?= Cd.CharWin1 0x9B),
            HU.testCase "CharWin1 #u23" (Cd.cdNormalise (Cd.CharWin1 0x9C) HU.@?= Cd.CharWin1 0x9C),
            HU.testCase "CharWin1 #u24" (Cd.cdNormalise (Cd.CharWin1 0x9D) HU.@?= Cd.cdReplacement),
            HU.testCase "CharWin1 #u25" (Cd.cdNormalise (Cd.CharWin1 0x9E) HU.@?= Cd.CharWin1 0x9E),
            HU.testCase "CharWin1 #u26" (Cd.cdNormalise (Cd.CharWin1 0x9F) HU.@?= Cd.CharWin1 0x9F),
            HU.testCase "CharWin1 #u27" (Cd.cdNormalise (Cd.CharWin1 0xA0) HU.@?= Cd.CharWin1 0xA0),
            HU.testCase "CharWin1 #u28" (Cd.cdNormalise (Cd.CharWin1 0xA1) HU.@?= Cd.CharWin1 0xA1),
            HU.testCase "CharWin1 #u29" (Cd.cdNormalise (Cd.CharWin1 0xA2) HU.@?= Cd.CharWin1 0xA2),
            HU.testCase "CharWin1 #u30" (Cd.cdNormalise (Cd.CharWin1 0xA3) HU.@?= Cd.CharWin1 0xA3),
            HU.testCase "CharWin1 #u31" (Cd.cdNormalise (Cd.CharWin1 0xFD) HU.@?= Cd.CharWin1 0xFD),
            HU.testCase "CharWin1 #u32" (Cd.cdNormalise (Cd.CharWin1 0xFE) HU.@?= Cd.CharWin1 0xFE),
            HU.testCase "CharWin1 #u33" (Cd.cdNormalise (Cd.CharWin1 0xFF) HU.@?= Cd.CharWin1 0xFF),
            QC.testProperty "Octets.Octet #p1" (\cd -> True QC.==> Cd.cdNormalise (cd :: Oct.Octet) == cd),
            HU.testCase  "Octets.Octet #u1" (Cd.cdNormalise (0x00 :: Oct.Octet) HU.@?= 0x00),
            HU.testCase  "Octets.Octet #u2" (Cd.cdNormalise (0x01 :: Oct.Octet) HU.@?= 0x01),
            HU.testCase  "Octets.Octet #u3" (Cd.cdNormalise (0x02 :: Oct.Octet) HU.@?= 0x02),
            HU.testCase  "Octets.Octet #u4" (Cd.cdNormalise (0xFD :: Oct.Octet) HU.@?= 0xFD),
            HU.testCase  "Octets.Octet #u5" (Cd.cdNormalise (0xFE :: Oct.Octet) HU.@?= 0xFE),
            HU.testCase  "Octets.Octet #u6" (Cd.cdNormalise (0xFF :: Oct.Octet) HU.@?= 0xFF),
            HU.testCase  "Bool #u1" (Cd.cdNormalise False HU.@?= False),
            HU.testCase  "Bool #u2" (Cd.cdNormalise True HU.@?= True)
       ]

{-  * validated: ✅
        * completeness: ✅
        * independence: ✅
          * NOTE: The test is dependent on cdNormalise - but cdNormalise is completely and independently tested in tgUnitcdNormalise.
        * edge cases  : ✅
        * conform doc.: ✅ -}
tgUnitisNormalised :: T.TestTree
tgUnitisNormalised =
    T.testGroup
        "isNormalised"
        [
            QC.testProperty "Stub #p1" (\cd -> (Cd.cdNormalise (cd :: TH.Stub) == cd) QC.==> Cd.isNormalised cd),
            QC.testProperty "Stub #p2" (\cd -> (Cd.cdNormalise (cd :: TH.Stub) /= cd) QC.==> not (Cd.isNormalised cd)),
            HU.testCase     "Stub #u3" (Cd.isNormalised (TH.Stub (-2)) HU.@?= False),
            HU.testCase     "Stub #u4" (Cd.isNormalised (TH.Stub (-1)) HU.@?= False),
            HU.testCase     "Stub #u5" (Cd.isNormalised (TH.Stub 0) HU.@?= True),
            HU.testCase     "Stub #u6" (Cd.isNormalised (TH.Stub 1) HU.@?= True),
            HU.testCase     "Stub #u7" (Cd.isNormalised (TH.Stub 48) HU.@?= True),
            HU.testCase     "Stub #u8" (Cd.isNormalised (TH.Stub 49) HU.@?= True),
            HU.testCase     "Stub #u9" (Cd.isNormalised (TH.Stub 50) HU.@?= False),
            HU.testCase     "Stub #u10" (Cd.isNormalised (TH.Stub 51) HU.@?= False),
            HU.testCase     "Stub #u11" (Cd.isNormalised (TH.Stub 98) HU.@?= False),
            HU.testCase     "Stub #u12" (Cd.isNormalised (TH.Stub 99) HU.@?= False),
            HU.testCase     "Stub #u13" (Cd.isNormalised (TH.Stub 100) HU.@?= False),
            HU.testCase     "Stub #u14" (Cd.isNormalised (TH.Stub 101) HU.@?= False),
            QC.testProperty "Char" (\cd -> True QC.==> Cd.isNormalised (cd :: Char)),
            QC.testProperty "CharUtf8 #p1" (\cd -> (Cd.cdNormalise (cd :: Cd.CharUtf8) == cd) QC.==> Cd.isNormalised cd),
            QC.testProperty "CharUtf8 #p2" (\cd -> (Cd.cdNormalise (cd :: Cd.CharUtf8) /= cd) QC.==> not (Cd.isNormalised cd)),
            HU.testCase  "CharUtf8 #u1" (Cd.isNormalised (Cd.CU8OneByte 0x00) HU.@?= True),
            HU.testCase  "CharUtf8 #u2" (Cd.isNormalised (Cd.CU8OneByte 0x01) HU.@?= True),
            HU.testCase  "CharUtf8 #u3" (Cd.isNormalised (Cd.CU8OneByte 0x02) HU.@?= True),
            HU.testCase  "CharUtf8 #u4" (Cd.isNormalised (Cd.CU8OneByte 0x7D) HU.@?= True),
            HU.testCase  "CharUtf8 #u5" (Cd.isNormalised (Cd.CU8OneByte 0x7E) HU.@?= True),
            HU.testCase  "CharUtf8 #u6" (Cd.isNormalised (Cd.CU8OneByte 0x7F) HU.@?= True),
            HU.testCase  "CharUtf8 #u7" (Cd.isNormalised (Cd.CU8OneByte 0x80) HU.@?= False),
            HU.testCase  "CharUtf8 #u8" (Cd.isNormalised (Cd.CU8OneByte 0x81) HU.@?= False),
            HU.testCase  "CharUtf8 #u9" (Cd.isNormalised (Cd.CU8OneByte 0xFE) HU.@?= False),
            HU.testCase "CharUtf8 #u10" (Cd.isNormalised (Cd.CU8OneByte 0xFF) HU.@?= False),
            HU.testCase "CharUtf8 #u11" (Cd.isNormalised (Cd.CU8TwoBytes 0b00000000 0b00000000) HU.@?= False),
            HU.testCase "CharUtf8 #u12" (Cd.isNormalised (Cd.CU8TwoBytes 0b11000000 0b00000000) HU.@?= False),
            HU.testCase "CharUtf8 #u13" (Cd.isNormalised (Cd.CU8TwoBytes 0b10000000 0b10000000) HU.@?= False),
            HU.testCase "CharUtf8 #u14" (Cd.isNormalised (Cd.CU8TwoBytes 0b01000000 0b10000000) HU.@?= False),
            HU.testCase "CharUtf8 #u15" (Cd.isNormalised (Cd.CU8TwoBytes 0b11000000 0b10000000) HU.@?= False),
            HU.testCase "CharUtf8 #u16" (Cd.isNormalised (Cd.CU8TwoBytes 0b11000000 0b10000001) HU.@?= False),
            HU.testCase "CharUtf8 #u17" (Cd.isNormalised (Cd.CU8TwoBytes 0b11000001 0b10111111) HU.@?= False),
            HU.testCase "CharUtf8 #u18" (Cd.isNormalised (Cd.CU8TwoBytes 0b11000010 0b10000000) HU.@?= True),
            HU.testCase "CharUtf8 #u19" (Cd.isNormalised (Cd.CU8TwoBytes 0b11000010 0b10000001) HU.@?= True),
            HU.testCase "CharUtf8 #u20" (Cd.isNormalised (Cd.CU8TwoBytes 0b11011111 0b10111110) HU.@?= True),
            HU.testCase "CharUtf8 #u21" (Cd.isNormalised (Cd.CU8TwoBytes 0b11011111 0b10111111) HU.@?= True),
            HU.testCase "CharUtf8 #u22" (Cd.isNormalised (Cd.CU8TwoBytes 0b11011111 0b01111111) HU.@?= False),
            HU.testCase "CharUtf8 #u23" (Cd.isNormalised (Cd.CU8TwoBytes 0b11011111 0b11111111) HU.@?= False),
            HU.testCase "CharUtf8 #u24" (Cd.isNormalised (Cd.CU8TwoBytes 0b01111111 0b10111111) HU.@?= False),
            HU.testCase "CharUtf8 #u25" (Cd.isNormalised (Cd.CU8TwoBytes 0b10111111 0b10111111) HU.@?= False),
            HU.testCase "CharUtf8 #u26" (Cd.isNormalised (Cd.CU8TwoBytes 0b11111111 0b10111111) HU.@?= False),
            HU.testCase "CharUtf8 #u27" (Cd.isNormalised (Cd.CU8TwoBytes 0b11111111 0b11111111) HU.@?= False),
            HU.testCase "CharUtf8 #u28" (Cd.isNormalised (Cd.CU8ThreeBytes 0b00000000 0b10000000 0b00000000) HU.@?= False),
            HU.testCase "CharUtf8 #u29" (Cd.isNormalised (Cd.CU8ThreeBytes 0b00000000 0b10000000 0b00000001) HU.@?= False),
            HU.testCase "CharUtf8 #u30" (Cd.isNormalised (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b00000000) HU.@?= False),
            HU.testCase "CharUtf8 #u31" (Cd.isNormalised (Cd.CU8ThreeBytes 0b11100000 0b00000000 0b10000000) HU.@?= False),
            HU.testCase "CharUtf8 #u32" (Cd.isNormalised (Cd.CU8ThreeBytes 0b11000000 0b10000000 0b10000000) HU.@?= False),
            HU.testCase "CharUtf8 #u33" (Cd.isNormalised (Cd.CU8ThreeBytes 0b10100000 0b10000000 0b10000000) HU.@?= False),
            HU.testCase "CharUtf8 #u34" (Cd.isNormalised (Cd.CU8ThreeBytes 0b01100000 0b10000000 0b10000000) HU.@?= False),
            HU.testCase "CharUtf8 #u35" (Cd.isNormalised (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b10000000) HU.@?= False),
            HU.testCase "CharUtf8 #u36" (Cd.isNormalised (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b10000001) HU.@?= False),
            HU.testCase "CharUtf8 #u37" (Cd.isNormalised (Cd.CU8ThreeBytes 0b11100000 0b10001111 0b10111111) HU.@?= False),
            HU.testCase "CharUtf8 #u38" (Cd.isNormalised (Cd.CU8ThreeBytes 0b11100000 0b10100000 0b10000000) HU.@?= True),
            HU.testCase "CharUtf8 #u39" (Cd.isNormalised (Cd.CU8ThreeBytes 0b11100000 0b10100000 0b10000001) HU.@?= True),
            HU.testCase "CharUtf8 #u40" (Cd.isNormalised (Cd.CU8ThreeBytes 0b11101111 0b10111111 0b10111111) HU.@?= True),
            HU.testCase "CharUtf8 #u41" (Cd.isNormalised (Cd.CU8ThreeBytes 0b11101111 0b10111111 0b11111111) HU.@?= False),
            HU.testCase "CharUtf8 #u42" (Cd.isNormalised (Cd.CU8ThreeBytes 0b11101111 0b11111111 0b10111111) HU.@?= False),
            HU.testCase "CharUtf8 #u43" (Cd.isNormalised (Cd.CU8ThreeBytes 0b11111111 0b10111111 0b10111111) HU.@?= False),
            HU.testCase "CharUtf8 #u44" (Cd.isNormalised (Cd.CU8ThreeBytes 0b11001111 0b10111111 0b10111111) HU.@?= False),
            HU.testCase "CharUtf8 #u45" (Cd.isNormalised (Cd.CU8ThreeBytes 0b10101111 0b10111111 0b10111111) HU.@?= False),
            HU.testCase "CharUtf8 #u46" (Cd.isNormalised (Cd.CU8ThreeBytes 0b01101111 0b10111111 0b10111111) HU.@?= False),
            HU.testCase "CharUtf8 #u47" (Cd.isNormalised (Cd.CU8ThreeBytes 0b11101111 0b10111111 0b11111111) HU.@?= False),
            HU.testCase "CharUtf8 #u48" (Cd.isNormalised (Cd.CU8ThreeBytes 0b11101111 0b11111111 0b10111111) HU.@?= False),
            HU.testCase "CharUtf8 #u49" (Cd.isNormalised (Cd.CU8ThreeBytes 0b11111111 0b10111111 0b10111111) HU.@?= False),
            HU.testCase "CharUtf8 #u50" (Cd.isNormalised (Cd.CU8ThreeBytes 0b11111111 0b11111111 0b11111111) HU.@?= False),
            HU.testCase "CharUtf8 #u51" (Cd.isNormalised (Cd.CU8FourBytes 0b00000000 0b00000000 0b00000000 0b00000000) HU.@?= False),
            HU.testCase "CharUtf8 #u52" (Cd.isNormalised (Cd.CU8FourBytes 0b00000000 0b00000000 0b00000000 0b00000001) HU.@?= False),
            HU.testCase "CharUtf8 #u53" (Cd.isNormalised (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b10000000) HU.@?= False),
            HU.testCase "CharUtf8 #u54" (Cd.isNormalised (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b10000001) HU.@?= False),
            HU.testCase "CharUtf8 #u55" (Cd.isNormalised (Cd.CU8FourBytes 0b11110000 0b10001111 0b10111111 0b10111111) HU.@?= False),
            HU.testCase "CharUtf8 #u56" (Cd.isNormalised (Cd.CU8FourBytes 0b11110000 0b10010000 0b10000000 0b10000000) HU.@?= True),
            HU.testCase "CharUtf8 #u57" (Cd.isNormalised (Cd.CU8FourBytes 0b11110000 0b10010000 0b10000000 0b10000001) HU.@?= True),
            HU.testCase "CharUtf8 #u58" (Cd.isNormalised (Cd.CU8FourBytes 0b11110100 0b10001111 0b10111111 0b10111110) HU.@?= True),
            HU.testCase "CharUtf8 #u59" (Cd.isNormalised (Cd.CU8FourBytes 0b11110100 0b10001111 0b10111111 0b10111111) HU.@?= True),
            HU.testCase "CharUtf8 #u60" (Cd.isNormalised (Cd.CU8FourBytes 0b11110100 0b10010000 0b10000000 0b10000000) HU.@?= False),
            HU.testCase "CharUtf8 #u61" (Cd.isNormalised (Cd.CU8FourBytes 0b11110100 0b10010000 0b10000000 0b10000001) HU.@?= False),
            HU.testCase "CharUtf8 #u62" (Cd.isNormalised (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b11000000) HU.@?= False),
            HU.testCase "CharUtf8 #u63" (Cd.isNormalised (Cd.CU8FourBytes 0b11110000 0b10000000 0b11000000 0b10000000) HU.@?= False),
            HU.testCase "CharUtf8 #u64" (Cd.isNormalised (Cd.CU8FourBytes 0b11110000 0b11000000 0b10000000 0b10000000) HU.@?= False),
            HU.testCase "CharUtf8 #u65" (Cd.isNormalised (Cd.CU8FourBytes 0b11111000 0b10000000 0b10000000 0b10000000) HU.@?= False),
            HU.testCase "CharUtf8 #u66" (Cd.isNormalised (Cd.CU8FourBytes 0b11100000 0b10010000 0b10000000 0b10000001) HU.@?= False),
            HU.testCase "CharUtf8 #u67" (Cd.isNormalised (Cd.CU8FourBytes 0b11010000 0b10010000 0b10000000 0b10000001) HU.@?= False),
            HU.testCase "CharUtf8 #u68" (Cd.isNormalised (Cd.CU8FourBytes 0b10110000 0b10010000 0b10000000 0b10000001) HU.@?= False),
            HU.testCase "CharUtf8 #u69" (Cd.isNormalised (Cd.CU8FourBytes 0b01110000 0b10010000 0b10000000 0b10000001) HU.@?= False),
            HU.testCase "CharUtf8 #u70" (Cd.isNormalised (Cd.CU8FourBytes 0b11111111 0b11111111 0b11111111 0b11111110) HU.@?= False),
            HU.testCase "CharUtf8 #u71" (Cd.isNormalised (Cd.CU8FourBytes 0b11111111 0b11111111 0b11111111 0b11111111) HU.@?= False),
            QC.testProperty "CharIso1 #p1" (\cd -> (Cd.cdNormalise (cd :: Cd.CharIso1) == cd) QC.==> Cd.isNormalised cd),
            QC.testProperty "CharIso1 #p2" (\cd -> (Cd.cdNormalise (cd :: Cd.CharIso1) /= cd) QC.==> not (Cd.isNormalised cd)),
            HU.testCase  "CharIso1 #u1" (Cd.isNormalised (Cd.CharIso1 0x00) HU.@?= True),
            HU.testCase  "CharIso1 #u2" (Cd.isNormalised (Cd.CharIso1 0x01) HU.@?= True),
            HU.testCase  "CharIso1 #u3" (Cd.isNormalised (Cd.CharIso1 0x7E) HU.@?= True),
            HU.testCase  "CharIso1 #u4" (Cd.isNormalised (Cd.CharIso1 0x7F) HU.@?= True),
            HU.testCase  "CharIso1 #u5" (Cd.isNormalised (Cd.CharIso1 0x80) HU.@?= False),
            HU.testCase  "CharIso1 #u6" (Cd.isNormalised (Cd.CharIso1 0x81) HU.@?= False),
            HU.testCase  "CharIso1 #u7" (Cd.isNormalised (Cd.CharIso1 0x9E) HU.@?= False),
            HU.testCase "CharIso1 #u8" (Cd.isNormalised (Cd.CharIso1 0x9F) HU.@?= False),
            HU.testCase "CharIso1 #u9" (Cd.isNormalised (Cd.CharIso1 0xA0) HU.@?= True),
            HU.testCase "CharIso1 #u10" (Cd.isNormalised (Cd.CharIso1 0xA1) HU.@?= True),
            HU.testCase "CharIso1 #u11" (Cd.isNormalised (Cd.CharIso1 0xFE) HU.@?= True),
            HU.testCase "CharIso1 #u12" (Cd.isNormalised (Cd.CharIso1 0xFF) HU.@?= True),
            QC.testProperty "CharWin1 #p1" (\cd -> (Cd.cdNormalise (cd :: Cd.CharWin1) == cd) QC.==> Cd.isNormalised cd),
            QC.testProperty "CharWin1 #p2" (\cd -> (Cd.cdNormalise (cd :: Cd.CharWin1) /= cd) QC.==> not (Cd.isNormalised cd)),
            HU.testCase     "CharWin1 #u1" (Cd.isNormalised (Cd.CharWin1 0x00) HU.@?= True),
            HU.testCase     "CharWin1 #u2" (Cd.isNormalised (Cd.CharWin1 0x01) HU.@?= True),
            HU.testCase     "CharWin1 #u3" (Cd.isNormalised (Cd.CharWin1 0x02) HU.@?= True),
            HU.testCase     "CharWin1 #u4" (Cd.isNormalised (Cd.CharWin1 0x7E) HU.@?= True),
            HU.testCase     "CharWin1 #u5" (Cd.isNormalised (Cd.CharWin1 0x7F) HU.@?= True),
            HU.testCase     "CharWin1 #u6" (Cd.isNormalised (Cd.CharWin1 0x80) HU.@?= True),
            HU.testCase     "CharWin1 #u7" (Cd.isNormalised (Cd.CharWin1 0x81) HU.@?= False),
            HU.testCase     "CharWin1 #u8" (Cd.isNormalised (Cd.CharWin1 0x82) HU.@?= True),
            HU.testCase     "CharWin1 #u9" (Cd.isNormalised (Cd.CharWin1 0x83) HU.@?= True),
            HU.testCase    "CharWin1 #u10" (Cd.isNormalised (Cd.CharWin1 0x8B) HU.@?= True),
            HU.testCase    "CharWin1 #u11" (Cd.isNormalised (Cd.CharWin1 0x8C) HU.@?= True),
            HU.testCase    "CharWin1 #u12" (Cd.isNormalised (Cd.CharWin1 0x8D) HU.@?= False),
            HU.testCase    "CharWin1 #u13" (Cd.isNormalised (Cd.CharWin1 0x8E) HU.@?= True),
            HU.testCase    "CharWin1 #u14" (Cd.isNormalised (Cd.CharWin1 0x8F) HU.@?= False),
            HU.testCase    "CharWin1 #u15" (Cd.isNormalised (Cd.CharWin1 0x90) HU.@?= False),
            HU.testCase    "CharWin1 #u16" (Cd.isNormalised (Cd.CharWin1 0x91) HU.@?= True),
            HU.testCase    "CharWin1 #u17" (Cd.isNormalised (Cd.CharWin1 0x92) HU.@?= True),
            HU.testCase    "CharWin1 #u18" (Cd.isNormalised (Cd.CharWin1 0x9B) HU.@?= True),
            HU.testCase    "CharWin1 #u19" (Cd.isNormalised (Cd.CharWin1 0x9C) HU.@?= True),
            HU.testCase    "CharWin1 #u20" (Cd.isNormalised (Cd.CharWin1 0x9D) HU.@?= False),
            HU.testCase    "CharWin1 #u21" (Cd.isNormalised (Cd.CharWin1 0x9E) HU.@?= True),
            HU.testCase    "CharWin1 #u22" (Cd.isNormalised (Cd.CharWin1 0x9F) HU.@?= True),
            HU.testCase    "CharWin1 #u23" (Cd.isNormalised (Cd.CharWin1 0xFE) HU.@?= True),
            HU.testCase    "CharWin1 #u24" (Cd.isNormalised (Cd.CharWin1 0xFF) HU.@?= True),
            QC.testProperty "Octets.Octet #p1" (\cd -> True QC.==> Cd.isNormalised (cd :: Oct.Octet)),
            QC.testProperty "Bool #p1" (\cd -> True QC.==> Cd.isNormalised (cd :: Bool))
        ]

{-  * validated: ✅
        * completeness: ✅
        * independence: ✅
        * edge cases  : ✅
        * conform doc.: ✅ -}
tgUnitchFromCode :: T.TestTree
tgUnitchFromCode =
    T.testGroup
        "chFromCode"
        [
            QC.testProperty "Stub - #p1" (\cd -> True QC.==>  Cd.chFromCode (cd :: TH.Stub) == chFromStub' cd),
            HU.testCase  "Stub #u1" (Cd.chFromCode (TH.Stub (-2)) HU.@?= '?'),
            HU.testCase  "Stub #u2" (Cd.chFromCode (TH.Stub (-1)) HU.@?= '?'),
            HU.testCase  "Stub #u3" (Cd.chFromCode (TH.Stub   0) HU.@?= '0'),
            HU.testCase  "Stub #u4" (Cd.chFromCode (TH.Stub   1) HU.@?= '1'),
            HU.testCase  "Stub #u5" (Cd.chFromCode (TH.Stub   2) HU.@?= '2'),
            HU.testCase  "Stub #u6" (Cd.chFromCode (TH.Stub  22) HU.@?= 'F'),
            HU.testCase  "Stub #u7" (Cd.chFromCode (TH.Stub  23) HU.@?= 'G'),
            HU.testCase  "Stub #u8" (Cd.chFromCode (TH.Stub  24) HU.@?= 'H'),
            HU.testCase  "Stub #u9" (Cd.chFromCode (TH.Stub  25) HU.@?= '\0'),
            HU.testCase "Stub #u10" (Cd.chFromCode (TH.Stub  26) HU.@?= 'a'),
            HU.testCase "Stub #u11" (Cd.chFromCode (TH.Stub  27) HU.@?= 'b'),
            HU.testCase "Stub #u12" (Cd.chFromCode (TH.Stub  28) HU.@?= 'c'),
            HU.testCase "Stub #u13" (Cd.chFromCode (TH.Stub  47) HU.@?= 'v'),
            HU.testCase "Stub #u14" (Cd.chFromCode (TH.Stub  48) HU.@?= 'w'),
            HU.testCase "Stub #u15" (Cd.chFromCode (TH.Stub  49) HU.@?= 'x'),
            HU.testCase "Stub #u16" (Cd.chFromCode (TH.Stub  50) HU.@?= '0'),
            HU.testCase "Stub #u17" (Cd.chFromCode (TH.Stub  51) HU.@?= '1'),
            HU.testCase "Stub #u18" (Cd.chFromCode (TH.Stub  52) HU.@?= '2'),
            HU.testCase "Stub #u20" (Cd.chFromCode (TH.Stub  72) HU.@?= 'F'),
            HU.testCase "Stub #u21" (Cd.chFromCode (TH.Stub  73) HU.@?= 'G'),
            HU.testCase "Stub #u22" (Cd.chFromCode (TH.Stub  74) HU.@?= 'H'),
            HU.testCase "Stub #u23" (Cd.chFromCode (TH.Stub  75) HU.@?= '\0'),
            HU.testCase "Stub #u24" (Cd.chFromCode (TH.Stub  76) HU.@?= 'a'),
            HU.testCase "Stub #u25" (Cd.chFromCode (TH.Stub  77) HU.@?= 'b'),
            HU.testCase "Stub #u26" (Cd.chFromCode (TH.Stub  78) HU.@?= 'c'),
            HU.testCase "Stub #u27" (Cd.chFromCode (TH.Stub  97) HU.@?= 'v'),
            HU.testCase "Stub #u28" (Cd.chFromCode (TH.Stub  98) HU.@?= 'w'),
            HU.testCase "Stub #u29" (Cd.chFromCode (TH.Stub  99) HU.@?= 'x'),
            HU.testCase "Stub #u30" (Cd.chFromCode (TH.Stub 100) HU.@?= '?'),
            HU.testCase "Stub #u31" (Cd.chFromCode (TH.Stub 101) HU.@?= '?'),
            QC.testProperty "Char #p1" (\cd -> True QC.==> Cd.chFromCode (cd :: Char) == cd),
            HU.testCase   "Char #u1" (Cd.chFromCode '\x0' HU.@?= '\x0'),
            HU.testCase   "Char #u2" (Cd.chFromCode '\x1' HU.@?= '\x1'),
            HU.testCase   "Char #u3" (Cd.chFromCode 'a' HU.@?= 'a'),
            HU.testCase   "Char #u4" (Cd.chFromCode 'b' HU.@?= 'b'),
            HU.testCase   "Char #u5" (Cd.chFromCode 'y' HU.@?= 'y'),
            HU.testCase   "Char #u6" (Cd.chFromCode 'z' HU.@?= 'z'),
            HU.testCase   "Char #u7" (Cd.chFromCode 'A' HU.@?= 'A'),
            HU.testCase   "Char #u8" (Cd.chFromCode 'B' HU.@?= 'B'),
            HU.testCase  "Char #u19" (Cd.chFromCode 'Y' HU.@?= 'Y'),
            HU.testCase  "Char #u10" (Cd.chFromCode 'Z' HU.@?= 'Z'),
            HU.testCase  "Char #u11" (Cd.chFromCode '0' HU.@?= '0'),
            HU.testCase  "Char #u12" (Cd.chFromCode '1' HU.@?= '1'),
            HU.testCase  "Char #u13" (Cd.chFromCode '8' HU.@?= '8'),
            HU.testCase  "Char #u14" (Cd.chFromCode '9' HU.@?= '9'),
            HU.testCase  "Char #u15" (Cd.chFromCode '\x10FFFE' HU.@?= '\x10FFFE'),
            HU.testCase  "Char #u16" (Cd.chFromCode '\x10FFFF' HU.@?= '\x10FFFF'),
            QC.testProperty "CharUtf8 - #p1" (\cd -> True QC.==>  Cd.chFromCode (cd :: Cd.CharUtf8) == chFromCharUtf8' cd),
            HU.testCase  "CharUtf8 #u1" (Cd.chFromCode (Cd.CU8OneByte 0b00000000) HU.@?= '\x0'),
            HU.testCase  "CharUtf8 #u2" (Cd.chFromCode (Cd.CU8TwoBytes 0b11000000 0b10000000) HU.@?= '\x0'),
            HU.testCase  "CharUtf8 #u3" (Cd.chFromCode (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b10000000) HU.@?= '\x0'),
            HU.testCase  "CharUtf8 #u4" (Cd.chFromCode (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b10000000) HU.@?= '\x0'),
            HU.testCase  "CharUtf8 #u5" (Cd.chFromCode (Cd.CU8OneByte 0b00000001) HU.@?= '\x1'),
            HU.testCase  "CharUtf8 #u6" (Cd.chFromCode (Cd.CU8TwoBytes 0b11000000 0b10000001) HU.@?= '\x1'),
            HU.testCase  "CharUtf8 #u7" (Cd.chFromCode (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b10000001) HU.@?= '\x1'),
            HU.testCase  "CharUtf8 #u8" (Cd.chFromCode (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b10000001) HU.@?= '\x1'),
            HU.testCase  "CharUtf8 #u9" (Cd.chFromCode (Cd.CU8OneByte 0b01000001) HU.@?= 'A'),
            HU.testCase "CharUtf8 #u10" (Cd.chFromCode (Cd.CU8TwoBytes 0b11000001 0b10000001) HU.@?= 'A'),
            HU.testCase "CharUtf8 #u11" (Cd.chFromCode (Cd.CU8ThreeBytes 0b11100000 0b10000001 0b10000001) HU.@?= 'A'),
            HU.testCase "CharUtf8 #u12" (Cd.chFromCode (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000001 0b10000001) HU.@?= 'A'),
            HU.testCase "CharUtf8 #u13" (Cd.chFromCode (Cd.CU8OneByte 0b01111110) HU.@?= '\x7E'),
            HU.testCase "CharUtf8 #u14" (Cd.chFromCode (Cd.CU8TwoBytes 0b11000001 0b10111110) HU.@?= '\x7E'),
            HU.testCase "CharUtf8 #u15" (Cd.chFromCode (Cd.CU8ThreeBytes 0b11100000 0b10000001 0b10111110) HU.@?= '\x7E'),
            HU.testCase "CharUtf8 #u16" (Cd.chFromCode (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000001 0b10111110) HU.@?= '\x7E'),
            HU.testCase "CharUtf8 #u17" (Cd.chFromCode (Cd.CU8OneByte 0b01111111) HU.@?= '\x7F'),
            HU.testCase "CharUtf8 #u18" (Cd.chFromCode (Cd.CU8TwoBytes 0b11000001 0b10111111) HU.@?= '\x7F'),
            HU.testCase "CharUtf8 #u19" (Cd.chFromCode (Cd.CU8ThreeBytes 0b11100000 0b10000001 0b10111111) HU.@?= '\x7F'),
            HU.testCase "CharUtf8 #u20" (Cd.chFromCode (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000001 0b10111111) HU.@?= '\x7F'),
            HU.testCase "CharUtf8 #u21" (Cd.chFromCode (Cd.CU8OneByte 0b10000000) HU.@?= Cd.chFromCode (Cd.cdReplacement :: Cd.CharUtf8)),
            HU.testCase "CharUtf8 #u22" (Cd.chFromCode (Cd.CU8TwoBytes 0b11000010 0b10000000) HU.@?= '\x80'),
            HU.testCase "CharUtf8 #u23" (Cd.chFromCode (Cd.CU8ThreeBytes 0b11100000 0b10000010 0b10000000) HU.@?= '\x80'),
            HU.testCase "CharUtf8 #u24" (Cd.chFromCode (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000010 0b10000000) HU.@?= '\x80'),
            HU.testCase "CharUtf8 #u25" (Cd.chFromCode (Cd.CU8OneByte 0b10000001) HU.@?= Cd.chFromCode (Cd.cdReplacement :: Cd.CharUtf8)),
            HU.testCase "CharUtf8 #u26" (Cd.chFromCode (Cd.CU8TwoBytes 0b11000010 0b10000001) HU.@?= '\x81'),
            HU.testCase "CharUtf8 #u27" (Cd.chFromCode (Cd.CU8ThreeBytes 0b11100000 0b10000010 0b10000001) HU.@?= '\x81'),
            HU.testCase "CharUtf8 #u28" (Cd.chFromCode (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000010 0b10000001) HU.@?= '\x81'),
            HU.testCase "CharUtf8 #u29" (Cd.chFromCode (Cd.CU8TwoBytes 0b11011111 0b10111110) HU.@?= '\x7FE'),
            HU.testCase "CharUtf8 #u30" (Cd.chFromCode (Cd.CU8ThreeBytes 0b11100000 0b10011111 0b10111110) HU.@?= '\x7FE'),
            HU.testCase "CharUtf8 #u31" (Cd.chFromCode (Cd.CU8FourBytes 0b11110000 0b10000000 0b10011111 0b10111110) HU.@?= '\x7FE'),
            HU.testCase "CharUtf8 #u32" (Cd.chFromCode (Cd.CU8TwoBytes 0b11011111 0b10111111) HU.@?= '\x7FF'),
            HU.testCase "CharUtf8 #u33" (Cd.chFromCode (Cd.CU8ThreeBytes 0b11100000 0b10011111 0b10111111) HU.@?= '\x7FF'),
            HU.testCase "CharUtf8 #u34" (Cd.chFromCode (Cd.CU8FourBytes 0b11110000 0b10000000 0b10011111 0b10111111) HU.@?= '\x7FF'),
            HU.testCase "CharUtf8 #u35" (Cd.chFromCode (Cd.CU8TwoBytes 0b11100000 0b10000000) HU.@?= Cd.chFromCode (Cd.cdReplacement :: Cd.CharUtf8)),
            HU.testCase "CharUtf8 #u36" (Cd.chFromCode (Cd.CU8ThreeBytes 0b11100000 0b10100000 0b10000000) HU.@?= '\x800'),
            HU.testCase "CharUtf8 #u37" (Cd.chFromCode (Cd.CU8FourBytes 0b11110000 0b10000000 0b10100000 0b10000000) HU.@?= '\x800'),
            HU.testCase "CharUtf8 #u38" (Cd.chFromCode (Cd.CU8TwoBytes 0b11100000 0b10000001) HU.@?= Cd.chFromCode (Cd.cdReplacement :: Cd.CharUtf8)),
            HU.testCase "CharUtf8 #u39" (Cd.chFromCode (Cd.CU8ThreeBytes 0b11100000 0b10100000 0b10000001) HU.@?= '\x801'),
            HU.testCase "CharUtf8 #u40" (Cd.chFromCode (Cd.CU8FourBytes 0b11110000 0b10000000 0b10100000 0b10000001) HU.@?= '\x801'),
            HU.testCase "CharUtf8 #u41" (Cd.chFromCode (Cd.CU8ThreeBytes 0b11101111 0b10111111 0b10111110) HU.@?= '\xFFFE'),
            HU.testCase "CharUtf8 #u42" (Cd.chFromCode (Cd.CU8FourBytes 0b11110000 0b10001111 0b10111111 0b10111110) HU.@?= '\xFFFE'),
            HU.testCase "CharUtf8 #u43" (Cd.chFromCode (Cd.CU8ThreeBytes 0b11101111 0b10111111 0b10111111) HU.@?= '\xFFFF'),
            HU.testCase "CharUtf8 #u44" (Cd.chFromCode (Cd.CU8FourBytes 0b11110000 0b10001111 0b10111111 0b10111111) HU.@?= '\xFFFF'),
            HU.testCase "CharUtf8 #u45" (Cd.chFromCode (Cd.CU8ThreeBytes 0b11110000 0b10000000 0b10000000) HU.@?= Cd.chFromCode (Cd.cdReplacement :: Cd.CharUtf8)),
            HU.testCase "CharUtf8 #u46" (Cd.chFromCode (Cd.CU8FourBytes 0b11110000 0b10010000 0b10000000 0b10000000) HU.@?= '\x10000'),
            HU.testCase "CharUtf8 #u47" (Cd.chFromCode (Cd.CU8ThreeBytes 0b11110000 0b10000000 0b10000001) HU.@?= Cd.chFromCode (Cd.cdReplacement :: Cd.CharUtf8)),
            HU.testCase "CharUtf8 #u48" (Cd.chFromCode (Cd.CU8FourBytes 0b11110000 0b10010000 0b10000000 0b10000001) HU.@?= '\x10001'),
            HU.testCase "CharUtf8 #u49" (Cd.chFromCode (Cd.CU8FourBytes 0b11110100 0b10001111 0b10111111 0b10111110) HU.@?= '\x10FFFE'),
            HU.testCase "CharUtf8 #u50" (Cd.chFromCode (Cd.CU8FourBytes 0b11110100 0b10001111 0b10111111 0b10111111) HU.@?= '\x10FFFF'),
            HU.testCase "CharUtf8 #u51" (Cd.chFromCode (Cd.CU8FourBytes 0b11110100 0b10010000 0b10000000 0b10000000) HU.@?= Cd.chFromCode (Cd.cdReplacement :: Cd.CharUtf8)),
            QC.testProperty "CharIso1 - #p1" (\cd -> True QC.==>  Cd.chFromCode (cd :: Cd.CharIso1) == chFromCharIso1' cd),
            HU.testCase  "CharIso1 #u1" (Cd.chFromCode (Cd.CharIso1 0x0 ) HU.@?= '\x0'), -- NUL
            HU.testCase  "CharIso1 #u2" (Cd.chFromCode (Cd.CharIso1 0x1 ) HU.@?= '\x1'), -- SOH (Start of Heading)
            HU.testCase  "CharIso1 #u3" (Cd.chFromCode (Cd.CharIso1 0x1E ) HU.@?= '\x1E'), -- RS (Record Seperator)
            HU.testCase  "CharIso1 #u4" (Cd.chFromCode (Cd.CharIso1 0x1F ) HU.@?= '\x1F'), -- RS (Unit Seperator)
            HU.testCase  "CharIso1 #u5" (Cd.chFromCode (Cd.CharIso1 0x20 ) HU.@?= ' '),
            HU.testCase  "CharIso1 #u6" (Cd.chFromCode (Cd.CharIso1 0x30 ) HU.@?= '0'),
            HU.testCase  "CharIso1 #u7" (Cd.chFromCode (Cd.CharIso1 0x31 ) HU.@?= '1'),
            HU.testCase  "CharIso1 #u8" (Cd.chFromCode (Cd.CharIso1 0x38 ) HU.@?= '8'),
            HU.testCase  "CharIso1 #u9" (Cd.chFromCode (Cd.CharIso1 0x39 ) HU.@?= '9'),
            HU.testCase "CharIso1 #u10" (Cd.chFromCode (Cd.CharIso1 0x3F ) HU.@?= '?'),
            HU.testCase "CharIso1 #u11" (Cd.chFromCode (Cd.CharIso1 0x41 ) HU.@?= 'A'),
            HU.testCase "CharIso1 #u12" (Cd.chFromCode (Cd.CharIso1 0x42 ) HU.@?= 'B'),
            HU.testCase "CharIso1 #u13" (Cd.chFromCode (Cd.CharIso1 0x59 ) HU.@?= 'Y'),
            HU.testCase "CharIso1 #u14" (Cd.chFromCode (Cd.CharIso1 0x5A ) HU.@?= 'Z'),
            HU.testCase "CharIso1 #u15" (Cd.chFromCode (Cd.CharIso1 0x61 ) HU.@?= 'a'),
            HU.testCase "CharIso1 #u16" (Cd.chFromCode (Cd.CharIso1 0x62 ) HU.@?= 'b'),
            HU.testCase "CharIso1 #u17" (Cd.chFromCode (Cd.CharIso1 0x79 ) HU.@?= 'y'),
            HU.testCase "CharIso1 #u18" (Cd.chFromCode (Cd.CharIso1 0x7A ) HU.@?= 'z'),
            HU.testCase "CharIso1 #u19" (Cd.chFromCode (Cd.CharIso1 0x7E ) HU.@?= '~'),
            HU.testCase "CharIso1 #u20" (Cd.chFromCode (Cd.CharIso1 0x7F ) HU.@?= '\x7F'), -- U+00A0 = DEL (Delete)
            HU.testCase "CharIso1 #u21" (Cd.chFromCode (Cd.CharIso1 0x80 ) HU.@?= '\xFFFD'), -- replacement character ('�')
            HU.testCase "CharIso1 #u22" (Cd.chFromCode (Cd.CharIso1 0x81 ) HU.@?= '\xFFFD'), -- replacement character ('�')
            HU.testCase "CharIso1 #u23" (Cd.chFromCode (Cd.CharIso1 0x9E ) HU.@?= '\xFFFD'), -- replacement character ('�')
            HU.testCase "CharIso1 #u24" (Cd.chFromCode (Cd.CharIso1 0x9F ) HU.@?= '\xFFFD'), -- replacement character ('�')
            HU.testCase "CharIso1 #u25" (Cd.chFromCode (Cd.CharIso1 0xA0 ) HU.@?= '\xA0'), -- U+00A0 = NBSP (No-Break Space)
            HU.testCase "CharIso1 #u26" (Cd.chFromCode (Cd.CharIso1 0xA1 ) HU.@?= '¡'),
            HU.testCase "CharIso1 #u27" (Cd.chFromCode (Cd.CharIso1 0xFE ) HU.@?= 'þ'),
            HU.testCase "CharIso1 #u28" (Cd.chFromCode (Cd.CharIso1 0xFF ) HU.@?= 'ÿ'),
            QC.testProperty "CharWin1 - #p1" (\cd -> True QC.==>  Cd.chFromCode (cd :: Cd.CharWin1) == chFromCharWin1' cd),
            HU.testCase  "CharWin1 #u1" (Cd.chFromCode (Cd.CharWin1 0x0 ) HU.@?= '\x0'), -- NUL
            HU.testCase  "CharWin1 #u2" (Cd.chFromCode (Cd.CharWin1 0x1 ) HU.@?= '\x1'), -- SOH (Start of Heading)
            HU.testCase  "CharWin1 #u3" (Cd.chFromCode (Cd.CharWin1 0x1E ) HU.@?= '\x1E'), -- RS (Record Seperator)
            HU.testCase  "CharWin1 #u4" (Cd.chFromCode (Cd.CharWin1 0x1F ) HU.@?= '\x1F'), -- RS (Unit Seperator)
            HU.testCase  "CharWin1 #u5" (Cd.chFromCode (Cd.CharWin1 0x20 ) HU.@?= ' '),
            HU.testCase  "CharWin1 #u6" (Cd.chFromCode (Cd.CharWin1 0x30 ) HU.@?= '0'),
            HU.testCase  "CharWin1 #u7" (Cd.chFromCode (Cd.CharWin1 0x31 ) HU.@?= '1'),
            HU.testCase  "CharWin1 #u8" (Cd.chFromCode (Cd.CharWin1 0x38 ) HU.@?= '8'),
            HU.testCase  "CharWin1 #u9" (Cd.chFromCode (Cd.CharWin1 0x39 ) HU.@?= '9'),
            HU.testCase "CharWin1 #u10" (Cd.chFromCode (Cd.CharWin1 0x3F ) HU.@?= '?'),
            HU.testCase "CharWin1 #u11" (Cd.chFromCode (Cd.CharWin1 0x41 ) HU.@?= 'A'),
            HU.testCase "CharWin1 #u12" (Cd.chFromCode (Cd.CharWin1 0x42 ) HU.@?= 'B'),
            HU.testCase "CharWin1 #u13" (Cd.chFromCode (Cd.CharWin1 0x59 ) HU.@?= 'Y'),
            HU.testCase "CharWin1 #u14" (Cd.chFromCode (Cd.CharWin1 0x5A ) HU.@?= 'Z'),
            HU.testCase "CharWin1 #u15" (Cd.chFromCode (Cd.CharWin1 0x61 ) HU.@?= 'a'),
            HU.testCase "CharWin1 #u16" (Cd.chFromCode (Cd.CharWin1 0x62 ) HU.@?= 'b'),
            HU.testCase "CharWin1 #u17" (Cd.chFromCode (Cd.CharWin1 0x79 ) HU.@?= 'y'),
            HU.testCase "CharWin1 #u18" (Cd.chFromCode (Cd.CharWin1 0x7A ) HU.@?= 'z'),
            HU.testCase "CharWin1 #u19" (Cd.chFromCode (Cd.CharWin1 0x7E ) HU.@?= '~'),
            HU.testCase "CharWin1 #u20" (Cd.chFromCode (Cd.CharWin1 0x7F ) HU.@?= '\x7F'), -- U+00A0 = DEL (Delete)
            HU.testCase "CharWin1 #u21" (Cd.chFromCode (Cd.CharWin1 0x80 ) HU.@?= '€'), -- U+20AC
            HU.testCase "CharWin1 #u22" (Cd.chFromCode (Cd.CharWin1 0x81 ) HU.@?= '\xFFFD'), -- replacement character ('�')
            HU.testCase "CharWin1 #u23" (Cd.chFromCode (Cd.CharWin1 0x82 ) HU.@?= '‚'), -- U+201A
            HU.testCase "CharWin1 #u24" (Cd.chFromCode (Cd.CharWin1 0x83 ) HU.@?= 'ƒ'), -- U+0192
            HU.testCase "CharWin1 #u25" (Cd.chFromCode (Cd.CharWin1 0x84 ) HU.@?= '„'), -- U+201E
            HU.testCase "CharWin1 #u26" (Cd.chFromCode (Cd.CharWin1 0x85 ) HU.@?= '…'), -- U+2026
            HU.testCase "CharWin1 #u27" (Cd.chFromCode (Cd.CharWin1 0x86 ) HU.@?= '†'), -- U+2020
            HU.testCase "CharWin1 #u28" (Cd.chFromCode (Cd.CharWin1 0x87 ) HU.@?= '‡'), -- U+2021
            HU.testCase "CharWin1 #u29" (Cd.chFromCode (Cd.CharWin1 0x88 ) HU.@?= 'ˆ'), -- U+20C6
            HU.testCase "CharWin1 #u30" (Cd.chFromCode (Cd.CharWin1 0x89 ) HU.@?= '‰'), -- U+2030
            HU.testCase "CharWin1 #u31" (Cd.chFromCode (Cd.CharWin1 0x8A ) HU.@?= 'Š'), -- U+0160
            HU.testCase "CharWin1 #u32" (Cd.chFromCode (Cd.CharWin1 0x8B ) HU.@?= '‹'), -- U+2039
            HU.testCase "CharWin1 #u33" (Cd.chFromCode (Cd.CharWin1 0x8C ) HU.@?= 'Œ'), -- U+0152
            HU.testCase "CharWin1 #u34" (Cd.chFromCode (Cd.CharWin1 0x8D ) HU.@?= '\xFFFD'), -- replacement character ('�')
            HU.testCase "CharWin1 #u35" (Cd.chFromCode (Cd.CharWin1 0x8E ) HU.@?= 'Ž'), -- U+017D
            HU.testCase "CharWin1 #u36" (Cd.chFromCode (Cd.CharWin1 0x8F ) HU.@?= '\xFFFD'), -- replacement character ('�')
            HU.testCase "CharWin1 #u37" (Cd.chFromCode (Cd.CharWin1 0x90 ) HU.@?= '\xFFFD'), -- replacement character ('�')
            HU.testCase "CharWin1 #u38" (Cd.chFromCode (Cd.CharWin1 0x91 ) HU.@?= '‘'), -- U+2018
            HU.testCase "CharWin1 #u39" (Cd.chFromCode (Cd.CharWin1 0x92 ) HU.@?= '’'), -- U+2019
            HU.testCase "CharWin1 #u40" (Cd.chFromCode (Cd.CharWin1 0x93 ) HU.@?= '“'), -- U+201C
            HU.testCase "CharWin1 #u41" (Cd.chFromCode (Cd.CharWin1 0x94 ) HU.@?= '”'), -- U+201D
            HU.testCase "CharWin1 #u42" (Cd.chFromCode (Cd.CharWin1 0x95 ) HU.@?= '•'), -- U+2022
            HU.testCase "CharWin1 #u43" (Cd.chFromCode (Cd.CharWin1 0x96 ) HU.@?= '–'), -- U+2013
            HU.testCase "CharWin1 #u44" (Cd.chFromCode (Cd.CharWin1 0x97 ) HU.@?= '—'), -- U+2014
            HU.testCase "CharWin1 #u45" (Cd.chFromCode (Cd.CharWin1 0x98 ) HU.@?= '˜'), -- U+02DC
            HU.testCase "CharWin1 #u46" (Cd.chFromCode (Cd.CharWin1 0x99 ) HU.@?= '™'), -- U+2122
            HU.testCase "CharWin1 #u47" (Cd.chFromCode (Cd.CharWin1 0x9A ) HU.@?= 'š'), -- U+0161
            HU.testCase "CharWin1 #u48" (Cd.chFromCode (Cd.CharWin1 0x9B ) HU.@?= '›'), -- U+203A
            HU.testCase "CharWin1 #u49" (Cd.chFromCode (Cd.CharWin1 0x9C ) HU.@?= 'œ'), -- U+0153
            HU.testCase "CharWin1 #u50" (Cd.chFromCode (Cd.CharWin1 0x9D ) HU.@?= '\xFFFD'), -- replacement character ('�')
            HU.testCase "CharWin1 #u51" (Cd.chFromCode (Cd.CharWin1 0x9E ) HU.@?= 'ž'), -- U+017E
            HU.testCase "CharWin1 #u52" (Cd.chFromCode (Cd.CharWin1 0x9F ) HU.@?= 'Ÿ'), -- U+0178
            HU.testCase "CharWin1 #u53" (Cd.chFromCode (Cd.CharWin1 0xA0 ) HU.@?= '\xA0'), -- U+00A0 = NBSP (No-Break Space)
            HU.testCase "CharWin1 #u54" (Cd.chFromCode (Cd.CharWin1 0xA1 ) HU.@?= '¡'),
            HU.testCase "CharWin1 #u55" (Cd.chFromCode (Cd.CharWin1 0xFE ) HU.@?= 'þ'),
            HU.testCase "CharWin1 #u56" (Cd.chFromCode (Cd.CharWin1 0xFF ) HU.@?= 'ÿ'),
            QC.testProperty "Octets.Octet - #p1" (\cd -> True QC.==>  Cd.chFromCode (cd :: Oct.Octet) == chFromOctet' cd),
            HU.testCase  "Octets.Octet #u1" (Cd.chFromCode (0x00 :: Oct.Octet) HU.@?= '\x0'), -- NUL
            HU.testCase  "Octets.Octet #u2" (Cd.chFromCode (0x01 :: Oct.Octet) HU.@?= '\x1'), -- SOH (Start of Heading)
            HU.testCase  "Octets.Octet #u3" (Cd.chFromCode (0x1E :: Oct.Octet) HU.@?= '\x1E'), -- RS (Record Seperator)
            HU.testCase  "Octets.Octet #u4" (Cd.chFromCode (0x1F :: Oct.Octet) HU.@?= '\x1F'), -- RS (Unit Seperator)
            HU.testCase  "Octets.Octet #u5" (Cd.chFromCode (0x20 :: Oct.Octet) HU.@?= ' '),
            HU.testCase  "Octets.Octet #u6" (Cd.chFromCode (0x30 :: Oct.Octet) HU.@?= '0'),
            HU.testCase  "Octets.Octet #u7" (Cd.chFromCode (0x31 :: Oct.Octet) HU.@?= '1'),
            HU.testCase  "Octets.Octet #u8" (Cd.chFromCode (0x38 :: Oct.Octet) HU.@?= '8'),
            HU.testCase  "Octets.Octet #u9" (Cd.chFromCode (0x39 :: Oct.Octet) HU.@?= '9'),
            HU.testCase "Octets.Octet #u10" (Cd.chFromCode (0x3F :: Oct.Octet) HU.@?= '?'),
            HU.testCase "Octets.Octet #u11" (Cd.chFromCode (0x41 :: Oct.Octet) HU.@?= 'A'),
            HU.testCase "Octets.Octet #u12" (Cd.chFromCode (0x42 :: Oct.Octet) HU.@?= 'B'),
            HU.testCase "Octets.Octet #u13" (Cd.chFromCode (0x59 :: Oct.Octet) HU.@?= 'Y'),
            HU.testCase "Octets.Octet #u14" (Cd.chFromCode (0x5A :: Oct.Octet) HU.@?= 'Z'),
            HU.testCase "Octets.Octet #u15" (Cd.chFromCode (0x61 :: Oct.Octet) HU.@?= 'a'),
            HU.testCase "Octets.Octet #u16" (Cd.chFromCode (0x62 :: Oct.Octet) HU.@?= 'b'),
            HU.testCase "Octets.Octet #u17" (Cd.chFromCode (0x79 :: Oct.Octet) HU.@?= 'y'),
            HU.testCase "Octets.Octet #u18" (Cd.chFromCode (0x7A :: Oct.Octet) HU.@?= 'z'),
            HU.testCase "Octets.Octet #u19" (Cd.chFromCode (0x7E :: Oct.Octet) HU.@?= '~'),
            HU.testCase "Octets.Octet #u20" (Cd.chFromCode (0x7F :: Oct.Octet) HU.@?= '\x7F'), -- U+00A0 = DEL (Delete)
            HU.testCase "Octets.Octet #u21" (Cd.chFromCode (0x80 :: Oct.Octet) HU.@?= '\x80'), -- U+0080 = PAD (Padding Character)
            HU.testCase "Octets.Octet #u22" (Cd.chFromCode (0x81 :: Oct.Octet) HU.@?= '\x81'), -- U+0080 = HOP (High Octet Preset)
            HU.testCase "Octets.Octet #u23" (Cd.chFromCode (0x9E :: Oct.Octet) HU.@?= '\x9E'), -- U+0080 = PM (Privacy Message)
            HU.testCase "Octets.Octet #u24" (Cd.chFromCode (0x9F :: Oct.Octet) HU.@?= '\x9F'), -- U+0080 = APC (Application Program Command)
            HU.testCase "Octets.Octet #u25" (Cd.chFromCode (0xA0 :: Oct.Octet) HU.@?= '\xA0'), -- U+00A0 = NBSP (No-Break Space)
            HU.testCase "Octets.Octet #u26" (Cd.chFromCode (0xA1 :: Oct.Octet) HU.@?= '¡'),
            HU.testCase "Octets.Octet #u27" (Cd.chFromCode (0xFE :: Oct.Octet) HU.@?= 'þ'),
            HU.testCase "Octets.Octet #u28" (Cd.chFromCode (0xFF :: Oct.Octet) HU.@?= 'ÿ'),
            HU.testCase "Bool #u1" (Cd.chFromCode False HU.@?= '0'),
            HU.testCase "Bool #u2" (Cd.chFromCode True HU.@?= '1')
        ]

{-  * validated: ✅
        * completeness: ✅
        * independence: ✅
        * edge cases  : ✅
        * conform doc.: ✅ -}
tgUnitcdFromChar :: T.TestTree
tgUnitcdFromChar =
    T.testGroup
    "cdFromChar"
    (
        QC.testProperty "Stub #p1" (\oct -> True QC.==>  Cd.cdFromChar (Chr.chr (fromIntegral oct)) == stbFromOct' oct) : 
        tgUnitcdFromCharUnitTestsForStub lchEdgeCases ++
        QC.testProperty "Char #p1" (\ch -> True QC.==>  Cd.cdFromChar ch == ch) : 
        tgUnitcdFromCharUnitTestsForStub lchEdgeCases ++
        QC.testProperty "CharUtf8 #p1" (\ch -> True QC.==>  Cd.cdFromChar ch == cu8FromChar' ch) : 
        tgUnitcdFromCharUnitTestsForCharUtf8 lchEdgeCases ++
        QC.testProperty "CharIso1 #p1" (\ch -> True QC.==>  Cd.cdFromChar ch == ci1FromChar' ch) : 
        tgUnitcdFromCharUnitTestsForCharIso1 lchEdgeCases ++
        QC.testProperty "CharWin1 #p1" (\ch -> True QC.==>  Cd.cdFromChar ch == cw1FromChar' ch) : 
        tgUnitcdFromCharUnitTestsForCharWin1 lchEdgeCases ++
        QC.testProperty "Octets.Octet #p1" (\ch -> True QC.==>  Cd.cdFromChar ch == octFromChar' ch) : 
        tgUnitcdFromCharUnitTestsForOctet lchEdgeCases ++ 
        QC.testProperty "Bool #p1" (\ch -> True QC.==>  Cd.cdFromChar ch == isFromChar' ch) : 
        tgUnitcdFromCharUnitTestsForBool lchEdgeCases
    )

tgUnitcdFromCharUnitTestsForStub :: [Char] -> [T.TestTree]
tgUnitcdFromCharUnitTestsForStub lx = 
    fmap 
        (\tpl -> 
            HU.testCase ("Stub #u" ++ show (snd tpl)) (Cd.cdFromChar (fst tpl) HU.@?= stbFromChar' (fst tpl)))  
        (zip lx [1,2..])

tgUnitcdFromCharUnitTestsForChar :: [Char] -> [T.TestTree]
tgUnitcdFromCharUnitTestsForChar lx = 
    fmap 
        (\tpl -> 
            HU.testCase ("Char #u" ++ show (snd tpl)) (Cd.cdFromChar (fst tpl) HU.@?= fst tpl))  
        (zip lx [1,2..])

tgUnitcdFromCharUnitTestsForCharUtf8 :: [Char] -> [T.TestTree]
tgUnitcdFromCharUnitTestsForCharUtf8 lx = 
    fmap 
        (\tpl -> 
            HU.testCase ("CharUtf8 #u" ++ show (snd tpl)) (Cd.cdFromChar (fst tpl) HU.@?= cu8FromChar' (fst tpl)))  
        (zip lx [1,2..])

tgUnitcdFromCharUnitTestsForCharIso1 :: [Char] -> [T.TestTree]
tgUnitcdFromCharUnitTestsForCharIso1 lx = 
    fmap 
        (\tpl -> 
            HU.testCase ("CharIso1 #u" ++ show (snd tpl)) (Cd.cdFromChar (fst tpl) HU.@?= ci1FromChar' (fst tpl)))  
        (zip lx [1,2..])

tgUnitcdFromCharUnitTestsForCharWin1 :: [Char] -> [T.TestTree]
tgUnitcdFromCharUnitTestsForCharWin1 lx = 
    fmap 
        (\tpl -> 
            HU.testCase ("CharWin1 #u" ++ show (snd tpl)) (Cd.cdFromChar (fst tpl) HU.@?= cw1FromChar' (fst tpl)))  
        (zip lx [1,2..])

tgUnitcdFromCharUnitTestsForOctet :: [Char] -> [T.TestTree]
tgUnitcdFromCharUnitTestsForOctet lx = 
    fmap 
        (\tpl -> 
            HU.testCase ("Octets.Octet #u" ++ show (snd tpl)) (Cd.cdFromChar (fst tpl) HU.@?= octFromChar' (fst tpl)))  
        (zip lx [1,2..])

tgUnitcdFromCharUnitTestsForBool :: [Char] -> [T.TestTree]
tgUnitcdFromCharUnitTestsForBool lx = 
    fmap 
        (\tpl -> 
            HU.testCase ("Bool #u" ++ show (snd tpl)) (Cd.cdFromChar (fst tpl) HU.@?= isFromChar' (fst tpl)))  
        (zip lx [1,2..])

{-  * validated: ✅
        * completeness: ✅
        * independence: ✅
        * edge cases  : ✅
        * conform doc.: ✅ -}
tgUnitroctCI1 :: T.TestTree
tgUnitroctCI1 =
    T.testGroup
    "roctCI1"
    [
        QC.testProperty "CharIso1 #p1" (\(ci1 :: Cd.CharIso1) -> True QC.==>  Cd.roctCI1 ci1 == roctCI1' ci1),
        HU.testCase  "CharIso1 #u1" (Cd.roctCI1 (Cd.CharIso1 0x00) HU.@?= 0x00),
        HU.testCase  "CharIso1 #u2" (Cd.roctCI1 (Cd.CharIso1 0x01) HU.@?= 0x01),
        HU.testCase  "CharIso1 #u3" (Cd.roctCI1 (Cd.CharIso1 0xFE) HU.@?= 0xFE),
        HU.testCase  "CharIso1 #u4" (Cd.roctCI1 (Cd.CharIso1 0xFF) HU.@?= 0xFF)
    ]

roctCI1' :: Cd.CharIso1 -> Oct.Octet
roctCI1' (Cd.CharIso1 oct) = oct

{-  * validated: ✅
        * completeness: ✅
        * independence: ✅
        * edge cases  : ✅
        * conform doc.: ✅ -}
tgUnitroctCW1 :: T.TestTree
tgUnitroctCW1 =
    T.testGroup
    "roctCW1"
    [
        QC.testProperty "CharWin1 #p1" (\(cw1 :: Cd.CharWin1) -> True QC.==>  Cd.roctCW1 cw1 == roctCW1' cw1),
        HU.testCase  "CharWin1 #u1" (Cd.roctCW1 (Cd.CharWin1 0x00) HU.@?= 0x00),
        HU.testCase  "CharWin1 #u2" (Cd.roctCW1 (Cd.CharWin1 0x01) HU.@?= 0x01),
        HU.testCase  "CharWin1 #u3" (Cd.roctCW1 (Cd.CharWin1 0xFE) HU.@?= 0xFE),
        HU.testCase  "CharWin1 #u4" (Cd.roctCW1 (Cd.CharWin1 0xFF) HU.@?= 0xFF)
    ]

roctCW1' :: Cd.CharWin1 -> Oct.Octet
roctCW1' (Cd.CharWin1 oct) = oct

--------------------------------------------------------------------------------
--  reference functions

isValidCharUtf8' :: Cd.CharUtf8 -> Bool
isValidCharUtf8' (Cd.CU8OneByte oct1) =
    oct1 .&. 0b10000000 == 0
isValidCharUtf8' (Cd.CU8TwoBytes oct1 oct2) =
    oct1 .&. 0b11100000 == 0b11000000 &&
    oct2 .&. 0b11000000 == 0b10000000
isValidCharUtf8' (Cd.CU8ThreeBytes oct1 oct2 oct3) =
    oct1 .&. 0b11110000 == 0b11100000 &&
    oct2 .&. 0b11000000 == 0b10000000 &&
    oct3 .&. 0b11000000 == 0b10000000
isValidCharUtf8' cd@(Cd.CU8FourBytes oct1 oct2 oct3 oct4) =
    oct1 .&. 0b11111000 == 0b11110000 &&
    oct2 .&. 0b11000000 == 0b10000000 &&
    oct3 .&. 0b11000000 == 0b10000000 &&
    oct4 .&. 0b11000000 == 0b10000000 &&
    niOrdFromCU8' cd <= 0x10FFFF

niOrdFromCU8' :: Cd.CharUtf8 -> Integer
niOrdFromCU8' cu8@(Cd.CU8OneByte oct1) =
         fromIntegral (oct1 .&. 0b01111111)
niOrdFromCU8' cu8@(Cd.CU8TwoBytes oct1 oct2) =
        (fromIntegral (oct1 .&. 0b00011111) * (1 `Bts.shift` 6)) +
         fromIntegral (oct2 .&. 0b00111111)
niOrdFromCU8' cu8@(Cd.CU8ThreeBytes oct1 oct2 oct3) =
        (fromIntegral (oct1 .&. 0b00001111) * (1 `Bts.shift` 12)) +
        (fromIntegral (oct2 .&. 0b00111111) * (1 `Bts.shift` 6)) +
         fromIntegral (oct3 .&. 0b00111111)
niOrdFromCU8' cu8@(Cd.CU8FourBytes oct1 oct2 oct3 oct4) =
        (fromIntegral (oct1 .&. 0b00000111) * (1 `Bts.shift` 18)) +
        (fromIntegral (oct2 .&. 0b00111111) * (1 `Bts.shift` 12)) +
        (fromIntegral (oct3 .&. 0b00111111) * (1 `Bts.shift` 6)) +
         fromIntegral (oct4 .&. 0b00111111)

niOrdCW1Char' :: Cd.CharWin1 -> Integer
niOrdCW1Char' (Cd.CharWin1 oct)
    | oct < 0x81 = fromIntegral oct
    | oct == 0x81 = -1
    | oct < 0x8D = fromIntegral oct - 1
    | oct == 0x8D = -1
    | oct < 0x8F = fromIntegral oct - 2
    | oct == 0x8F = -1
    | oct == 0x90 = -1
    | oct < 0x9D = fromIntegral oct - 4
    | oct == 0x9D = -1
    | otherwise = fromIntegral oct - 5

stbNormalise' :: TH.Stub -> TH.Stub
stbNormalise' stb@(TH.Stub nj)
    | nj >= 0 && nj <= 49 = TH.Stub nj
    | nj >= 50 && nj <= 99 = TH.Stub (nj - 50)
    | otherwise = TH.Stub 13

chNormalise' :: Char -> Char
chNormalise' ch = ch

cu8Normalise' :: Cd.CharUtf8 -> Cd.CharUtf8
cu8Normalise' cu8
    | isValidCharUtf8' cu8 = (cu8FromChar' . chFromCharUtf8') cu8
    | otherwise            = Cd.cdReplacement

cu8FromChar' :: Char -> Cd.CharUtf8
cu8FromChar' ch = cu8FromInt' nj
    where
        nj = Chr.ord ch

cu8FromInt' :: Int -> Cd.CharUtf8
cu8FromInt' nj
    | nj <= 0x7F =
        Cd.CU8OneByte
            oct1Of1
    | nj <= 0x7FF =
        Cd.CU8TwoBytes
            oct1Of2
            oct2Of2
    | nj <= 0xFFFF =
        Cd.CU8ThreeBytes
            oct1Of3
            oct2Of3
            oct3Of3
    | nj <= 0x10FFFF =
        Cd.CU8FourBytes
            oct1Of4
            oct2Of4
            oct3Of4
            oct4Of4
    | otherwise = Cd.cdReplacement
    where
        oct1Of1 = fromIntegral nj1' .&. 0b01111111
        oct1Of2 = (fromIntegral nj2' .&. 0b00011111) .|. 0b11000000
        oct2Of2 = (fromIntegral nj1' .&. 0b00111111) .|. 0b10000000
        oct1Of3 = (fromIntegral nj3' .&. 0b00001111) .|. 0b11100000
        oct2Of3 = (fromIntegral nj2' .&. 0b00111111) .|. 0b10000000
        oct3Of3 = (fromIntegral nj1' .&. 0b00111111) .|. 0b10000000
        oct1Of4 = (fromIntegral nj4' .&. 0b00000111) .|. 0b11110000
        oct2Of4 = (fromIntegral nj3' .&. 0b00111111) .|. 0b10000000
        oct3Of4 = (fromIntegral nj2' .&. 0b00111111) .|. 0b10000000
        oct4Of4 = (fromIntegral nj1' .&. 0b00111111) .|. 0b10000000
        nj1' :: Int
        nj1' = nj
        nj2' :: Int
        nj2' = nj1' `Bts.shift` (-6)
        nj3' :: Int
        nj3' = nj2' `Bts.shift` (-6)
        nj4' :: Int
        nj4' = nj3' `Bts.shift` (-6)

ci1Normalise' :: Cd.CharIso1 -> Cd.CharIso1
ci1Normalise' (Cd.CharIso1 nj)
    | nj >= 0x80 && nj <= 0x9F = Cd.cdReplacement
    | otherwise                = Cd.CharIso1 nj

cw1Normalise' :: Cd.CharWin1 -> Cd.CharWin1
cw1Normalise' (Cd.CharWin1 nj)
    | nj `elem` [0x81, 0x8D, 0x8F, 0x90, 0x9D] = Cd.cdReplacement
    | otherwise = Cd.CharWin1 nj

chFromStub' :: TH.Stub -> Char
chFromStub' (TH.Stub nj)
    | nj >= 0 && nj <= 24 = Chr.chr (fromIntegral nj + Chr.ord '0')
    | nj == 25 = '\0'
    | nj >= 26 && nj <= 49 = Chr.chr (fromIntegral nj + Chr.ord 'a' - 26)
    | nj >= 50 && nj <= 74 = Chr.chr (fromIntegral nj + Chr.ord '0' - 50)
    | nj == 75 = '\0'
    | nj >= 76 && nj <= 99 = Chr.chr (fromIntegral nj + Chr.ord 'a' - 76)
    | otherwise = '?'

chFromCharUtf8' :: Cd.CharUtf8 -> Char
chFromCharUtf8' cu8
    | nj >= 0 && nj <= 0x10FFFF = Chr.chr nj
    | otherwise = '\xFFFD'
    where
        nj = nj' cu8
        nj' :: Cd.CharUtf8 -> Int
        nj' (Cd.CU8OneByte oct1)
            | oct1 >= 0 && oct1 <= 0x7F = fromIntegral oct1
            | otherwise = -1
        nj' (Cd.CU8TwoBytes oct1 oct2)
            |
                (oct1 .&. 0b11100000 == 0b11000000) &&
                (oct2 .&. 0b11000000 == 0b10000000) =
                    fromIntegral (oct1 .&. 0b00011111) `Bts.shift` 6 +
                    fromIntegral (oct2 .&. 0b00111111)
            | otherwise = -1
        nj' (Cd.CU8ThreeBytes oct1 oct2 oct3)
            |
                (oct1 .&. 0b11110000 == 0b11100000) &&
                (oct2 .&. 0b11000000 == 0b10000000) &&
                (oct3 .&. 0b11000000 == 0b10000000) =
                    fromIntegral (oct1 .&. 0b00001111) `Bts.shift` 12 +
                    fromIntegral (oct2 .&. 0b00111111) `Bts.shift` 6 +
                    fromIntegral (oct3 .&. 0b00111111)
            | otherwise = -1
        nj' (Cd.CU8FourBytes oct1 oct2 oct3 oct4)
            |
                (oct1 .&. 0b11111000 == 0b11110000) &&
                (oct2 .&. 0b11000000 == 0b10000000) &&
                (oct3 .&. 0b11000000 == 0b10000000) &&
                (oct4 .&. 0b11000000 == 0b10000000) =
                    fromIntegral (oct1 .&. 0b00001111) `Bts.shift` 18 +
                    fromIntegral (oct2 .&. 0b00111111) `Bts.shift` 12 +
                    fromIntegral (oct3 .&. 0b00111111) `Bts.shift` 6 +
                    fromIntegral (oct4 .&. 0b00111111)
            | otherwise = -1

chFromCharIso1' :: Cd.CharIso1 -> Char
chFromCharIso1' (Cd.CharIso1 oct)
    | oct >= 0x00 && oct <= 0x7F = Chr.chr (fromIntegral oct)
    | oct >= 0xA0 && oct <= 0xFF = Chr.chr (fromIntegral oct)
    | otherwise = '\xFFFD'

chFromCharWin1' :: Cd.CharWin1 -> Char
chFromCharWin1' (Cd.CharWin1 oct)
    | oct >= 0x00 && oct <= 0x7F = Chr.chr (fromIntegral oct)
    | oct == 0x80  = '€'
    | oct == 0x82  = '‚'
    | oct == 0x83  = 'ƒ'
    | oct == 0x84  = '„'
    | oct == 0x85  = '…'
    | oct == 0x86  = '†'
    | oct == 0x87  = '‡'
    | oct == 0x88  = 'ˆ'
    | oct == 0x89  = '‰'
    | oct == 0x8A  = 'Š'
    | oct == 0x8B  = '‹'
    | oct == 0x8C  = 'Œ'
    | oct == 0x8E  = 'Ž'
    | oct == 0x91  = '‘'
    | oct == 0x92  = '’'
    | oct == 0x93  = '“'
    | oct == 0x94  = '”'
    | oct == 0x95  = '•'
    | oct == 0x96  = '–'
    | oct == 0x97  = '—'
    | oct == 0x98  = '˜'
    | oct == 0x99  = '™'
    | oct == 0x9A  = 'š'
    | oct == 0x9B  = '›'
    | oct == 0x9C  = 'œ'
    | oct == 0x9E  = 'ž'
    | oct == 0x9F  = 'Ÿ'
    | oct >= 0xA0 && oct <= 0xFF = Chr.chr (fromIntegral oct)
    | otherwise = '\xFFFD'

chFromOctet' :: Oct.Octet -> Char
chFromOctet' oct = Chr.chr (fromIntegral oct)

stbFromOct' :: Oct.Octet -> TH.Stub
stbFromOct' oct
    | (oct >= 0x30) && (oct <= 0x48) = TH.Stub (fromIntegral oct - 0x30)
    | (oct >= 0x61) && (oct <= 0x78) = TH.Stub (fromIntegral oct - 0x47)
    | otherwise                      = TH.Stub 13

stbFromChar' :: Char -> TH.Stub
stbFromChar' ch 
    | njOrd >= 0 && njOrd <= 0xFF = stbFromOct' (fromIntegral njOrd)
    | otherwise = TH.Stub 13
    where
        njOrd = Chr.ord ch

ci1FromChar' :: Char -> Cd.CharIso1
ci1FromChar' ch 
    | njOrd >= 0x00 && njOrd <= 0x7F = Cd.CharIso1 (fromIntegral njOrd)
    | njOrd >= 0xA0 && njOrd <= 0xFF = Cd.CharIso1 (fromIntegral njOrd)
    | otherwise                      = Cd.cdReplacement
    where
        njOrd = Chr.ord ch

cw1FromChar' :: Char -> Cd.CharWin1
cw1FromChar' ch 
    | njOrd >= 0x00 && njOrd <= 0x7F = Cd.CharWin1 (fromIntegral njOrd)
    | ch == '€' = Cd.CharWin1 0x80
    | ch == '‚' = Cd.CharWin1 0x82
    | ch == 'ƒ' = Cd.CharWin1 0x83
    | ch == '„' = Cd.CharWin1 0x84
    | ch == '…' = Cd.CharWin1 0x85
    | ch == '†' = Cd.CharWin1 0x86
    | ch == '‡' = Cd.CharWin1 0x87
    | ch == 'ˆ' = Cd.CharWin1 0x88
    | ch == '‰' = Cd.CharWin1 0x89
    | ch == 'Š' = Cd.CharWin1 0x8A
    | ch == '‹' = Cd.CharWin1 0x8B
    | ch == 'Œ' = Cd.CharWin1 0x8C
    --                        0x8D invalid
    | ch == 'Ž' = Cd.CharWin1 0x8E
    --                        0x8F invalid
    --                        0x90 invalid
    | ch == '‘' = Cd.CharWin1 0x91
    | ch == '’' = Cd.CharWin1 0x92
    | ch == '“' = Cd.CharWin1 0x93
    | ch == '”' = Cd.CharWin1 0x94
    | ch == '•' = Cd.CharWin1 0x95
    | ch == '–' = Cd.CharWin1 0x96
    | ch == '—' = Cd.CharWin1 0x97
    | ch == '˜' = Cd.CharWin1 0x98
    | ch == '™' = Cd.CharWin1 0x99
    | ch == 'š' = Cd.CharWin1 0x9A
    | ch == '›' = Cd.CharWin1 0x9B
    | ch == 'œ' = Cd.CharWin1 0x9C
    --                        0x9D invalid
    | ch == 'ž' = Cd.CharWin1 0x9E
    | ch == 'Ÿ' = Cd.CharWin1 0x9F
    | njOrd >= 0xA0 && njOrd <= 0xFF = Cd.CharWin1 (fromIntegral njOrd)
    | otherwise                      = Cd.cdReplacement
    where
        njOrd = Chr.ord ch

octFromChar' :: Char -> Oct.Octet
octFromChar' ch 
    | njOrd >= 0x00 && njOrd <= 0xFF = fromIntegral njOrd
    | otherwise                      = Cd.cdReplacement
    where
        njOrd = Chr.ord ch

isFromChar' :: Char -> Bool
isFromChar' '0' = False
isFromChar' '1' = True
isFromChar' _ = Cd.cdReplacement

--------------------------------------------------------------------------------
--  edge cases

lstbEdgeCases :: [TH.Stub]
lstbEdgeCases = 
    lstbNormalisedEdgeCases ++
    lstbOtherValidEdgeCases ++
    lstbInvalidEdgeCases

lstbNormalisedEdgeCases :: [TH.Stub]
lstbNormalisedEdgeCases = 
    [
        TH.Stub 0, TH.Stub 1, TH.Stub 2,
        TH.Stub 13, -- replacement
        TH.Stub 25, -- NUL
        TH.Stub 47, TH.Stub 48, TH.Stub 49
    ]

lstbOtherValidEdgeCases :: [TH.Stub]
lstbOtherValidEdgeCases = 
    [
        TH.Stub 50, TH.Stub 51, TH.Stub 52,
        TH.Stub 75, -- NUL
        TH.Stub 97, TH.Stub 98, TH.Stub 99
    ]

lstbInvalidEdgeCases :: [TH.Stub]
lstbInvalidEdgeCases = 
    [
        TH.Stub (-3), TH.Stub (-2), TH.Stub (-1),
        TH.Stub 100, TH.Stub 101, TH.Stub 102
    ]

lchEdgeCases :: [Char]
lchEdgeCases =
    [
        '\x00', -- NUL
        '\x01', '\x02', 
        '\x1D', '\x1E', '\x1F', 
        ' ', '!', '"',
        '-', '.', '/',
        '0', '1', '2',
        '7', '8', '9',
        ':', ';', '<',
        '>', '?', '@',
        'A', 'B', 'C',
        'X', 'Y', 'Z',
        '[', '\\', ']',
        '^', '_', '`',
        'a', 'b', 'c',
        'x', 'y', 'z',
        '{', '|', '}', '~',
        '\x80', '\x81', '\x82',
        '\x83', '\x84',
        '\x8A', '\x8B', '\x8C',
        '\x8E',
        '\x91', '\x92', '\x93',
        '\x9A', '\x9B', '\x9C',
        '\x9E', '\x9F',
        '\xA0', '\xA1', '\xA2',
        '\xA3',
        '\xFD', '\xFE', '\xFF', 
        '\x100', '\x101', '\x102', 
        '\x7FD', '\x7FE', '\x7FF',
        '\x800', '\x801', '\x802',
        '\x20AC', '\x201A', '\x0192', '\x201E', '\x2026', '\x2020', '\x2021', '\x02C6', 
        '\x2030', '\x0160', '\x2039', '\x0152', '\x017D', '\x2018', '\x2019', '\x201C', 
        '\x201D', '\x2022', '\x2013', '\x2014', '\x02DC', '\x2122', '\x0161', '\x203A',
        '\x0153', '\x017E', '\x0178',
        '\xFFFD', -- replacement
        '\xFFFE', '\xFFFF',
        '\x10000', '\x10001', '\x10002',
        '\x1FFFD', '\x1FFFE', '\x1FFFF',
        '\x10FFFD', '\x10FFFE', '\x10FFFF'
    ]

lcu8EdgeCases :: [Cd.CharUtf8]
lcu8EdgeCases =
    lcu8NormalisedEdgeCases ++
    lcu8OtherValidEdgeCases ++
    lcu8InvalidEdgeCases

lcu8NormalisedEdgeCases :: [Cd.CharUtf8]
lcu8NormalisedEdgeCases =
    [
        Cd.CU8OneByte 0b00000000, -- NUL
        Cd.CU8OneByte 0b00000001,
        Cd.CU8OneByte 0b00000010,
        Cd.CU8OneByte 0b01111101,
        Cd.CU8OneByte 0b01111110,
        Cd.CU8OneByte 0b01111111,
        Cd.CU8TwoBytes 0b11011111 0b10111101,
        Cd.CU8TwoBytes 0b11011111 0b10111110,
        Cd.CU8TwoBytes 0b11011111 0b10111111,
        Cd.CU8ThreeBytes 0b11101111 0b10111111 0b10111101, -- replacement
        Cd.CU8ThreeBytes 0b11101111 0b10111111 0b10111110,
        Cd.CU8ThreeBytes 0b11101111 0b10111111 0b10111111,
        Cd.CU8FourBytes 0b11110111 0b10111111 0b10111111 0b10111101,
        Cd.CU8FourBytes 0b11110111 0b10111111 0b10111111 0b10111110,
        Cd.CU8FourBytes 0b11110111 0b10111111 0b10111111 0b10111111
    ]
lcu8OtherValidEdgeCases :: [Cd.CharUtf8]
lcu8OtherValidEdgeCases =
    [
        Cd.CU8TwoBytes 0b11000000 0b10000000, -- NUL
        Cd.CU8TwoBytes 0b11000000 0b10000001,
        Cd.CU8TwoBytes 0b11000000 0b10000010,
        Cd.CU8ThreeBytes 0b11100000 0b10000000 0b10000000, -- NUL
        Cd.CU8ThreeBytes 0b11100000 0b10000000 0b10000001,
        Cd.CU8ThreeBytes 0b11100000 0b10000000 0b10000010,
        Cd.CU8ThreeBytes 0b11100000 0b10000000 0b10111101,
        Cd.CU8ThreeBytes 0b11100000 0b10000000 0b10111110,
        Cd.CU8ThreeBytes 0b11100000 0b10000000 0b10111111,
        Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b10000000, -- NUL
        Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b10000001,
        Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b10000010,
        Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b10111101,
        Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b10111110,
        Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b10111111
    ]

lcu8InvalidEdgeCases :: [Cd.CharUtf8]
lcu8InvalidEdgeCases =
    [
        Cd.CU8OneByte 0b10000000,
        Cd.CU8OneByte 0b10000001,
        Cd.CU8OneByte 0b10000010,
        Cd.CU8OneByte 0b11111101,
        Cd.CU8OneByte 0b11111110,
        Cd.CU8OneByte 0b11111111,
        Cd.CU8TwoBytes 0b00000000 0b00000000,
        Cd.CU8TwoBytes 0b00000000 0b00000001,
        Cd.CU8TwoBytes 0b00000000 0b00000010,
        Cd.CU8TwoBytes 0b11000000 0b01111101,
        Cd.CU8TwoBytes 0b11000000 0b01111110,
        Cd.CU8TwoBytes 0b11000000 0b01111111,
        Cd.CU8TwoBytes 0b11011111 0b11000000,
        Cd.CU8TwoBytes 0b11011111 0b11000001,
        Cd.CU8TwoBytes 0b11011111 0b11000010,
        Cd.CU8TwoBytes 0b11111111 0b11111101,
        Cd.CU8TwoBytes 0b11111111 0b11111110,
        Cd.CU8TwoBytes 0b11111111 0b11111111,
        Cd.CU8TwoBytes 0b11011111 0b11111111,
        Cd.CU8TwoBytes 0b11011111 0b00111111,
        Cd.CU8TwoBytes 0b11111111 0b10111111,
        Cd.CU8TwoBytes 0b10011111 0b10111111,
        Cd.CU8TwoBytes 0b01011111 0b10111111,
        Cd.CU8ThreeBytes 0b00000000 0b00000000 0b00000000,
        Cd.CU8ThreeBytes 0b00000000 0b00000000 0b00000001,
        Cd.CU8ThreeBytes 0b00000000 0b00000000 0b00000010,
        Cd.CU8ThreeBytes 0b11100000 0b10000000 0b01111101,
        Cd.CU8ThreeBytes 0b11100000 0b10000000 0b01111110,
        Cd.CU8ThreeBytes 0b11100000 0b10000000 0b01111111,
        Cd.CU8ThreeBytes 0b11100000 0b10000000 0b11000000,
        Cd.CU8ThreeBytes 0b11100000 0b10000000 0b11000001,
        Cd.CU8ThreeBytes 0b11100000 0b10000000 0b11000010,
        Cd.CU8ThreeBytes 0b11101111 0b10111111 0b11000000,
        Cd.CU8ThreeBytes 0b11101111 0b10111111 0b11000001,
        Cd.CU8ThreeBytes 0b11101111 0b10111111 0b11000010,
        Cd.CU8ThreeBytes 0b11111111 0b11111111 0b11111101,
        Cd.CU8ThreeBytes 0b11111111 0b11111111 0b11111110,
        Cd.CU8ThreeBytes 0b11111111 0b11111111 0b11111111,
        Cd.CU8ThreeBytes 0b11101111 0b10111111 0b11111111,
        Cd.CU8ThreeBytes 0b11101111 0b10111111 0b00111111,
        Cd.CU8ThreeBytes 0b11101111 0b11111111 0b10111111,
        Cd.CU8ThreeBytes 0b11101111 0b00111111 0b10111111,
        Cd.CU8ThreeBytes 0b11111111 0b10111111 0b10111111,
        Cd.CU8ThreeBytes 0b11001111 0b10111111 0b10111111,
        Cd.CU8ThreeBytes 0b10101111 0b10111111 0b10111111,
        Cd.CU8ThreeBytes 0b01101111 0b10111111 0b10111111,
        Cd.CU8FourBytes 0b00000000 0b00000000 0b00000000 0b00000000,
        Cd.CU8FourBytes 0b00000000 0b00000000 0b00000000 0b00000001,
        Cd.CU8FourBytes 0b00000000 0b00000000 0b00000000 0b00000010,
        Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b01111101,
        Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b01111110,
        Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b01111111,
        Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b11000000,
        Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b11000001,
        Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b11000010,
        Cd.CU8FourBytes 0b11111000 0b10000000 0b10000000 0b10000000,
        Cd.CU8FourBytes 0b11111000 0b10000000 0b10000000 0b10000001,
        Cd.CU8FourBytes 0b11111000 0b10000000 0b10000000 0b10000010,
        Cd.CU8FourBytes 0b11111111 0b11111111 0b11111111 0b11111101,
        Cd.CU8FourBytes 0b11111111 0b11111111 0b11111111 0b11111110,
        Cd.CU8FourBytes 0b11111111 0b11111111 0b11111111 0b11111111,
        Cd.CU8FourBytes 0b11110111 0b10111111 0b10111111 0b11111111,
        Cd.CU8FourBytes 0b11110111 0b10111111 0b10111111 0b00111111,
        Cd.CU8FourBytes 0b11110111 0b10111111 0b11111111 0b10111111,
        Cd.CU8FourBytes 0b11110111 0b10111111 0b00111111 0b10111111,
        Cd.CU8FourBytes 0b11110111 0b11111111 0b10111111 0b10111111,
        Cd.CU8FourBytes 0b11110111 0b00111111 0b10111111 0b10111111,
        Cd.CU8FourBytes 0b11111111 0b10111111 0b10111111 0b10111111,
        Cd.CU8FourBytes 0b11100111 0b10111111 0b10111111 0b10111111,
        Cd.CU8FourBytes 0b11010111 0b10111111 0b10111111 0b10111111,
        Cd.CU8FourBytes 0b10110111 0b10111111 0b10111111 0b10111111,
        Cd.CU8FourBytes 0b01110111 0b10111111 0b10111111 0b10111111
    ]

lci1EdgeCases :: [Cd.CharIso1]
lci1EdgeCases =
    lci1NormalisedEdgeCases ++
    lci1InvalidEdgeCases

lci1NormalisedEdgeCases :: [Cd.CharIso1]
lci1NormalisedEdgeCases = 
    [
        Cd.CharIso1 0x00, Cd.CharIso1 0x01, Cd.CharIso1 0x02, 
        Cd.CharIso1 0x1D, Cd.CharIso1 0x1E, Cd.CharIso1 0x1F, 
        Cd.CharIso1 0x20, Cd.CharIso1 0x21, Cd.CharIso1 0x22, 
        Cd.CharIso1 0x2D, Cd.CharIso1 0x2E, Cd.CharIso1 0x2F, 
        Cd.CharIso1 0x30, Cd.CharIso1 0x31, Cd.CharIso1 0x32, 
        Cd.CharIso1 0x37, Cd.CharIso1 0x38, Cd.CharIso1 0x39, 
        Cd.CharIso1 0x3F, Cd.CharIso1 0x40,
        Cd.CharIso1 0x41, Cd.CharIso1 0x42, Cd.CharIso1 0x43, 
        Cd.CharIso1 0x58, Cd.CharIso1 0x59, Cd.CharIso1 0x5A, 
        Cd.CharIso1 0x61, Cd.CharIso1 0x62, Cd.CharIso1 0x63, 
        Cd.CharIso1 0x78, Cd.CharIso1 0x79, Cd.CharIso1 0x7A, 
        Cd.CharIso1 0x7D, Cd.CharIso1 0x7E, Cd.CharIso1 0x7F, 
        Cd.CharIso1 0x7D, Cd.CharIso1 0x7E, Cd.CharIso1 0x7F, 
        Cd.CharIso1 0xA0, Cd.CharIso1 0xA1, Cd.CharIso1 0xA2, 
        Cd.CharIso1 0xFD, Cd.CharIso1 0xFE, Cd.CharIso1 0xFF
    ]

lci1InvalidEdgeCases :: [Cd.CharIso1]
lci1InvalidEdgeCases = 
    [
        Cd.CharIso1 0x80, Cd.CharIso1 0x81, Cd.CharIso1 0x82, 
        Cd.CharIso1 0x9D, Cd.CharIso1 0x9E, Cd.CharIso1 0x9F
    ]

lcw1EdgeCases :: [Cd.CharWin1]
lcw1EdgeCases =
    lcw1NormalisedEdgeCases ++
    lcw1InvalidEdgeCases

lcw1NormalisedEdgeCases :: [Cd.CharWin1]
lcw1NormalisedEdgeCases = 
    [
        Cd.CharWin1 0x00, Cd.CharWin1 0x01, Cd.CharWin1 0x02,
        Cd.CharWin1 0x1D, Cd.CharWin1 0x1E, Cd.CharWin1 0x1F,
        Cd.CharWin1 0x20, Cd.CharWin1 0x21, Cd.CharWin1 0x22,
        Cd.CharWin1 0x2D, Cd.CharWin1 0x2E, Cd.CharWin1 0x2F,
        Cd.CharWin1 0x30, Cd.CharWin1 0x31, Cd.CharWin1 0x32,
        Cd.CharWin1 0x37, Cd.CharWin1 0x38, Cd.CharWin1 0x39,
        Cd.CharWin1 0x3F, Cd.CharWin1 0x40,
        Cd.CharWin1 0x41, Cd.CharWin1 0x42, Cd.CharWin1 0x43,
        Cd.CharWin1 0x58, Cd.CharWin1 0x59, Cd.CharWin1 0x5A,
        Cd.CharWin1 0x61, Cd.CharWin1 0x62, Cd.CharWin1 0x63,
        Cd.CharWin1 0x78, Cd.CharWin1 0x79, Cd.CharWin1 0x7A,
        Cd.CharWin1 0x7D, Cd.CharWin1 0x7E, Cd.CharWin1 0x7F,
        Cd.CharWin1 0x80,
        Cd.CharWin1 0x82, Cd.CharWin1 0x83,  Cd.CharWin1 0x84,
        Cd.CharWin1 0x8A, Cd.CharWin1 0x8B,  Cd.CharWin1 0x8C,
        Cd.CharWin1 0x8E, 
        Cd.CharWin1 0x91, Cd.CharWin1 0x92,  Cd.CharWin1 0x93,
        Cd.CharWin1 0x9A, Cd.CharWin1 0x9B,  Cd.CharWin1 0x9C,
        Cd.CharWin1 0x9E, Cd.CharWin1 0x9F,  Cd.CharWin1 0xA0,
        Cd.CharWin1 0xA1, Cd.CharWin1 0xA2,  Cd.CharWin1 0xA3,
        Cd.CharWin1 0xFD, Cd.CharWin1 0xFE, Cd.CharWin1 0xFF
    ]

lcw1InvalidEdgeCases :: [Cd.CharWin1]
lcw1InvalidEdgeCases = 
    [
        Cd.CharWin1 0x81,
        Cd.CharWin1 0x8D,
        Cd.CharWin1 0x8F,
        Cd.CharWin1 0x90,
        Cd.CharWin1 0x9D
    ]

loctEdgeCases :: [Oct.Octet]
loctEdgeCases =
    [
        0x00, -- NUL
        0x01, 0x02,
        0x7D, 0x7E, 0x7F,
        0x80, 0x81, 0x82,
        0xFD, 0xFE, 
        0xFF -- replacement
    ]

lisEdgeCases :: [Bool]
lisEdgeCases =
    [
        False, 
        True
    ]

lnjEdgeCasesForStub :: [Int]
lnjEdgeCasesForStub = fmap TH.rniStb lstbEdgeCases

lnjEdgeCasesForChar :: [Int]
lnjEdgeCasesForChar = 
    fmap Chr.ord lchEdgeCases ++ 
    [
        -3,
        -2,
        -1,
        0x110000,
        0x110001,
        0x110002
    ]

lnjEdgeCasesForOctets :: [Int]
lnjEdgeCasesForOctets = 
    [
        -3,
        -2,
        -1,
        0x00,
        0x01, 0x02,
        0x1D, 0x1E, 0x1F, 
        0x20, 0x21, 0x22, 
        0x2D, 0x2E, 0x2F, 
        0x30, 0x31, 0x32, 
        0x37, 0x38, 0x39, 
        0x3F, 0x40,
        0x41, 0x42, 0x43, 
        0x58, 0x59, 0x5A, 
        0x61, 0x62, 0x63, 
        0x78, 0x79, 0x7A, 
        0x7D, 0x7E, 0x7F,
        0x80, 0x81, 0x82,
        0x83, 0x84,
        0x8A, 0x8B, 0x8C,
        0x8E,
        0x91, 0x92, 0x93,
        0x9A, 0x9B, 0x9C,
        0x9E, 0x9F,
        0xA0, 0xA1, 0xA2,
        0xA3,
        0xFD, 0xFE,
        0xFF, 
        0x100, 0x101, 0x102
    ]
