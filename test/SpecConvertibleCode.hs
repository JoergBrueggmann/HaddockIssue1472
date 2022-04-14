{-|
Description : test.
Copyright   : (c) Jörg K.-H. W. Brüggmann, 2021-2022
License     : proprietary, to be dual licensed
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE BinaryLiterals #-}

module SpecConvertibleCode
    (
        testGroup
    ) where

import qualified Test.Tasty as T
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit as HU

import qualified Data.Char as Chr

import qualified TestCaseUnwrap as TCU
import qualified SpecCodeTestHelper as TH

import qualified ConvertibleCode as Cd
import qualified Octets as Oct

--------------------------------------------------------------------------------
-- test group hirachy

{-  * module ConvertibleCode - validated ❓
        * justification:
            * completeness: ❌ - not yet (all functions including types, type classes are tested)
            * conform doc.: ✅ (all tests are conform to documentation, e.g. source code comments by haddock) -}
testGroup :: T.TestTree
testGroup =
    T.testGroup
        "module ConvertibleCode"
        [
            tgClassConvertibleCode
        ]

{-  * class ConvertibleCode - validated ❓
        * justification:
            * completeness: ❌ - not yet (all functions are tested)
            * stubbed     : N/A (the default functions of the class are also tested by stub instance(s), if any)
            * conform doc.: ✅ (all tests are conform to documentation, e.g. source code comments by haddock) -}
tgClassConvertibleCode :: T.TestTree
tgClassConvertibleCode =
    T.testGroup
        "class ConvertibleCode"
        [
            tgUnitcdConvert,
            tgUnitlcdConvert
        ]

{-  * convert - validated ❓
        * justification:
            * completeness: ❌ - not yet (the function is tested for all types and type combinations)
            * conform doc.: ✅ (all tests are conform to documentation, e.g. source code comments by haddock) -}
tgUnitcdConvert :: T.TestTree
tgUnitcdConvert =
    T.testGroup
        "cdConvert"
        [
        {-  * Char to Char - validated ✅
                * justification:
                    * completeness: ✅ (the complete set of possible test values/data is used in unit tests, or values for all possible equivalence classes are used or ramdom sample values/data from complete set of possible values/data is used in case of high-volume cases)
                    * independence: ✅ (test method is independent from library functions that are under test, or dependent on tested functions, or dependent on tested functions)
                    * boundaries  : ✅ (corner cases and boundaries are tested by unit tests)
                    * conform doc.: ✅ (all tests are conform to documentation, e.g. source code comments by haddock) -}
            QC.testProperty "Char to Char #1" (\cd -> True QC.==> Cd.cdConvert (cd :: Char) == cd),
            HU.testCase     "Char to Char #2" (Cd.cdConvert '\x00' HU.@?= '\x00'),
            HU.testCase     "Char to Char #3" (Cd.cdConvert '\x01' HU.@?= '\x01'),
            HU.testCase     "Char to Char #4" (Cd.cdConvert '\x10FFFE' HU.@?= '\x10FFFE'),
            HU.testCase     "Char to Char #5" (Cd.cdConvert '\x10FFFF' HU.@?= '\x10FFFF'),
        {-  * Char to CharUtf8 - validated ✅
                *justification:
                    * completeness: ✅ (the complete set of possible test values/data is used in unit tests, or values for all possible equivalence classes are used or ramdom sample values/data from complete set of possible values/data is used in case of high-volume cases)
                    * independence: ❌ (test method is independent from library functions that are under test, or dependent on tested functions)
                        NOTE: correct functioning depends on "Cd.cdFromChar :: Char -> CharUtf8" test
                        mitigation: ✅ supplemental test data (approx. 100 characters) are use in unit tests
                    * boundaries  : ✅ (corner cases and boundaries are tested by unit tests)
                    * conform doc.: ✅ (all tests are conform to documentation, e.g. source code comments by haddock) -}
            QC.testProperty "Char to CharUtf8 #1" (\cd -> True QC.==> (Cd.cdConvert (cd :: Char) :: Cd.CharUtf8) == Cd.cdFromChar cd),
            HU.testCase     "Char to CharUtf8 #2" (Cd.cdConvert '\x00' HU.@?= Cd.CU8OneByte 0x00),
            HU.testCase     "Char to CharUtf8 #3" (Cd.cdConvert '\x01' HU.@?= Cd.CU8OneByte 0x01),
            HU.testCase     "Char to CharUtf8 #4" (Cd.cdConvert ' ' HU.@?= Cd.CU8OneByte 0x20),
            HU.testCase     "Char to CharUtf8 #5" (Cd.cdConvert '!' HU.@?= Cd.CU8OneByte 0x21),
            HU.testCase     "Char to CharUtf8 #6" (Cd.cdConvert '/' HU.@?= Cd.CU8OneByte 0x2F),
            HU.testCase     "Char to CharUtf8 #7" (Cd.cdConvert '0' HU.@?= Cd.CU8OneByte 0x30),
            HU.testCase     "Char to CharUtf8 #8" (Cd.cdConvert '1' HU.@?= Cd.CU8OneByte 0x31),
            HU.testCase     "Char to CharUtf8 #9" (Cd.cdConvert '8' HU.@?= Cd.CU8OneByte 0x38),
            HU.testCase    "Char to CharUtf8 #10" (Cd.cdConvert '9' HU.@?= Cd.CU8OneByte 0x39),
            HU.testCase    "Char to CharUtf8 #11" (Cd.cdConvert '?' HU.@?= Cd.CU8OneByte 0x3F),
            HU.testCase    "Char to CharUtf8 #12" (Cd.cdConvert '@' HU.@?= Cd.CU8OneByte 0x40),
            HU.testCase    "Char to CharUtf8 #13" (Cd.cdConvert 'A' HU.@?= Cd.CU8OneByte 0x41),
            HU.testCase    "Char to CharUtf8 #14" (Cd.cdConvert 'B' HU.@?= Cd.CU8OneByte 0x42),
            HU.testCase    "Char to CharUtf8 #15" (Cd.cdConvert 'Y' HU.@?= Cd.CU8OneByte 0x59),
            HU.testCase    "Char to CharUtf8 #16" (Cd.cdConvert 'Z' HU.@?= Cd.CU8OneByte 0x5A),
            HU.testCase    "Char to CharUtf8 #17" (Cd.cdConvert '[' HU.@?= Cd.CU8OneByte 0x5B),
            HU.testCase    "Char to CharUtf8 #18" (Cd.cdConvert '\\' HU.@?= Cd.CU8OneByte 0x5C),
            HU.testCase    "Char to CharUtf8 #19" (Cd.cdConvert ']' HU.@?= Cd.CU8OneByte 0x5D),
            HU.testCase    "Char to CharUtf8 #20" (Cd.cdConvert '^' HU.@?= Cd.CU8OneByte 0x5E),
            HU.testCase    "Char to CharUtf8 #21" (Cd.cdConvert '_' HU.@?= Cd.CU8OneByte 0x5F),
            HU.testCase    "Char to CharUtf8 #22" (Cd.cdConvert '`' HU.@?= Cd.CU8OneByte 0x60),
            HU.testCase    "Char to CharUtf8 #23" (Cd.cdConvert 'a' HU.@?= Cd.CU8OneByte 0x61),
            HU.testCase    "Char to CharUtf8 #24" (Cd.cdConvert 'b' HU.@?= Cd.CU8OneByte 0x62),
            HU.testCase    "Char to CharUtf8 #25" (Cd.cdConvert 'y' HU.@?= Cd.CU8OneByte 0x79),
            HU.testCase    "Char to CharUtf8 #26" (Cd.cdConvert 'z' HU.@?= Cd.CU8OneByte 0x7A),
            HU.testCase    "Char to CharUtf8 #27" (Cd.cdConvert '{' HU.@?= Cd.CU8OneByte 0x7B),
            HU.testCase    "Char to CharUtf8 #28" (Cd.cdConvert '|' HU.@?= Cd.CU8OneByte 0x7C),
            HU.testCase    "Char to CharUtf8 #29" (Cd.cdConvert '}' HU.@?= Cd.CU8OneByte 0x7D),
            HU.testCase    "Char to CharUtf8 #30" (Cd.cdConvert '~' HU.@?= Cd.CU8OneByte 0x7E),
            HU.testCase    "Char to CharUtf8 #31" (Cd.cdConvert '\x7E' HU.@?= Cd.CU8OneByte 0x7E),
            HU.testCase    "Char to CharUtf8 #32" (Cd.cdConvert '\x7F' HU.@?= Cd.CU8OneByte 0x7F),
            HU.testCase    "Char to CharUtf8 #33" (Cd.cdConvert '\x80' HU.@?= Cd.CU8TwoBytes 0b11000010 0b10000000),
            HU.testCase    "Char to CharUtf8 #34" (Cd.cdConvert '\x81' HU.@?= Cd.CU8TwoBytes 0b11000010 0b10000001),
            HU.testCase    "Char to CharUtf8 #35" (Cd.cdConvert '¡' HU.@?= Cd.CU8TwoBytes 0xC2 0xA1),
            HU.testCase    "Char to CharUtf8 #36" (Cd.cdConvert '¢' HU.@?= Cd.CU8TwoBytes 0xC2 0xA2),
            HU.testCase    "Char to CharUtf8 #37" (Cd.cdConvert '£' HU.@?= Cd.CU8TwoBytes 0xC2 0xA3),
            HU.testCase    "Char to CharUtf8 #38" (Cd.cdConvert '¤' HU.@?= Cd.CU8TwoBytes 0xC2 0xA4),
            HU.testCase    "Char to CharUtf8 #39" (Cd.cdConvert 'ü' HU.@?= Cd.CU8TwoBytes 0xC3 0xBC),
            HU.testCase    "Char to CharUtf8 #40" (Cd.cdConvert 'ý' HU.@?= Cd.CU8TwoBytes 0xC3 0xBD),
            HU.testCase    "Char to CharUtf8 #41" (Cd.cdConvert 'þ' HU.@?= Cd.CU8TwoBytes 0b11000011 0b10111110),
            HU.testCase    "Char to CharUtf8 #42" (Cd.cdConvert 'ÿ' HU.@?= Cd.CU8TwoBytes 0b11000011 0b10111111),
            HU.testCase    "Char to CharUtf8 #43" (Cd.cdConvert '\xFE' HU.@?= Cd.CU8TwoBytes 0b11000011 0b10111110),
            HU.testCase    "Char to CharUtf8 #44" (Cd.cdConvert '\xFF' HU.@?= Cd.CU8TwoBytes 0b11000011 0b10111111),
            HU.testCase    "Char to CharUtf8 #45" (Cd.cdConvert '\x100' HU.@?= Cd.CU8TwoBytes 0b11000100 0b10000000),
            HU.testCase    "Char to CharUtf8 #46" (Cd.cdConvert '\x101' HU.@?= Cd.CU8TwoBytes 0b11000100 0b10000001),
            HU.testCase    "Char to CharUtf8 #47" (Cd.cdConvert 'Ā' HU.@?= Cd.CU8TwoBytes 0b11000100 0b10000000),
            HU.testCase    "Char to CharUtf8 #48" (Cd.cdConvert 'ā' HU.@?= Cd.CU8TwoBytes 0b11000100 0b10000001),
            HU.testCase    "Char to CharUtf8 #49" (Cd.cdConvert 'Ă' HU.@?= Cd.CU8TwoBytes 0xC4 0x82),
            HU.testCase    "Char to CharUtf8 #50" (Cd.cdConvert 'ă' HU.@?= Cd.CU8TwoBytes 0xC4 0x83),
            HU.testCase    "Char to CharUtf8 #51" (Cd.cdConvert 'α' HU.@?= Cd.CU8TwoBytes 0xCE 0xB1),
            HU.testCase    "Char to CharUtf8 #52" (Cd.cdConvert 'β' HU.@?= Cd.CU8TwoBytes 0xCE 0xB2),
            HU.testCase    "Char to CharUtf8 #53" (Cd.cdConvert 'ψ' HU.@?= Cd.CU8TwoBytes 0xCF 0x88),
            HU.testCase    "Char to CharUtf8 #54" (Cd.cdConvert 'ω' HU.@?= Cd.CU8TwoBytes 0xCF 0x89),
            HU.testCase    "Char to CharUtf8 #55" (Cd.cdConvert 'ߺ' HU.@?= Cd.CU8TwoBytes 0xDF 0xBA),
            HU.testCase    "Char to CharUtf8 #56" (Cd.cdConvert '߽' HU.@?= Cd.CU8TwoBytes 0xDF 0xBD),
            HU.testCase    "Char to CharUtf8 #57" (Cd.cdConvert '߾' HU.@?= Cd.CU8TwoBytes 0xDF 0xBE),
            HU.testCase    "Char to CharUtf8 #58" (Cd.cdConvert '߿' HU.@?= Cd.CU8TwoBytes 0xDF 0xBF),
            HU.testCase    "Char to CharUtf8 #59" (Cd.cdConvert '\x7FE' HU.@?= Cd.CU8TwoBytes 0b11011111 0b10111110),
            HU.testCase    "Char to CharUtf8 #60" (Cd.cdConvert '\x7FF' HU.@?= Cd.CU8TwoBytes 0b11011111 0b10111111),
            HU.testCase    "Char to CharUtf8 #61" (Cd.cdConvert '\x800' HU.@?= Cd.CU8ThreeBytes 0b11100000 0b10100000 0b10000000),
            HU.testCase    "Char to CharUtf8 #62" (Cd.cdConvert '\x801' HU.@?= Cd.CU8ThreeBytes 0b11100000 0b10100000 0b10000001),
            HU.testCase    "Char to CharUtf8 #63" (Cd.cdConvert '\x800' HU.@?= Cd.CU8ThreeBytes 0b11100000 0b10100000 0b10000000),
            HU.testCase    "Char to CharUtf8 #64" (Cd.cdConvert 'ࢠ' HU.@?= Cd.CU8ThreeBytes 0xE0 0xA2 0xA0),
            HU.testCase    "Char to CharUtf8 #65" (Cd.cdConvert 'ࢡ' HU.@?= Cd.CU8ThreeBytes 0xE0 0xA2 0xA1),
            HU.testCase    "Char to CharUtf8 #66" (Cd.cdConvert 'ࢢ' HU.@?= Cd.CU8ThreeBytes 0xE0 0xA2 0xA2),
            HU.testCase    "Char to CharUtf8 #67" (Cd.cdConvert 'ࢣ' HU.@?= Cd.CU8ThreeBytes 0xE0 0xA2 0xA3),
            HU.testCase    "Char to CharUtf8 #68" (Cd.cdConvert '∀' HU.@?= Cd.CU8ThreeBytes 0xE2 0x88 0x80),
            HU.testCase    "Char to CharUtf8 #69" (Cd.cdConvert '∁' HU.@?= Cd.CU8ThreeBytes 0xE2 0x88 0x81),
            HU.testCase    "Char to CharUtf8 #70" (Cd.cdConvert '∂' HU.@?= Cd.CU8ThreeBytes 0xE2 0x88 0x82),
            HU.testCase    "Char to CharUtf8 #71" (Cd.cdConvert '∃' HU.@?= Cd.CU8ThreeBytes 0xE2 0x88 0x83),
            HU.testCase    "Char to CharUtf8 #72" (Cd.cdConvert 'ힰ' HU.@?= Cd.CU8ThreeBytes 0xED 0x9E 0xB0),
            HU.testCase    "Char to CharUtf8 #73" (Cd.cdConvert 'ힱ' HU.@?= Cd.CU8ThreeBytes 0xED 0x9E 0xB1),
            HU.testCase    "Char to CharUtf8 #74" (Cd.cdConvert 'ힲ' HU.@?= Cd.CU8ThreeBytes 0xED 0x9E 0xB2),
            HU.testCase    "Char to CharUtf8 #75" (Cd.cdConvert 'ힳ' HU.@?= Cd.CU8ThreeBytes 0xED 0x9E 0xB3),
            HU.testCase    "Char to CharUtf8 #76" (Cd.cdConvert 'ퟸ' HU.@?= Cd.CU8ThreeBytes 0xED 0x9F 0xB8),
            HU.testCase    "Char to CharUtf8 #77" (Cd.cdConvert 'ퟹ' HU.@?= Cd.CU8ThreeBytes 0xED 0x9F 0xB9),
            HU.testCase    "Char to CharUtf8 #78" (Cd.cdConvert 'ퟺ' HU.@?= Cd.CU8ThreeBytes 0xED 0x9F 0xBA),
            HU.testCase    "Char to CharUtf8 #79" (Cd.cdConvert 'ퟻ' HU.@?= Cd.CU8ThreeBytes 0xED 0x9F 0xBB),
            HU.testCase    "Char to CharUtf8 #80" (Cd.cdConvert '為' HU.@?= Cd.CU8ThreeBytes 0xE7 0x82 0xBA),
            HU.testCase    "Char to CharUtf8 #81" (Cd.cdConvert '炻' HU.@?= Cd.CU8ThreeBytes 0xE7 0x82 0xBB),
            HU.testCase    "Char to CharUtf8 #82" (Cd.cdConvert '炼' HU.@?= Cd.CU8ThreeBytes 0xE7 0x82 0xBC),
            HU.testCase    "Char to CharUtf8 #83" (Cd.cdConvert '炽' HU.@?= Cd.CU8ThreeBytes 0xE7 0x82 0xBD),
            HU.testCase    "Char to CharUtf8 #84" (Cd.cdConvert '無' HU.@?= Cd.CU8ThreeBytes 0xE7 0x84 0xA1),
            HU.testCase    "Char to CharUtf8 #85" (Cd.cdConvert '焢' HU.@?= Cd.CU8ThreeBytes 0xE7 0x84 0xA2),
            HU.testCase    "Char to CharUtf8 #86" (Cd.cdConvert '焣' HU.@?= Cd.CU8ThreeBytes 0xE7 0x84 0xA3),
            HU.testCase    "Char to CharUtf8 #87" (Cd.cdConvert '焤' HU.@?= Cd.CU8ThreeBytes 0xE7 0x84 0xA4),
            HU.testCase    "Char to CharUtf8 #88" (Cd.cdConvert '禪' HU.@?= Cd.CU8ThreeBytes 0xE7 0xA6 0xAA),
            HU.testCase    "Char to CharUtf8 #89" (Cd.cdConvert '禫' HU.@?= Cd.CU8ThreeBytes 0xE7 0xA6 0xAB),
            HU.testCase    "Char to CharUtf8 #90" (Cd.cdConvert '禬' HU.@?= Cd.CU8ThreeBytes 0xE7 0xA6 0xAC),
            HU.testCase    "Char to CharUtf8 #91" (Cd.cdConvert '禭' HU.@?= Cd.CU8ThreeBytes 0xE7 0xA6 0xAD),
            HU.testCase    "Char to CharUtf8 #92" (Cd.cdConvert 'בֿ' HU.@?= Cd.CU8ThreeBytes 0xEF 0xAD 0x8C),
            HU.testCase    "Char to CharUtf8 #93" (Cd.cdConvert 'ﬀ' HU.@?= Cd.CU8ThreeBytes 0xEF 0xAC 0x80),
            HU.testCase    "Char to CharUtf8 #94" (Cd.cdConvert 'ﬁ' HU.@?= Cd.CU8ThreeBytes 0xEF 0xAC 0x81),
            HU.testCase    "Char to CharUtf8 #95" (Cd.cdConvert 'ﬂ' HU.@?= Cd.CU8ThreeBytes 0xEF 0xAC 0x82),
            HU.testCase    "Char to CharUtf8 #96" (Cd.cdConvert 'ﬃ' HU.@?= Cd.CU8ThreeBytes 0xEF 0xAC 0x83),
            HU.testCase    "Char to CharUtf8 #97" (Cd.cdConvert 'כֿ' HU.@?= Cd.CU8ThreeBytes 0xEF 0xAD 0x8D),
            HU.testCase    "Char to CharUtf8 #98" (Cd.cdConvert 'פֿ' HU.@?= Cd.CU8ThreeBytes 0xEF 0xAD 0x8E),
            HU.testCase    "Char to CharUtf8 #99" (Cd.cdConvert 'ﭏ' HU.@?= Cd.CU8ThreeBytes 0xEF 0xAD 0x8F),
            HU.testCase   "Char to CharUtf8 #100" (Cd.cdConvert '�' HU.@?= Cd.CU8ThreeBytes 0b11101111 0b10111111 0b10111101),
            HU.testCase   "Char to CharUtf8 #101" (Cd.cdConvert '\xFFFE' HU.@?= Cd.CU8ThreeBytes 0b11101111 0b10111111 0b10111110),
            HU.testCase   "Char to CharUtf8 #102" (Cd.cdConvert '\xFFFF' HU.@?= Cd.CU8ThreeBytes 0b11101111 0b10111111 0b10111111),
            HU.testCase   "Char to CharUtf8 #103" (Cd.cdConvert '你' HU.@?= Cd.CU8FourBytes 0xF0 0xAF 0xA0 0x84),
            HU.testCase   "Char to CharUtf8 #104" (Cd.cdConvert '兔' HU.@?= Cd.CU8FourBytes 0xF0 0xAF 0xA0 0x8F),
            HU.testCase   "Char to CharUtf8 #105" (Cd.cdConvert '再' HU.@?= Cd.CU8FourBytes 0xF0 0xAF 0xA0 0x95),
            HU.testCase   "Char to CharUtf8 #106" (Cd.cdConvert '冤' HU.@?= Cd.CU8FourBytes 0xF0 0xAF 0xA0 0x98),
            HU.testCase   "Char to CharUtf8 #107" (Cd.cdConvert '𐠀' HU.@?= Cd.CU8FourBytes 0xF0 0x90 0xA0 0x80),
            HU.testCase   "Char to CharUtf8 #108" (Cd.cdConvert '𐠁' HU.@?= Cd.CU8FourBytes 0xF0 0x90 0xA0 0x81),
            HU.testCase   "Char to CharUtf8 #109" (Cd.cdConvert '𐠂' HU.@?= Cd.CU8FourBytes 0xF0 0x90 0xA0 0x82),
            HU.testCase   "Char to CharUtf8 #110" (Cd.cdConvert '𐠃' HU.@?= Cd.CU8FourBytes 0xF0 0x90 0xA0 0x83),
            HU.testCase   "Char to CharUtf8 #111" (Cd.cdConvert '\x10000' HU.@?= Cd.CU8FourBytes 0b11110000 0b10010000 0b10000000 0b10000000),
            HU.testCase   "Char to CharUtf8 #112" (Cd.cdConvert '\x10001' HU.@?= Cd.CU8FourBytes 0b11110000 0b10010000 0b10000000 0b10000001),
            HU.testCase   "Char to CharUtf8 #113" (Cd.cdConvert '\x10FFFF' HU.@?= Cd.CU8FourBytes 0b11110100 0b10001111 0b10111111 0b10111111),
        {-  * Char to CharIso1 - validated ✅
                * justification:
                * completeness: ✅ (the complete set of possible test values/data is used in unit tests, or values for all possible equivalence classes are used or ramdom sample values/data from complete set of possible values/data is used in case of high-volume cases)
                * independence: ❌ (test method is independent from library functions that are under test, or dependent on tested functions)
                    NOTE: correct functioning depends on "Cd.cdFromChar :: Char -> CharIso1" test
                    mitigation: ✅
                        - supplemental test data (more than 10 characters) are use in unit tests
                        - ramdom sample data from complete set of non-convertable characters are tested
                * boundaries  : ✅ (corner cases and boundaries are tested by unit tests)
                * conform doc.: ✅ (all tests are conform to documentation, e.g. source code comments by haddock) -}
            QC.testProperty "Char to CharIso1 #1" (\cd -> elem cd lchConvertableToCI1 QC.==> (Cd.cdConvert (cd :: Char) :: Cd.CharIso1) == Cd.cdFromChar cd),
            QC.testProperty "Char to CharIso1 #2" (\cd -> notElem cd lchConvertableToCI1 QC.==> (Cd.cdConvert (cd :: Char) :: Cd.CharIso1) == Cd.CharIso1 0x3F),
            HU.testCase     "Char to CharIso1 #3" (Cd.cdConvert '\x00' HU.@?= Cd.CharIso1 0x00),
            HU.testCase     "Char to CharIso1 #4" (Cd.cdConvert '\x01' HU.@?= Cd.CharIso1 0x01),
            HU.testCase     "Char to CharIso1 #5" (Cd.cdConvert '\x1E' HU.@?= Cd.CharIso1 0x1E),
            HU.testCase     "Char to CharIso1 #6" (Cd.cdConvert '\x1F' HU.@?= Cd.CharIso1 0x1F),
            HU.testCase     "Char to CharIso1 #7" (Cd.cdConvert ' ' HU.@?= Cd.CharIso1 0x20),
            HU.testCase     "Char to CharIso1 #8" (Cd.cdConvert '!' HU.@?= Cd.CharIso1 0x21),
            HU.testCase     "Char to CharIso1 #9" (Cd.cdConvert '~' HU.@?= Cd.CharIso1 0x7E),
            HU.testCase    "Char to CharIso1 #10" (Cd.cdConvert '\xA0' HU.@?= Cd.CharIso1 0xA0),
            HU.testCase    "Char to CharIso1 #11" (Cd.cdConvert '¡' HU.@?= Cd.CharIso1 0xA1),
            HU.testCase    "Char to CharIso1 #12" (Cd.cdConvert '¢' HU.@?= Cd.CharIso1 0xA2),
            HU.testCase    "Char to CharIso1 #13" (Cd.cdConvert '£' HU.@?= Cd.CharIso1 0xA3),
            HU.testCase    "Char to CharIso1 #14" (Cd.cdConvert '¤' HU.@?= Cd.CharIso1 0xA4),
            HU.testCase    "Char to CharIso1 #15" (Cd.cdConvert '¥' HU.@?= Cd.CharIso1 0xA5),
            HU.testCase    "Char to CharIso1 #16" (Cd.cdConvert '©' HU.@?= Cd.CharIso1 0xA9),
            HU.testCase    "Char to CharIso1 #17" (Cd.cdConvert '°' HU.@?= Cd.CharIso1 0xB0),
            HU.testCase    "Char to CharIso1 #18" (Cd.cdConvert '±' HU.@?= Cd.CharIso1 0xB1),
            HU.testCase    "Char to CharIso1 #19" (Cd.cdConvert '²' HU.@?= Cd.CharIso1 0xB2),
            HU.testCase    "Char to CharIso1 #20" (Cd.cdConvert '³' HU.@?= Cd.CharIso1 0xB3),
            HU.testCase    "Char to CharIso1 #21" (Cd.cdConvert 'µ' HU.@?= Cd.CharIso1 0xB5),
            HU.testCase    "Char to CharIso1 #22" (Cd.cdConvert 'Å' HU.@?= Cd.CharIso1 0xC5),
            HU.testCase    "Char to CharIso1 #23" (Cd.cdConvert '×' HU.@?= Cd.CharIso1 0xD7),
            HU.testCase    "Char to CharIso1 #24" (Cd.cdConvert 'Ø' HU.@?= Cd.CharIso1 0xD8),
            HU.testCase    "Char to CharIso1 #25" (Cd.cdConvert 'ß' HU.@?= Cd.CharIso1 0xDF),
            HU.testCase    "Char to CharIso1 #26" (Cd.cdConvert '÷' HU.@?= Cd.CharIso1 0xF7),
            HU.testCase    "Char to CharIso1 #27" (Cd.cdConvert 'þ' HU.@?= Cd.CharIso1 0xFE),
            HU.testCase    "Char to CharIso1 #28" (Cd.cdConvert 'ÿ' HU.@?= Cd.CharIso1 0xFF),
        {-  * Char to CharWin1 - validated ✅
                * justification:
                * completeness: ✅ (the complete set of possible test values/data is used in unit tests, or values for all possible equivalence classes are used or ramdom sample values/data from complete set of possible values/data is used in case of high-volume cases)
                * independence: ❌ (test method is independent from library functions that are under test, or dependent on tested functions)
                    NOTE: correct functioning depends on "Cd.cdFromChar :: Char -> CharIso1" test
                    mitigation: ✅ supplemental test data (at least 10 more characters) are use in unit tests
                * boundaries  : ✅ (corner cases and boundaries are tested by unit tests)
                * conform doc.: ✅ (all tests are conform to documentation, e.g. source code comments by haddock) -}
            QC.testProperty "Char to CharWin1 #1" (\cd -> elem cd lchConvertableToCW1 QC.==> (Cd.cdConvert (cd :: Char) :: Cd.CharWin1) == Cd.cdFromChar cd),
            QC.testProperty "Char to CharWin1 #2" (\cd -> notElem cd lchConvertableToCW1 QC.==> (Cd.cdConvert (cd :: Char) :: Cd.CharWin1) == Cd.CharWin1 0x3F),
            HU.testCase     "Char to CharWin1 #3" (Cd.cdConvert '\x00' HU.@?= Cd.CharWin1 0x00),
            HU.testCase     "Char to CharWin1 #4" (Cd.cdConvert '\x01' HU.@?= Cd.CharWin1 0x01),
            HU.testCase     "Char to CharWin1 #5" (Cd.cdConvert ' ' HU.@?= Cd.CharWin1 0x20),
            HU.testCase     "Char to CharWin1 #6" (Cd.cdConvert '!' HU.@?= Cd.CharWin1 0x21),
            HU.testCase     "Char to CharWin1 #7" (Cd.cdConvert '~' HU.@?= Cd.CharWin1 0x7E),
            HU.testCase     "Char to CharWin1 #8" (Cd.cdConvert '\x7F' HU.@?= Cd.CharWin1 0x7F),
            HU.testCase     "Char to CharWin1 #9" (Cd.cdConvert '€' HU.@?= Cd.CharWin1 0x80),
            HU.testCase    "Char to CharWin1 #10" (Cd.cdConvert '‚' HU.@?= Cd.CharWin1 0x82),
            HU.testCase    "Char to CharWin1 #11" (Cd.cdConvert 'Œ' HU.@?= Cd.CharWin1 0x8C),
            HU.testCase    "Char to CharWin1 #12" (Cd.cdConvert 'Ž' HU.@?= Cd.CharWin1 0x8E),
            HU.testCase    "Char to CharWin1 #13" (Cd.cdConvert '‘' HU.@?= Cd.CharWin1 0x91),
            HU.testCase    "Char to CharWin1 #14" (Cd.cdConvert 'œ' HU.@?= Cd.CharWin1 0x9C),
            HU.testCase    "Char to CharWin1 #15" (Cd.cdConvert 'ž' HU.@?= Cd.CharWin1 0x9E),
            HU.testCase    "Char to CharWin1 #16" (Cd.cdConvert 'Ÿ' HU.@?= Cd.CharWin1 0x9F),
            HU.testCase    "Char to CharWin1 #17" (Cd.cdConvert '\xA0' HU.@?= Cd.CharWin1 0xA0),
            HU.testCase    "Char to CharWin1 #18" (Cd.cdConvert '¡' HU.@?= Cd.CharWin1 0xA1),
            HU.testCase    "Char to CharWin1 #19" (Cd.cdConvert '¤' HU.@?= Cd.CharWin1 0xA4),
            HU.testCase    "Char to CharWin1 #20" (Cd.cdConvert '¥' HU.@?= Cd.CharWin1 0xA5),
            HU.testCase    "Char to CharWin1 #21" (Cd.cdConvert '§' HU.@?= Cd.CharWin1 0xA7),
            HU.testCase    "Char to CharWin1 #22" (Cd.cdConvert '©' HU.@?= Cd.CharWin1 0xA9),
            HU.testCase    "Char to CharWin1 #23" (Cd.cdConvert 'Å' HU.@?= Cd.CharWin1 0xC5),
            HU.testCase    "Char to CharWin1 #24" (Cd.cdConvert 'Ö' HU.@?= Cd.CharWin1 0xD6),
            HU.testCase    "Char to CharWin1 #25" (Cd.cdConvert 'å' HU.@?= Cd.CharWin1 0xE5),
            HU.testCase    "Char to CharWin1 #26" (Cd.cdConvert '×' HU.@?= Cd.CharWin1 0xD7),
            HU.testCase    "Char to CharWin1 #27" (Cd.cdConvert 'Ø' HU.@?= Cd.CharWin1 0xD8),
            HU.testCase    "Char to CharWin1 #28" (Cd.cdConvert '÷' HU.@?= Cd.CharWin1 0xF7),
            HU.testCase    "Char to CharWin1 #29" (Cd.cdConvert 'ø' HU.@?= Cd.CharWin1 0xF8),
            HU.testCase    "Char to CharWin1 #30" (Cd.cdConvert 'þ' HU.@?= Cd.CharWin1 0xFE),
            HU.testCase    "Char to CharWin1 #31" (Cd.cdConvert 'ÿ' HU.@?= Cd.CharWin1 0xFF),
        {-  * Char to Octets.Octet - validated ✅
                * justification:
                * completeness: ✅ (the complete set of possible test values/data is used in unit tests, or values for all possible equivalence classes are used or ramdom sample values/data from complete set of possible values/data is used in case of high-volume cases)
                * independence: ✅ (test method is independent from library functions that are under test, or dependent on tested functions)
                * boundaries  : ✅ (corner cases and boundaries are tested by unit tests)
                * conform doc.: ✅ (all tests are conform to documentation, e.g. source code comments by haddock) -}
            QC.testProperty "Char to Octets.Octet #1" (\cd -> Chr.ord cd <= 0xFF QC.==> (Cd.cdConvert (cd :: Char) :: Oct.Octet) == fromIntegral (Chr.ord cd)),
            QC.testProperty "Char to Octets.Octet #2" (\cd -> Chr.ord cd > 0xFF QC.==> (Cd.cdConvert (cd :: Char) :: Oct.Octet) == 0xFF),
            HU.testCase     "Char to Octets.Octet #3" (Cd.cdConvert '\x00' HU.@?= (0x00 :: Oct.Octet)),
            HU.testCase     "Char to Octets.Octet #4" (Cd.cdConvert '\x01' HU.@?= (0x01 :: Oct.Octet)),
            HU.testCase     "Char to Octets.Octet #5" (Cd.cdConvert '\xFE' HU.@?= (0xFE :: Oct.Octet)),
            HU.testCase     "Char to Octets.Octet #6" (Cd.cdConvert '\xFF' HU.@?= (0xFF :: Oct.Octet)),
            HU.testCase     "Char to Octets.Octet #7" (Cd.cdConvert '\x100' HU.@?= (0xFF :: Oct.Octet)),
            HU.testCase     "Char to Octets.Octet #8" (Cd.cdConvert '\x101' HU.@?= (0xFF :: Oct.Octet)),
            HU.testCase     "Char to Octets.Octet #9" (Cd.cdConvert '\x10FFE' HU.@?= (0xFF :: Oct.Octet)),
            HU.testCase    "Char to Octets.Octet #10" (Cd.cdConvert '\x10FFF' HU.@?= (0xFF :: Oct.Octet)),
        {-  * Char to Bool - validated ✅
                * justification:
                * completeness: ✅ (the complete set of possible test values/data is used in unit tests, or values for all possible equivalence classes are used or ramdom sample values/data from complete set of possible values/data is used in case of high-volume cases)
                * independence: ✅ (test method is independent from library functions that are under test, or dependent on tested functions)
                * boundaries  : ✅ (corner cases and boundaries are tested by unit tests)
                * conform doc.: ✅ (all tests are conform to documentation, e.g. source code comments by haddock) -}
            QC.testProperty "Char to Bool #1" (\cd -> True QC.==> (Cd.cdConvert (cd :: Char) :: Bool) == (cd /= '0')),
            HU.testCase     "Char to Bool #2" (Cd.cdConvert '\x00' HU.@?= True),
            HU.testCase     "Char to Bool #3" (Cd.cdConvert '\x01' HU.@?= True),
            HU.testCase     "Char to Bool #4" (Cd.cdConvert '\x10FFE' HU.@?= True),
            HU.testCase     "Char to Bool #5" (Cd.cdConvert '\x10FFF' HU.@?= True),
            HU.testCase     "Char to Bool #6" (Cd.cdConvert '0' HU.@?= False),
            HU.testCase     "Char to Bool #7" (Cd.cdConvert '1' HU.@?= True),
        {-  * CharUtf8 to Char - validated ✅
                * justification:
                * completeness: ✅ (the complete set of possible test values/data is used in unit tests, or values for all possible equivalence classes are used or ramdom sample values/data from complete set of possible values/data is used in case of high-volume cases)
                * independence: ❌ (test method is independent from library functions that are under test, or dependent on tested functions)
                    NOTE: correct functioning depends on "Cd.chFromCode :: CharUtf8 -> Char" test
                    mitigation: ✅ supplemental test data (at least NNN more characters) are use in unit tests
                * boundaries  : ✅ (corner cases and boundaries are tested by unit tests)
                * conform doc.: ✅ (all tests are conform to documentation, e.g. source code comments by haddock) -}
            QC.testProperty "CharUtf8 to Char #1a" (\cd -> Cd.isValid (cd :: Cd.CharUtf8) QC.==> (Cd.cdConvert cd :: Char) == Cd.chFromCode cd),
            QC.testProperty "CharUtf8 to Char #1b" (\cd -> not (Cd.isValid (cd :: Cd.CharUtf8)) QC.==> (Cd.cdConvert cd :: Char) == '\xFFFD'), -- '\xFFFD' equals Cd.cdReplacement
            HU.testCase     "CharUtf8 to Char #2" (Cd.cdConvert (Cd.CU8OneByte 0x00) HU.@?= '\x00'),
            HU.testCase     "CharUtf8 to Char #3" (Cd.cdConvert (Cd.CU8OneByte 0x01) HU.@?= '\x01'),
            HU.testCase     "CharUtf8 to Char #4" (Cd.cdConvert (Cd.CU8OneByte 0x20) HU.@?= ' '),
            HU.testCase     "CharUtf8 to Char #5" (Cd.cdConvert (Cd.CU8OneByte 0x21) HU.@?= '!'),
            HU.testCase     "CharUtf8 to Char #6" (Cd.cdConvert (Cd.CU8OneByte 0x2F) HU.@?= '/'),
            HU.testCase     "CharUtf8 to Char #7" (Cd.cdConvert (Cd.CU8OneByte 0x30) HU.@?= '0'),
            HU.testCase     "CharUtf8 to Char #8" (Cd.cdConvert (Cd.CU8OneByte 0x31) HU.@?= '1'),
            HU.testCase     "CharUtf8 to Char #9" (Cd.cdConvert (Cd.CU8OneByte 0x38) HU.@?= '8'),
            HU.testCase    "CharUtf8 to Char #10" (Cd.cdConvert (Cd.CU8OneByte 0x39) HU.@?= '9'),
            HU.testCase    "CharUtf8 to Char #11" (Cd.cdConvert (Cd.CU8OneByte 0x3F) HU.@?= '?'),
            HU.testCase    "CharUtf8 to Char #12" (Cd.cdConvert (Cd.CU8OneByte 0x40) HU.@?= '@'),
            HU.testCase    "CharUtf8 to Char #13" (Cd.cdConvert (Cd.CU8OneByte 0x41) HU.@?= 'A'),
            HU.testCase    "CharUtf8 to Char #14" (Cd.cdConvert (Cd.CU8OneByte 0x42) HU.@?= 'B'),
            HU.testCase    "CharUtf8 to Char #15" (Cd.cdConvert (Cd.CU8OneByte 0x59) HU.@?= 'Y'),
            HU.testCase    "CharUtf8 to Char #16" (Cd.cdConvert (Cd.CU8OneByte 0x5A) HU.@?= 'Z'),
            HU.testCase    "CharUtf8 to Char #17" (Cd.cdConvert (Cd.CU8OneByte 0x5B) HU.@?= '['),
            HU.testCase    "CharUtf8 to Char #18" (Cd.cdConvert (Cd.CU8OneByte 0x5C) HU.@?= '\\'),
            HU.testCase    "CharUtf8 to Char #19" (Cd.cdConvert (Cd.CU8OneByte 0x5D) HU.@?= ']'),
            HU.testCase    "CharUtf8 to Char #20" (Cd.cdConvert (Cd.CU8OneByte 0x5E) HU.@?= '^'),
            HU.testCase    "CharUtf8 to Char #21" (Cd.cdConvert (Cd.CU8OneByte 0x5F) HU.@?= '_'),
            HU.testCase    "CharUtf8 to Char #22" (Cd.cdConvert (Cd.CU8OneByte 0x60) HU.@?= '`'),
            HU.testCase    "CharUtf8 to Char #23" (Cd.cdConvert (Cd.CU8OneByte 0x61) HU.@?= 'a'),
            HU.testCase    "CharUtf8 to Char #24" (Cd.cdConvert (Cd.CU8OneByte 0x62) HU.@?= 'b'),
            HU.testCase    "CharUtf8 to Char #25" (Cd.cdConvert (Cd.CU8OneByte 0x79) HU.@?= 'y'),
            HU.testCase    "CharUtf8 to Char #26" (Cd.cdConvert (Cd.CU8OneByte 0x7A) HU.@?= 'z'),
            HU.testCase    "CharUtf8 to Char #27" (Cd.cdConvert (Cd.CU8OneByte 0x7B) HU.@?= '{'),
            HU.testCase    "CharUtf8 to Char #28" (Cd.cdConvert (Cd.CU8OneByte 0x7C) HU.@?= '|'),
            HU.testCase    "CharUtf8 to Char #29" (Cd.cdConvert (Cd.CU8OneByte 0x7D) HU.@?= '}'),
            HU.testCase    "CharUtf8 to Char #30" (Cd.cdConvert (Cd.CU8OneByte 0x7E) HU.@?= '~'),
            HU.testCase    "CharUtf8 to Char #31" (Cd.cdConvert (Cd.CU8OneByte 0x7E) HU.@?= '\x7E'),
            HU.testCase    "CharUtf8 to Char #32" (Cd.cdConvert (Cd.CU8OneByte 0x7F) HU.@?= '\x7F'),
            HU.testCase    "CharUtf8 to Char #33" (Cd.cdConvert (Cd.CU8TwoBytes 0b11000010 0b10000000) HU.@?= '\x80'),
            HU.testCase    "CharUtf8 to Char #34" (Cd.cdConvert (Cd.CU8TwoBytes 0b11000010 0b10000001) HU.@?= '\x81'),
            HU.testCase    "CharUtf8 to Char #35" (Cd.cdConvert (Cd.CU8TwoBytes 0xC2 0xA1) HU.@?= '¡'),
            HU.testCase    "CharUtf8 to Char #36" (Cd.cdConvert (Cd.CU8TwoBytes 0xC2 0xA2) HU.@?= '¢'),
            HU.testCase    "CharUtf8 to Char #37" (Cd.cdConvert (Cd.CU8TwoBytes 0xC2 0xA3) HU.@?= '£'),
            HU.testCase    "CharUtf8 to Char #38" (Cd.cdConvert (Cd.CU8TwoBytes 0xC2 0xA4) HU.@?= '¤'),
            HU.testCase    "CharUtf8 to Char #39" (Cd.cdConvert (Cd.CU8TwoBytes 0xC3 0xBC) HU.@?= 'ü'),
            HU.testCase    "CharUtf8 to Char #40" (Cd.cdConvert (Cd.CU8TwoBytes 0xC3 0xBD) HU.@?= 'ý'),
            HU.testCase    "CharUtf8 to Char #41" (Cd.cdConvert (Cd.CU8TwoBytes 0b11000011 0b10111110) HU.@?= 'þ'),
            HU.testCase    "CharUtf8 to Char #42" (Cd.cdConvert (Cd.CU8TwoBytes 0b11000011 0b10111111) HU.@?= 'ÿ'),
            HU.testCase    "CharUtf8 to Char #43" (Cd.cdConvert (Cd.CU8TwoBytes 0b11000011 0b10111110) HU.@?= '\xFE'),
            HU.testCase    "CharUtf8 to Char #44" (Cd.cdConvert (Cd.CU8TwoBytes 0b11000011 0b10111111) HU.@?= '\xFF'),
            HU.testCase    "CharUtf8 to Char #45" (Cd.cdConvert (Cd.CU8TwoBytes 0b11000100 0b10000000) HU.@?= '\x100'),
            HU.testCase    "CharUtf8 to Char #46" (Cd.cdConvert (Cd.CU8TwoBytes 0b11000100 0b10000001) HU.@?= '\x101'),
            HU.testCase    "CharUtf8 to Char #47" (Cd.cdConvert (Cd.CU8TwoBytes 0b11000100 0b10000000) HU.@?= 'Ā'),
            HU.testCase    "CharUtf8 to Char #48" (Cd.cdConvert (Cd.CU8TwoBytes 0b11000100 0b10000001) HU.@?= 'ā'),
            HU.testCase    "CharUtf8 to Char #49" (Cd.cdConvert (Cd.CU8TwoBytes 0xC4 0x82) HU.@?= 'Ă'),
            HU.testCase    "CharUtf8 to Char #50" (Cd.cdConvert (Cd.CU8TwoBytes 0xC4 0x83) HU.@?= 'ă'),
            HU.testCase    "CharUtf8 to Char #51" (Cd.cdConvert (Cd.CU8TwoBytes 0xCE 0xB1) HU.@?= 'α'),
            HU.testCase    "CharUtf8 to Char #52" (Cd.cdConvert (Cd.CU8TwoBytes 0xCE 0xB2) HU.@?= 'β'),
            HU.testCase    "CharUtf8 to Char #53" (Cd.cdConvert (Cd.CU8TwoBytes 0xCF 0x88) HU.@?= 'ψ'),
            HU.testCase    "CharUtf8 to Char #54" (Cd.cdConvert (Cd.CU8TwoBytes 0xCF 0x89) HU.@?= 'ω'),
            HU.testCase    "CharUtf8 to Char #55" (Cd.cdConvert (Cd.CU8TwoBytes 0xDF 0xBA) HU.@?= 'ߺ'),
            HU.testCase    "CharUtf8 to Char #56" (Cd.cdConvert (Cd.CU8TwoBytes 0xDF 0xBD) HU.@?= '߽'),
            HU.testCase    "CharUtf8 to Char #57" (Cd.cdConvert (Cd.CU8TwoBytes 0xDF 0xBE) HU.@?= '߾'),
            HU.testCase    "CharUtf8 to Char #58" (Cd.cdConvert (Cd.CU8TwoBytes 0xDF 0xBF) HU.@?= '߿'),
            HU.testCase    "CharUtf8 to Char #59" (Cd.cdConvert (Cd.CU8TwoBytes 0b11011111 0b10111110) HU.@?= '\x7FE'),
            HU.testCase    "CharUtf8 to Char #60" (Cd.cdConvert (Cd.CU8TwoBytes 0b11011111 0b10111111) HU.@?= '\x7FF'),
            HU.testCase    "CharUtf8 to Char #61" (Cd.cdConvert (Cd.CU8ThreeBytes 0b11100000 0b10100000 0b10000000) HU.@?= '\x800'),
            HU.testCase    "CharUtf8 to Char #62" (Cd.cdConvert (Cd.CU8ThreeBytes 0b11100000 0b10100000 0b10000001) HU.@?= '\x801'),
            HU.testCase    "CharUtf8 to Char #63" (Cd.cdConvert (Cd.CU8ThreeBytes 0b11100000 0b10100000 0b10000000) HU.@?= '\x800'),
            HU.testCase    "CharUtf8 to Char #64" (Cd.cdConvert (Cd.CU8ThreeBytes 0xE0 0xA2 0xA0) HU.@?= 'ࢠ'),
            HU.testCase    "CharUtf8 to Char #65" (Cd.cdConvert (Cd.CU8ThreeBytes 0xE0 0xA2 0xA1) HU.@?= 'ࢡ'),
            HU.testCase    "CharUtf8 to Char #66" (Cd.cdConvert (Cd.CU8ThreeBytes 0xE0 0xA2 0xA2) HU.@?= 'ࢢ'),
            HU.testCase    "CharUtf8 to Char #67" (Cd.cdConvert (Cd.CU8ThreeBytes 0xE0 0xA2 0xA3) HU.@?= 'ࢣ'),
            HU.testCase    "CharUtf8 to Char #68" (Cd.cdConvert (Cd.CU8ThreeBytes 0xE2 0x88 0x80) HU.@?= '∀'),
            HU.testCase    "CharUtf8 to Char #69" (Cd.cdConvert (Cd.CU8ThreeBytes 0xE2 0x88 0x81) HU.@?= '∁'),
            HU.testCase    "CharUtf8 to Char #70" (Cd.cdConvert (Cd.CU8ThreeBytes 0xE2 0x88 0x82) HU.@?= '∂'),
            HU.testCase    "CharUtf8 to Char #71" (Cd.cdConvert (Cd.CU8ThreeBytes 0xE2 0x88 0x83) HU.@?= '∃'),
            HU.testCase    "CharUtf8 to Char #72" (Cd.cdConvert (Cd.CU8ThreeBytes 0xED 0x9E 0xB0) HU.@?= 'ힰ'),
            HU.testCase    "CharUtf8 to Char #73" (Cd.cdConvert (Cd.CU8ThreeBytes 0xED 0x9E 0xB1) HU.@?= 'ힱ'),
            HU.testCase    "CharUtf8 to Char #74" (Cd.cdConvert (Cd.CU8ThreeBytes 0xED 0x9E 0xB2) HU.@?= 'ힲ'),
            HU.testCase    "CharUtf8 to Char #75" (Cd.cdConvert (Cd.CU8ThreeBytes 0xED 0x9E 0xB3) HU.@?= 'ힳ'),
            HU.testCase    "CharUtf8 to Char #76" (Cd.cdConvert (Cd.CU8ThreeBytes 0xED 0x9F 0xB8) HU.@?= 'ퟸ'),
            HU.testCase    "CharUtf8 to Char #77" (Cd.cdConvert (Cd.CU8ThreeBytes 0xED 0x9F 0xB9) HU.@?= 'ퟹ'),
            HU.testCase    "CharUtf8 to Char #78" (Cd.cdConvert (Cd.CU8ThreeBytes 0xED 0x9F 0xBA) HU.@?= 'ퟺ'),
            HU.testCase    "CharUtf8 to Char #79" (Cd.cdConvert (Cd.CU8ThreeBytes 0xED 0x9F 0xBB) HU.@?= 'ퟻ'),
            HU.testCase    "CharUtf8 to Char #80" (Cd.cdConvert (Cd.CU8ThreeBytes 0xE7 0x82 0xBA) HU.@?= '為'),
            HU.testCase    "CharUtf8 to Char #81" (Cd.cdConvert (Cd.CU8ThreeBytes 0xE7 0x82 0xBB) HU.@?= '炻'),
            HU.testCase    "CharUtf8 to Char #82" (Cd.cdConvert (Cd.CU8ThreeBytes 0xE7 0x82 0xBC) HU.@?= '炼'),
            HU.testCase    "CharUtf8 to Char #83" (Cd.cdConvert (Cd.CU8ThreeBytes 0xE7 0x82 0xBD) HU.@?= '炽'),
            HU.testCase    "CharUtf8 to Char #84" (Cd.cdConvert (Cd.CU8ThreeBytes 0xE7 0x84 0xA1) HU.@?= '無'),
            HU.testCase    "CharUtf8 to Char #85" (Cd.cdConvert (Cd.CU8ThreeBytes 0xE7 0x84 0xA2) HU.@?= '焢'),
            HU.testCase    "CharUtf8 to Char #86" (Cd.cdConvert (Cd.CU8ThreeBytes 0xE7 0x84 0xA3) HU.@?= '焣'),
            HU.testCase    "CharUtf8 to Char #87" (Cd.cdConvert (Cd.CU8ThreeBytes 0xE7 0x84 0xA4) HU.@?= '焤'),
            HU.testCase    "CharUtf8 to Char #88" (Cd.cdConvert (Cd.CU8ThreeBytes 0xE7 0xA6 0xAA) HU.@?= '禪'),
            HU.testCase    "CharUtf8 to Char #89" (Cd.cdConvert (Cd.CU8ThreeBytes 0xE7 0xA6 0xAB) HU.@?= '禫'),
            HU.testCase    "CharUtf8 to Char #90" (Cd.cdConvert (Cd.CU8ThreeBytes 0xE7 0xA6 0xAC) HU.@?= '禬'),
            HU.testCase    "CharUtf8 to Char #91" (Cd.cdConvert (Cd.CU8ThreeBytes 0xE7 0xA6 0xAD) HU.@?= '禭'),
            HU.testCase    "CharUtf8 to Char #92" (Cd.cdConvert (Cd.CU8ThreeBytes 0xEF 0xAD 0x8C) HU.@?= 'בֿ'),
            HU.testCase    "CharUtf8 to Char #93" (Cd.cdConvert (Cd.CU8ThreeBytes 0xEF 0xAC 0x80) HU.@?= 'ﬀ'),
            HU.testCase    "CharUtf8 to Char #94" (Cd.cdConvert (Cd.CU8ThreeBytes 0xEF 0xAC 0x81) HU.@?= 'ﬁ'),
            HU.testCase    "CharUtf8 to Char #95" (Cd.cdConvert (Cd.CU8ThreeBytes 0xEF 0xAC 0x82) HU.@?= 'ﬂ'),
            HU.testCase    "CharUtf8 to Char #96" (Cd.cdConvert (Cd.CU8ThreeBytes 0xEF 0xAC 0x83) HU.@?= 'ﬃ'),
            HU.testCase    "CharUtf8 to Char #97" (Cd.cdConvert (Cd.CU8ThreeBytes 0xEF 0xAD 0x8D) HU.@?= 'כֿ'),
            HU.testCase    "CharUtf8 to Char #98" (Cd.cdConvert (Cd.CU8ThreeBytes 0xEF 0xAD 0x8E) HU.@?= 'פֿ'),
            HU.testCase    "CharUtf8 to Char #99" (Cd.cdConvert (Cd.CU8ThreeBytes 0xEF 0xAD 0x8F) HU.@?= 'ﭏ'),
            HU.testCase   "CharUtf8 to Char #100" (Cd.cdConvert (Cd.CU8ThreeBytes 0b11101111 0b10111111 0b10111101) HU.@?= '�'),
            HU.testCase   "CharUtf8 to Char #101" (Cd.cdConvert (Cd.CU8ThreeBytes 0b11101111 0b10111111 0b10111110) HU.@?= '\xFFFE'),
            HU.testCase   "CharUtf8 to Char #102" (Cd.cdConvert (Cd.CU8ThreeBytes 0b11101111 0b10111111 0b10111111) HU.@?= '\xFFFF'),
            HU.testCase   "CharUtf8 to Char #103" (Cd.cdConvert (Cd.CU8FourBytes 0xF0 0xAF 0xA0 0x84) HU.@?= '你'),
            HU.testCase   "CharUtf8 to Char #104" (Cd.cdConvert (Cd.CU8FourBytes 0xF0 0xAF 0xA0 0x8F) HU.@?= '兔'),
            HU.testCase   "CharUtf8 to Char #105" (Cd.cdConvert (Cd.CU8FourBytes 0xF0 0xAF 0xA0 0x95) HU.@?= '再'),
            HU.testCase   "CharUtf8 to Char #106" (Cd.cdConvert (Cd.CU8FourBytes 0xF0 0xAF 0xA0 0x98) HU.@?= '冤'),
            HU.testCase   "CharUtf8 to Char #107" (Cd.cdConvert (Cd.CU8FourBytes 0xF0 0x90 0xA0 0x80) HU.@?= '𐠀'),
            HU.testCase   "CharUtf8 to Char #108" (Cd.cdConvert (Cd.CU8FourBytes 0xF0 0x90 0xA0 0x81) HU.@?= '𐠁'),
            HU.testCase   "CharUtf8 to Char #109" (Cd.cdConvert (Cd.CU8FourBytes 0xF0 0x90 0xA0 0x82) HU.@?= '𐠂'),
            HU.testCase   "CharUtf8 to Char #110" (Cd.cdConvert (Cd.CU8FourBytes 0xF0 0x90 0xA0 0x83) HU.@?= '𐠃'),
            HU.testCase   "CharUtf8 to Char #111" (Cd.cdConvert (Cd.CU8FourBytes 0b11110000 0b10010000 0b10000000 0b10000000) HU.@?= '\x10000'),
            HU.testCase   "CharUtf8 to Char #112" (Cd.cdConvert (Cd.CU8FourBytes 0b11110000 0b10010000 0b10000000 0b10000001) HU.@?= '\x10001'),
            HU.testCase   "CharUtf8 to Char #113" (Cd.cdConvert (Cd.CU8FourBytes 0b11110100 0b10001111 0b10111111 0b10111111) HU.@?= '\x10FFFF'),
            HU.testCase   "CharUtf8 to Char #114" (Cd.cdConvert (Cd.CU8OneByte 0x80) HU.@?= '\xFFFD'), -- replacement character ('�')
            HU.testCase   "CharUtf8 to Char #115" (Cd.cdConvert (Cd.CU8OneByte 0x81) HU.@?= '\xFFFD'), -- replacement character ('�')
            HU.testCase   "CharUtf8 to Char #116" (Cd.cdConvert (Cd.CU8OneByte 0xFE) HU.@?= '\xFFFD'), -- replacement character ('�')
            HU.testCase   "CharUtf8 to Char #117" (Cd.cdConvert (Cd.CU8OneByte 0xFF) HU.@?= '\xFFFD'), -- replacement character ('�')
            HU.testCase   "CharUtf8 to Char #118" (Cd.cdConvert (Cd.CU8TwoBytes 0b11000000 0b11000000) HU.@?= '\xFFFD'), -- replacement character ('�')
            HU.testCase   "CharUtf8 to Char #119" (Cd.cdConvert (Cd.CU8TwoBytes 0b11100000 0b10000000) HU.@?= '\xFFFD'), -- replacement character ('�')
            HU.testCase   "CharUtf8 to Char #120" (Cd.cdConvert (Cd.CU8TwoBytes 0b11000000 0b00000000) HU.@?= '\xFFFD'), -- replacement character ('�')
            HU.testCase   "CharUtf8 to Char #121" (Cd.cdConvert (Cd.CU8TwoBytes 0b01000000 0b10000000) HU.@?= '\xFFFD'), -- replacement character ('�')
            HU.testCase   "CharUtf8 to Char #122" (Cd.cdConvert (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b11000000) HU.@?= '\xFFFD'), -- replacement character ('�')
            HU.testCase   "CharUtf8 to Char #123" (Cd.cdConvert (Cd.CU8ThreeBytes 0b11100000 0b11000000 0b10000000) HU.@?= '\xFFFD'), -- replacement character ('�')
            HU.testCase   "CharUtf8 to Char #124" (Cd.cdConvert (Cd.CU8ThreeBytes 0b11110000 0b10000000 0b10000000) HU.@?= '\xFFFD'), -- replacement character ('�')
            HU.testCase   "CharUtf8 to Char #125" (Cd.cdConvert (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b00000000) HU.@?= '\xFFFD'), -- replacement character ('�')
            HU.testCase   "CharUtf8 to Char #126" (Cd.cdConvert (Cd.CU8ThreeBytes 0b11100000 0b00000000 0b10000000) HU.@?= '\xFFFD'), -- replacement character ('�')
            HU.testCase   "CharUtf8 to Char #127" (Cd.cdConvert (Cd.CU8ThreeBytes 0b01100000 0b10000000 0b10000000) HU.@?= '\xFFFD'), -- replacement character ('�')
            HU.testCase   "CharUtf8 to Char #128" (Cd.cdConvert (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b11000000) HU.@?= '\xFFFD'), -- replacement character ('�')
            HU.testCase   "CharUtf8 to Char #129" (Cd.cdConvert (Cd.CU8FourBytes 0b11110000 0b10000000 0b11000000 0b10000000) HU.@?= '\xFFFD'), -- replacement character ('�')
            HU.testCase   "CharUtf8 to Char #130" (Cd.cdConvert (Cd.CU8FourBytes 0b11110000 0b11000000 0b10000000 0b10000000) HU.@?= '\xFFFD'), -- replacement character ('�')
            HU.testCase   "CharUtf8 to Char #131" (Cd.cdConvert (Cd.CU8FourBytes 0b11111000 0b10000000 0b10000000 0b10000000) HU.@?= '\xFFFD'), -- replacement character ('�')
            HU.testCase   "CharUtf8 to Char #132" (Cd.cdConvert (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b00000000) HU.@?= '\xFFFD'), -- replacement character ('�')
            HU.testCase   "CharUtf8 to Char #133" (Cd.cdConvert (Cd.CU8FourBytes 0b11110000 0b10000000 0b00000000 0b10000000) HU.@?= '\xFFFD'), -- replacement character ('�')
            HU.testCase   "CharUtf8 to Char #134" (Cd.cdConvert (Cd.CU8FourBytes 0b11110000 0b00000000 0b10000000 0b10000000) HU.@?= '\xFFFD'), -- replacement character ('�')
            HU.testCase   "CharUtf8 to Char #135" (Cd.cdConvert (Cd.CU8FourBytes 0b01110000 0b10000000 0b10000000 0b10000000) HU.@?= '\xFFFD'), -- replacement character ('�')
        {-  * CharUtf8 to CharUtf8 - validated ✅
                * justification:
                * completeness: ✅ (the complete set of possible test values/data is used in unit tests, or values for all possible equivalence classes are used or ramdom sample values/data from complete set of possible values/data is used in case of high-volume cases)
                * independence: ✅ (test method is independent from library functions that are under test, or dependent on tested functions)
                * boundaries  : ✅ (corner cases and boundaries are tested by unit tests)
                * conform doc.: ✅ (all tests are conform to documentation, e.g. source code comments by haddock) -}
            QC.testProperty "CharUtf8 to CharUtf8 #1" (\cd -> Cd.isValid (cd :: Cd.CharUtf8) QC.==> (Cd.cdConvert cd :: Cd.CharUtf8) == cd),
            QC.testProperty "CharUtf8 to CharUtf8 #2" (\cd -> not (Cd.isValid (cd :: Cd.CharUtf8)) QC.==> (Cd.cdConvert cd :: Cd.CharUtf8) == Cd.cdReplacement),
            HU.testCase     "CharUtf8 to CharUtf8 #3" (Cd.cdConvert (Cd.CU8OneByte 0x00) HU.@?= Cd.CU8OneByte 0x00),
            HU.testCase     "CharUtf8 to CharUtf8 #4" (Cd.cdConvert (Cd.CU8OneByte 0x01) HU.@?= Cd.CU8OneByte 0x01),
            HU.testCase     "CharUtf8 to CharUtf8 #5" (Cd.cdConvert (Cd.CU8OneByte 0x7E) HU.@?= Cd.CU8OneByte 0x7E),
            HU.testCase     "CharUtf8 to CharUtf8 #6" (Cd.cdConvert (Cd.CU8OneByte 0x7F) HU.@?= Cd.CU8OneByte 0x7F),
            HU.testCase     "CharUtf8 to CharUtf8 #7" (Cd.cdConvert (Cd.CU8OneByte 0x80) HU.@?= Cd.CU8ThreeBytes 0xEF 0xBF 0xBD), -- replacement character ('�')
            HU.testCase     "CharUtf8 to CharUtf8 #8" (Cd.cdConvert (Cd.CU8OneByte 0x81) HU.@?= Cd.CU8ThreeBytes 0xEF 0xBF 0xBD), -- replacement character ('�')
            HU.testCase     "CharUtf8 to CharUtf8 #9" (Cd.cdConvert (Cd.CU8OneByte 0xFE) HU.@?= Cd.CU8ThreeBytes 0xEF 0xBF 0xBD), -- replacement character ('�')
            HU.testCase    "CharUtf8 to CharUtf8 #10" (Cd.cdConvert (Cd.CU8OneByte 0xFF) HU.@?= Cd.CU8ThreeBytes 0xEF 0xBF 0xBD), -- replacement character ('�')
            HU.testCase    "CharUtf8 to CharUtf8 #11" (Cd.cdConvert (Cd.CU8TwoBytes 0b11000000 0b10000000) HU.@?= Cd.CU8TwoBytes 0b11000000 0b10000000),
            HU.testCase    "CharUtf8 to CharUtf8 #12" (Cd.cdConvert (Cd.CU8TwoBytes 0b11000000 0b10000001) HU.@?= Cd.CU8TwoBytes 0b11000000 0b10000001),
            HU.testCase    "CharUtf8 to CharUtf8 #13" (Cd.cdConvert (Cd.CU8TwoBytes 0b11011111 0b10111110) HU.@?= Cd.CU8TwoBytes 0b11011111 0b10111110),
            HU.testCase    "CharUtf8 to CharUtf8 #14" (Cd.cdConvert (Cd.CU8TwoBytes 0b11011111 0b10111111) HU.@?= Cd.CU8TwoBytes 0b11011111 0b10111111),
            HU.testCase    "CharUtf8 to CharUtf8 #15" (Cd.cdConvert (Cd.CU8TwoBytes 0b11000000 0b11000000) HU.@?= Cd.CU8ThreeBytes 0xEF 0xBF 0xBD), -- replacement character ('�')
            HU.testCase    "CharUtf8 to CharUtf8 #16" (Cd.cdConvert (Cd.CU8TwoBytes 0b11100000 0b10000000) HU.@?= Cd.CU8ThreeBytes 0xEF 0xBF 0xBD), -- replacement character ('�')
            HU.testCase    "CharUtf8 to CharUtf8 #17" (Cd.cdConvert (Cd.CU8TwoBytes 0b11000000 0b00000000) HU.@?= Cd.CU8ThreeBytes 0xEF 0xBF 0xBD), -- replacement character ('�')
            HU.testCase    "CharUtf8 to CharUtf8 #18" (Cd.cdConvert (Cd.CU8TwoBytes 0b01000000 0b10000000) HU.@?= Cd.CU8ThreeBytes 0xEF 0xBF 0xBD), -- replacement character ('�')
            HU.testCase    "CharUtf8 to CharUtf8 #19" (Cd.cdConvert (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b10000000) HU.@?= Cd.CU8ThreeBytes 0b11100000 0b10000000 0b10000000),
            HU.testCase    "CharUtf8 to CharUtf8 #20" (Cd.cdConvert (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b10000001) HU.@?= Cd.CU8ThreeBytes 0b11100000 0b10000000 0b10000001),
            HU.testCase    "CharUtf8 to CharUtf8 #21" (Cd.cdConvert (Cd.CU8ThreeBytes 0b11101111 0b10111111 0b10111110) HU.@?= Cd.CU8ThreeBytes 0b11101111 0b10111111 0b10111110),
            HU.testCase    "CharUtf8 to CharUtf8 #22" (Cd.cdConvert (Cd.CU8ThreeBytes 0b11101111 0b10111111 0b10111111) HU.@?= Cd.CU8ThreeBytes 0b11101111 0b10111111 0b10111111),
            HU.testCase    "CharUtf8 to CharUtf8 #23" (Cd.cdConvert (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b11000000) HU.@?= Cd.CU8ThreeBytes 0xEF 0xBF 0xBD), -- replacement character ('�')
            HU.testCase    "CharUtf8 to CharUtf8 #24" (Cd.cdConvert (Cd.CU8ThreeBytes 0b11100000 0b11000000 0b10000000) HU.@?= Cd.CU8ThreeBytes 0xEF 0xBF 0xBD), -- replacement character ('�')
            HU.testCase    "CharUtf8 to CharUtf8 #25" (Cd.cdConvert (Cd.CU8ThreeBytes 0b11110000 0b10000000 0b10000000) HU.@?= Cd.CU8ThreeBytes 0xEF 0xBF 0xBD), -- replacement character ('�')
            HU.testCase    "CharUtf8 to CharUtf8 #26" (Cd.cdConvert (Cd.CU8ThreeBytes 0b11100000 0b10000000 0b00000000) HU.@?= Cd.CU8ThreeBytes 0xEF 0xBF 0xBD), -- replacement character ('�')
            HU.testCase    "CharUtf8 to CharUtf8 #27" (Cd.cdConvert (Cd.CU8ThreeBytes 0b11100000 0b00000000 0b10000000) HU.@?= Cd.CU8ThreeBytes 0xEF 0xBF 0xBD), -- replacement character ('�')
            HU.testCase    "CharUtf8 to CharUtf8 #28" (Cd.cdConvert (Cd.CU8ThreeBytes 0b01100000 0b10000000 0b10000000) HU.@?= Cd.CU8ThreeBytes 0xEF 0xBF 0xBD), -- replacement character ('�')
            HU.testCase    "CharUtf8 to CharUtf8 #29" (Cd.cdConvert (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b10000000) HU.@?= Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b10000000),
            HU.testCase    "CharUtf8 to CharUtf8 #30" (Cd.cdConvert (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b10000001) HU.@?= Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b10000001),
            HU.testCase    "CharUtf8 to CharUtf8 #31" (Cd.cdConvert (Cd.CU8FourBytes 0b11110100 0b10001111 0b10111111 0b10111110) HU.@?= Cd.CU8FourBytes 0b11110100 0b10001111 0b10111111 0b10111110),
            HU.testCase    "CharUtf8 to CharUtf8 #32" (Cd.cdConvert (Cd.CU8FourBytes 0b11110100 0b10001111 0b10111111 0b10111111) HU.@?= Cd.CU8FourBytes 0b11110100 0b10001111 0b10111111 0b10111111),
            HU.testCase    "CharUtf8 to CharUtf8 #33" (Cd.cdConvert (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b1100000) HU.@?= Cd.CU8ThreeBytes 0xEF 0xBF 0xBD), -- replacement character ('�')
            HU.testCase    "CharUtf8 to CharUtf8 #34" (Cd.cdConvert (Cd.CU8FourBytes 0b11110000 0b10000000 0b1100000 0b10000000) HU.@?= Cd.CU8ThreeBytes 0xEF 0xBF 0xBD), -- replacement character ('�')
            HU.testCase    "CharUtf8 to CharUtf8 #35" (Cd.cdConvert (Cd.CU8FourBytes 0b11110000 0b1100000 0b10000000 0b10000000) HU.@?= Cd.CU8ThreeBytes 0xEF 0xBF 0xBD), -- replacement character ('�')
            HU.testCase    "CharUtf8 to CharUtf8 #36" (Cd.cdConvert (Cd.CU8FourBytes 0b11111000 0b10000000 0b10000000 0b10000000) HU.@?= Cd.CU8ThreeBytes 0xEF 0xBF 0xBD), -- replacement character ('�')
            HU.testCase    "CharUtf8 to CharUtf8 #37" (Cd.cdConvert (Cd.CU8FourBytes 0b11110000 0b10000000 0b10000000 0b00000000) HU.@?= Cd.CU8ThreeBytes 0xEF 0xBF 0xBD), -- replacement character ('�')
            HU.testCase    "CharUtf8 to CharUtf8 #38" (Cd.cdConvert (Cd.CU8FourBytes 0b11110000 0b10000000 0b00000000 0b10000000) HU.@?= Cd.CU8ThreeBytes 0xEF 0xBF 0xBD), -- replacement character ('�')
            HU.testCase    "CharUtf8 to CharUtf8 #39" (Cd.cdConvert (Cd.CU8FourBytes 0b11110000 0b00000000 0b10000000 0b10000000) HU.@?= Cd.CU8ThreeBytes 0xEF 0xBF 0xBD), -- replacement character ('�')
            HU.testCase    "CharUtf8 to CharUtf8 #40" (Cd.cdConvert (Cd.CU8FourBytes 0b01110000 0b10000000 0b10000000 0b10000000) HU.@?= Cd.CU8ThreeBytes 0xEF 0xBF 0xBD), -- replacement character ('�')
        {-  * CharUtf8 to CharIso1 - validated ❓
                * justification:
                * completeness: ❓ (the complete set of possible test values/data is used in unit tests, or values for all possible equivalence classes are used or ramdom sample values/data from complete set of possible values/data is used in case of high-volume cases)
                * independence: ❓ (test method is independent from library functions that are under test, or dependent on tested functions)
                * boundaries  : ❓ (corner cases and boundaries are tested by unit tests)
                * conform doc.: ✅ (all tests are conform to documentation, e.g. source code comments by haddock) -}

        {-  * CharUtf8 to CharWin1 - validated ❓
                * justification:
                * completeness: ❓ (the complete set of possible test values/data is used in unit tests, or values for all possible equivalence classes are used or ramdom sample values/data from complete set of possible values/data is used in case of high-volume cases)
                * independence: ❓ (test method is independent from library functions that are under test, or dependent on tested functions)
                * boundaries  : ❓ (corner cases and boundaries are tested by unit tests)
                * conform doc.: ✅ (all tests are conform to documentation, e.g. source code comments by haddock) -}

        {-  * CharUtf8 to Octets.Octet - validated ❓
                * justification:
                * completeness: ❓ (the complete set of possible test values/data is used in unit tests, or values for all possible equivalence classes are used or ramdom sample values/data from complete set of possible values/data is used in case of high-volume cases)
                * independence: ❓ (test method is independent from library functions that are under test, or dependent on tested functions)
                * boundaries  : ❓ (corner cases and boundaries are tested by unit tests)
                * conform doc.: ✅ (all tests are conform to documentation, e.g. source code comments by haddock) -}

        {-  * CharUtf8 to Bool - validated ❓
                * justification:
                * completeness: ❓ (the complete set of possible test values/data is used in unit tests, or values for all possible equivalence classes are used or ramdom sample values/data from complete set of possible values/data is used in case of high-volume cases)
                * independence: ❓ (test method is independent from library functions that are under test, or dependent on tested functions)
                * boundaries  : ❓ (corner cases and boundaries are tested by unit tests)
                * conform doc.: ✅ (all tests are conform to documentation, e.g. source code comments by haddock) -}

        {-  * CharIso1 to Char - validated ❓
                * justification:
                * completeness: ❓ (the complete set of possible test values/data is used in unit tests, or values for all possible equivalence classes are used or ramdom sample values/data from complete set of possible values/data is used in case of high-volume cases)
                * independence: ❓ (test method is independent from library functions that are under test, or dependent on tested functions)
                * boundaries  : ❓ (corner cases and boundaries are tested by unit tests)
                * conform doc.: ✅ (all tests are conform to documentation, e.g. source code comments by haddock) -}

        {-  * CharIso1 to CharUtf8 - validated ❓
                * justification:
                * completeness: ❓ (the complete set of possible test values/data is used in unit tests, or values for all possible equivalence classes are used or ramdom sample values/data from complete set of possible values/data is used in case of high-volume cases)
                * independence: ❓ (test method is independent from library functions that are under test, or dependent on tested functions)
                * boundaries  : ❓ (corner cases and boundaries are tested by unit tests)
                * conform doc.: ✅ (all tests are conform to documentation, e.g. source code comments by haddock) -}

        {-  * CharIso1 to CharIso1 - validated ❓
                * justification:
                * completeness: ❓ (the complete set of possible test values/data is used in unit tests, or values for all possible equivalence classes are used or ramdom sample values/data from complete set of possible values/data is used in case of high-volume cases)
                * independence: ❓ (test method is independent from library functions that are under test, or dependent on tested functions)
                * boundaries  : ❓ (corner cases and boundaries are tested by unit tests)
                * conform doc.: ✅ (all tests are conform to documentation, e.g. source code comments by haddock) -}

        {-  * CharIso1 to CharWin1 - validated ❓
                * justification:
                * completeness: ❓ (the complete set of possible test values/data is used in unit tests, or values for all possible equivalence classes are used or ramdom sample values/data from complete set of possible values/data is used in case of high-volume cases)
                * independence: ❓ (test method is independent from library functions that are under test, or dependent on tested functions)
                * boundaries  : ❓ (corner cases and boundaries are tested by unit tests)
                * conform doc.: ✅ (all tests are conform to documentation, e.g. source code comments by haddock) -}

        {-  * CharIso1 to Octets.Octet - validated ❓
                * justification:
                * completeness: ❓ (the complete set of possible test values/data is used in unit tests, or values for all possible equivalence classes are used or ramdom sample values/data from complete set of possible values/data is used in case of high-volume cases)
                * independence: ❓ (test method is independent from library functions that are under test, or dependent on tested functions)
                * boundaries  : ❓ (corner cases and boundaries are tested by unit tests)
                * conform doc.: ✅ (all tests are conform to documentation, e.g. source code comments by haddock) -}

        {-  * CharIso1 to Bool - validated ❓
                * justification:
                * completeness: ❓ (the complete set of possible test values/data is used in unit tests, or values for all possible equivalence classes are used or ramdom sample values/data from complete set of possible values/data is used in case of high-volume cases)
                * independence: ❓ (test method is independent from library functions that are under test, or dependent on tested functions)
                * boundaries  : ❓ (corner cases and boundaries are tested by unit tests)
                * conform doc.: ✅ (all tests are conform to documentation, e.g. source code comments by haddock) -}

        {-  * CharWin1 to Char - validated ✅
                * justification:
                * completeness: ✅ (the complete set of possible test values/data is used in unit tests, or values for all possible equivalence classes are used or ramdom sample values/data from complete set of possible values/data is used in case of high-volume cases)
                * independence: ❌ (test method is independent from library functions that are under test, or dependent on tested functions)
                    NOTE: correct functioning depends on "Cd.cdFromChar :: Char -> CharIso1" test
                    mitigation: ✅
                        - supplemental test data (more than 10 characters) are use in unit tests
                        - ramdom sample data from complete set of non-convertable characters are tested
                * boundaries  : ✅ (corner cases and boundaries are tested by unit tests)
                * conform doc.: ✅ (all tests are conform to documentation, e.g. source code comments by haddock) -}
            QC.testProperty "CharWin1 to Char #1a" (\cd -> Cd.isValid (cd :: Cd.CharWin1) QC.==> (Cd.cdConvert cd :: Char) == Cd.chFromCode cd),
            QC.testProperty "CharWin1 to Char #1b" (\cd -> not (Cd.isValid (cd :: Cd.CharWin1)) QC.==> (Cd.cdConvert cd :: Char) == '\xFFFD'), -- '\xFFFD' equals Cd.cdReplacement
            HU.testCase     "CharWin1 to Char #2" (Cd.cdConvert (Cd.CharWin1 0x00) HU.@?= '\x00'), -- NUL
            HU.testCase     "CharWin1 to Char #2" (Cd.cdConvert (Cd.CharWin1 0x01) HU.@?= '\x01'), -- SOH
            HU.testCase     "CharWin1 to Char #3" (Cd.cdConvert (Cd.CharWin1 0x20) HU.@?= ' '),
            HU.testCase     "CharWin1 to Char #4" (Cd.cdConvert (Cd.CharWin1 0x21) HU.@?= '!'),
            HU.testCase     "CharWin1 to Char #5" (Cd.cdConvert (Cd.CharWin1 0x7E) HU.@?= '~'),
            HU.testCase     "CharWin1 to Char #6" (Cd.cdConvert (Cd.CharWin1 0x7F) HU.@?= '\x7F'), -- DEL
            HU.testCase     "CharWin1 to Char #7" (Cd.cdConvert (Cd.CharWin1 0x80) HU.@?= '€'),
            HU.testCase     "CharWin1 to Char #8" (Cd.cdConvert (Cd.CharWin1 0x81) HU.@?= '\xFFFD'), -- replacement character ('�')
            HU.testCase     "CharWin1 to Char #9" (Cd.cdConvert (Cd.CharWin1 0x82) HU.@?= '‚'),
            HU.testCase    "CharWin1 to Char #10" (Cd.cdConvert (Cd.CharWin1 0x8C) HU.@?= 'Œ'),
            HU.testCase    "CharWin1 to Char #11" (Cd.cdConvert (Cd.CharWin1 0x8D) HU.@?= '\xFFFD'), -- replacement character ('�')
            HU.testCase    "CharWin1 to Char #12" (Cd.cdConvert (Cd.CharWin1 0x8E) HU.@?= 'Ž'),
            HU.testCase    "CharWin1 to Char #13" (Cd.cdConvert (Cd.CharWin1 0x8F) HU.@?= '\xFFFD'), -- replacement character ('�')
            HU.testCase    "CharWin1 to Char #14" (Cd.cdConvert (Cd.CharWin1 0x90) HU.@?= '\xFFFD'), -- replacement character ('�')
            HU.testCase    "CharWin1 to Char #15" (Cd.cdConvert (Cd.CharWin1 0x91) HU.@?= '‘'),
            HU.testCase    "CharWin1 to Char #16" (Cd.cdConvert (Cd.CharWin1 0x9C) HU.@?= 'œ'),
            HU.testCase    "CharWin1 to Char #17" (Cd.cdConvert (Cd.CharWin1 0x9D) HU.@?= '\xFFFD'), -- replacement character ('�')
            HU.testCase    "CharWin1 to Char #18" (Cd.cdConvert (Cd.CharWin1 0x9E) HU.@?= 'ž'),
            HU.testCase    "CharWin1 to Char #19" (Cd.cdConvert (Cd.CharWin1 0x9F) HU.@?= 'Ÿ'),
            HU.testCase    "CharWin1 to Char #20" (Cd.cdConvert (Cd.CharWin1 0xA0) HU.@?= '\xA0'),
            HU.testCase    "CharWin1 to Char #21" (Cd.cdConvert (Cd.CharWin1 0xA1) HU.@?= '¡'),
            HU.testCase    "CharWin1 to Char #21" (Cd.cdConvert (Cd.CharWin1 0xA6) HU.@?= '¦'),
            HU.testCase    "CharWin1 to Char #21" (Cd.cdConvert (Cd.CharWin1 0xA7) HU.@?= '§'),
            HU.testCase    "CharWin1 to Char #21" (Cd.cdConvert (Cd.CharWin1 0xA9) HU.@?= '©'),
            HU.testCase    "CharWin1 to Char #21" (Cd.cdConvert (Cd.CharWin1 0xB0) HU.@?= '°'),
            HU.testCase    "CharWin1 to Char #21" (Cd.cdConvert (Cd.CharWin1 0xB2) HU.@?= '²'),
            HU.testCase    "CharWin1 to Char #21" (Cd.cdConvert (Cd.CharWin1 0xB3) HU.@?= '³'),
            HU.testCase    "CharWin1 to Char #21" (Cd.cdConvert (Cd.CharWin1 0xB5) HU.@?= 'µ'),
            HU.testCase    "CharWin1 to Char #21" (Cd.cdConvert (Cd.CharWin1 0xC4) HU.@?= 'Ä'),
            HU.testCase    "CharWin1 to Char #21" (Cd.cdConvert (Cd.CharWin1 0xC5) HU.@?= 'Å'),
            HU.testCase    "CharWin1 to Char #21" (Cd.cdConvert (Cd.CharWin1 0xC6) HU.@?= 'Æ'),
            HU.testCase    "CharWin1 to Char #21" (Cd.cdConvert (Cd.CharWin1 0xD6) HU.@?= 'Ö'),
            HU.testCase    "CharWin1 to Char #21" (Cd.cdConvert (Cd.CharWin1 0xD7) HU.@?= '×'),
            HU.testCase    "CharWin1 to Char #21" (Cd.cdConvert (Cd.CharWin1 0xD8) HU.@?= 'Ø'),
            HU.testCase    "CharWin1 to Char #21" (Cd.cdConvert (Cd.CharWin1 0xDF) HU.@?= 'ß'),
            HU.testCase    "CharWin1 to Char #22" (Cd.cdConvert (Cd.CharWin1 0xFF) HU.@?= 'ÿ')

        {-  * CharWin1 to CharUtf8 - validated ❓
                * justification:
                * completeness: ❓ (the complete set of possible test values/data is used in unit tests, or values for all possible equivalence classes are used or ramdom sample values/data from complete set of possible values/data is used in case of high-volume cases)
                * independence: ❓ (test method is independent from library functions that are under test, or dependent on tested functions)
                * boundaries  : ❓ (corner cases and boundaries are tested by unit tests)
                * conform doc.: ✅ (all tests are conform to documentation, e.g. source code comments by haddock) -}

        {-  * CharWin1 to CharIso1 - validated ❓
                * justification:
                * completeness: ❓ (the complete set of possible test values/data is used in unit tests, or values for all possible equivalence classes are used or ramdom sample values/data from complete set of possible values/data is used in case of high-volume cases)
                * independence: ❓ (test method is independent from library functions that are under test, or dependent on tested functions)
                * boundaries  : ❓ (corner cases and boundaries are tested by unit tests)
                * conform doc.: ✅ (all tests are conform to documentation, e.g. source code comments by haddock) -}

        {-  * CharWin1 to CharWin1 - validated ❓
                * justification:
                * completeness: ❓ (the complete set of possible test values/data is used in unit tests, or values for all possible equivalence classes are used or ramdom sample values/data from complete set of possible values/data is used in case of high-volume cases)
                * independence: ❓ (test method is independent from library functions that are under test, or dependent on tested functions)
                * boundaries  : ❓ (corner cases and boundaries are tested by unit tests)
                * conform doc.: ✅ (all tests are conform to documentation, e.g. source code comments by haddock) -}

        {-  * CharWin1 to Octets.Octet - validated ❓
                * justification:
                * completeness: ❓ (the complete set of possible test values/data is used in unit tests, or values for all possible equivalence classes are used or ramdom sample values/data from complete set of possible values/data is used in case of high-volume cases)
                * independence: ❓ (test method is independent from library functions that are under test, or dependent on tested functions)
                * boundaries  : ❓ (corner cases and boundaries are tested by unit tests)
                * conform doc.: ✅ (all tests are conform to documentation, e.g. source code comments by haddock) -}

        {-  * CharWin1 to Bool - validated ❓
                * justification:
                * completeness: ❓ (the complete set of possible test values/data is used in unit tests, or values for all possible equivalence classes are used or ramdom sample values/data from complete set of possible values/data is used in case of high-volume cases)
                * independence: ❓ (test method is independent from library functions that are under test, or dependent on tested functions)
                * boundaries  : ❓ (corner cases and boundaries are tested by unit tests)
                * conform doc.: ✅ (all tests are conform to documentation, e.g. source code comments by haddock) -}
        ]

{-  * lcdConvert - validated ❓
        * justification:
            * completeness: ❌ - not yet (the function is tested for all type and types combinations)
            * stubbed     : ❌ (the default function of the class is also tested, if any) -}
tgUnitlcdConvert :: T.TestTree
tgUnitlcdConvert =
    T.testGroup
        "lcdConvert"
        [
        ]

convertCI1ToChar' :: Cd.CharIso1 -> Char
convertCI1ToChar' (Cd.CharIso1 oct)
    | oct >= 0x7F && oct <= 0x9F  = '\xFFFD'
    | otherwise = Chr.chr (fromIntegral oct)

lchConvertableToCI1 :: String
lchConvertableToCI1 = ['\x00'..'\x7F'] ++ ['\xA0'..'\xFF']

lchConvertableToCW1 :: String
lchConvertableToCW1 =
    ['\x00'..'\x7F'] ++ 
    [
        '€',            '‚',    'ƒ',    '„',    '…',    '†',    '‡',
        'ˆ',    '‰',    'Š',    '‹',    'Œ',            'Ž',
                '‘',    '’',    '“',    '”',    '•',    '–',    '—',
        '˜',    '™',    'š',    '›',    'œ',            'ž',    'Ÿ',
        '\xA0', '¡',    '¢',    '£',    '¤',    '¥',    '¦',    '§',
        '¨',    '©',    'ª',    '«',    '¬',    '\xAD', '®',    '¯',
        '°',    '±',    '²',    '³',    '´',    'µ',    '¶',    '·',
        '¸',    '¹',    'º',    '»',    '¼',    '½',    '¾',    '¿',
        'À',    'Á',    'Â',    'Ã',    'Ä',    'Å',    'Æ',    'Ç',
        'È',    'É',    'Ê',    'Ë',    'Ì',    'Í',    'Î',    'Ï',
        'Ð',    'Ñ',    'Ò',    'Ó',    'Ô',    'Õ',    'Ö',    '×',
        'Ø',    'Ù',    'Ú',    'Û',    'Ü',    'Ý',    'Þ',    'ß',
        'à',    'á',    'â',    'ã',    'ä',    'å',    'æ',    'ç',
        'è',    'é',    'ê',    'ë',    'ì',    'í',    'î',    'ï',
        'ð',    'ñ',    'ò',    'ó',    'ô',    'õ',    'ö',    '÷',
        'ø',    'ù',    'ú',    'û',    'ü',    'ý',    'þ',    'ÿ'
    ]
