{-|
Description : test.
Copyright   : (c) Jörg K.-H. W. Brüggmann, 2021-2022
License     : proprietary, to be dual licensed
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX
-}

module SpecOctetable
    (
        testGroup
    ) where

import qualified Test.Tasty as T
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit as HU

import qualified Octetable as Oct

testGroup :: T.TestTree
testGroup = 
    T.testGroup 
        "Octetable" 
        [
            tgInstanceWord32, 
            tgInstanceWord64, 
            tgInstanceChar
        ]

tgInstanceWord32 :: T.TestTree
tgInstanceWord32 = 
    T.testGroup 
        "instance Word32" 
        [
            QC.testProperty 
                "fromOctets #1" 
                (\w32 -> (True QC.==> (Oct.fromOctets (Oct.toOctets w32)) == (w32 :: Oct.Word32))), 
            HU.testCase 
                "fromOctets #2" 
                ( ((Oct.fromOctets [0x00, 0x00, 0x00, 0x00]) :: Oct.Word32) HU.@?= 0x00000000 ), 
            HU.testCase 
                "fromOctets #3" 
                ( ((Oct.fromOctets [0x00, 0x00, 0x00, 0x01]) :: Oct.Word32) HU.@?= 0x00000001 ), 
            HU.testCase 
                "fromOctets #4" 
                ( ((Oct.fromOctets [0x00, 0x00, 0x00, 0x02]) :: Oct.Word32) HU.@?= 0x00000002 ), 
            HU.testCase 
                "fromOctets #5" 
                ( ((Oct.fromOctets [0xA5, 0x5A, 0x5A, 0xA5]) :: Oct.Word32) HU.@?= 0xA55A5AA5 ), 
            HU.testCase 
                "fromOctets #6" 
                ( ((Oct.fromOctets [0x5A, 0x5A, 0xA5, 0xA5]) :: Oct.Word32) HU.@?= 0x5A5AA5A5 ), 
            HU.testCase 
                "fromOctets #7" 
                ( ((Oct.fromOctets [0x5A, 0xA5, 0xA5, 0x5A]) :: Oct.Word32) HU.@?= 0x5AA5A55A ), 
            HU.testCase 
                "fromOctets #8" 
                ( ((Oct.fromOctets [0xFF, 0xFF, 0xFF, 0xFD]) :: Oct.Word32) HU.@?= 0xFFFFFFFD ), 
            HU.testCase 
                "fromOctets #9" 
                ( ((Oct.fromOctets [0xFF, 0xFF, 0xFF, 0xFE]) :: Oct.Word32) HU.@?= 0xFFFFFFFE ), 
            HU.testCase 
                "fromOctets #10" 
                ( ((Oct.fromOctets [0xFF, 0xFF, 0xFF, 0xFF]) :: Oct.Word32) HU.@?= 0xFFFFFFFF ), 
            HU.testCase 
                "toOctets #1" 
                ( (Oct.toOctets (0x00000000 :: Oct.Word32)) HU.@?= [0x00, 0x00, 0x00, 0x00] ), 
            HU.testCase 
                "toOctets #2" 
                ( (Oct.toOctets (0x00000001 :: Oct.Word32)) HU.@?= [0x00, 0x00, 0x00, 0x01] ), 
            HU.testCase 
                "toOctets #3" 
                ( (Oct.toOctets (0x00000002 :: Oct.Word32)) HU.@?= [0x00, 0x00, 0x00, 0x02] ), 
            HU.testCase 
                "toOctets #4" 
                ( (Oct.toOctets (0xA55A5AA5 :: Oct.Word32)) HU.@?= [0xA5, 0x5A, 0x5A, 0xA5] ), 
            HU.testCase 
                "toOctets #5" 
                ( (Oct.toOctets (0xA5A55A5A :: Oct.Word32)) HU.@?= [0xA5, 0xA5, 0x5A, 0x5A] ), 
            HU.testCase 
                "toOctets #6" 
                ( (Oct.toOctets (0x5AA5A55A :: Oct.Word32)) HU.@?= [0x5A, 0xA5, 0xA5, 0x5A] ), 
            HU.testCase 
                "toOctets #7" 
                ( (Oct.toOctets (0xFFFFFFFD :: Oct.Word32)) HU.@?= [0xFF, 0xFF, 0xFF, 0xFD] ), 
            HU.testCase 
                "toOctets #8" 
                ( (Oct.toOctets (0xFFFFFFFE :: Oct.Word32)) HU.@?= [0xFF, 0xFF, 0xFF, 0xFE] ), 
            HU.testCase 
                "toOctets #9" 
                ( (Oct.toOctets (0xFFFFFFFF :: Oct.Word32)) HU.@?= [0xFF, 0xFF, 0xFF, 0xFF] )
        ]

tgInstanceWord64 :: T.TestTree
tgInstanceWord64 = 
    T.testGroup 
        "instance Word64" 
        [
            QC.testProperty 
                "fromOctets #1" 
                (\w64 -> (True QC.==> (Oct.fromOctets (Oct.toOctets w64)) == (w64 :: Oct.Word64))), 
            HU.testCase 
                "fromOctets #1" 
                ( ((Oct.fromOctets [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]) :: Oct.Word64) HU.@?= 0x0000000000000000 ), 
            HU.testCase 
                "fromOctets #2" 
                ( ((Oct.fromOctets [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]) :: Oct.Word64) HU.@?= 0x0000000000000001 ), 
            HU.testCase 
                "fromOctets #3" 
                ( ((Oct.fromOctets [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02]) :: Oct.Word64) HU.@?= 0x0000000000000002 ), 
            HU.testCase 
                "fromOctets #4" 
                ( ((Oct.fromOctets [0x5A, 0xA5, 0xA5, 0x5A, 0xA5, 0x5A, 0x5A, 0xA5]) :: Oct.Word64) HU.@?= 0x5AA5A55AA55A5AA5 ), 
            HU.testCase 
                "fromOctets #5" 
                ( ((Oct.fromOctets [0xA5, 0xA5, 0x5A, 0xA5, 0x5A, 0x5A, 0xA5, 0x5A]) :: Oct.Word64) HU.@?= 0xA5A55AA55A5AA55A ), 
            HU.testCase 
                "fromOctets #6" 
                ( ((Oct.fromOctets [0xA5, 0x5A, 0xA5, 0x5A, 0x5A, 0xA5, 0x5A, 0xA5]) :: Oct.Word64) HU.@?= 0xA55AA55A5AA55AA5 ), 
            HU.testCase 
                "fromOctets #7" 
                ( ((Oct.fromOctets [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFD]) :: Oct.Word64) HU.@?= 0xFFFFFFFFFFFFFFFD ), 
            HU.testCase 
                "fromOctets #8" 
                ( ((Oct.fromOctets [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFE]) :: Oct.Word64) HU.@?= 0xFFFFFFFFFFFFFFFE ), 
            HU.testCase 
                "fromOctets #9" 
                ( ((Oct.fromOctets [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]) :: Oct.Word64) HU.@?= 0xFFFFFFFFFFFFFFFF ), 
            HU.testCase 
                "toOctets #1" 
                ( (Oct.toOctets (0x0000000000000000 :: Oct.Word64)) HU.@?= [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00] ), 
            HU.testCase 
                "toOctets #2" 
                ( (Oct.toOctets (0x0000000000000001 :: Oct.Word64)) HU.@?= [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01] ), 
            HU.testCase 
                "toOctets #3" 
                ( (Oct.toOctets (0x0000000000000002 :: Oct.Word64)) HU.@?= [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02] ), 
            HU.testCase 
                "toOctets #4" 
                ( (Oct.toOctets (0x5AA5A55AA55A5AA5 :: Oct.Word64)) HU.@?= [0x5A, 0xA5, 0xA5, 0x5A, 0xA5, 0x5A, 0x5A, 0xA5] ), 
            HU.testCase 
                "toOctets #5" 
                ( (Oct.toOctets (0xA55AA5A55AA55A5A :: Oct.Word64)) HU.@?= [0xA5, 0x5A, 0xA5, 0xA5, 0x5A, 0xA5, 0x5A, 0x5A] ), 
            HU.testCase 
                "toOctets #6" 
                ( (Oct.toOctets (0x5AA55AA5A55AA55A :: Oct.Word64)) HU.@?= [0x5A, 0xA5, 0x5A, 0xA5, 0xA5, 0x5A, 0xA5, 0x5A] ), 
            HU.testCase 
                "toOctets #7" 
                ( (Oct.toOctets (0xFFFFFFFFFFFFFFFD :: Oct.Word64)) HU.@?= [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFD] ), 
            HU.testCase 
                "toOctets #8" 
                ( (Oct.toOctets (0xFFFFFFFFFFFFFFFE :: Oct.Word64)) HU.@?= [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFE] ), 
            HU.testCase 
                "toOctets #9" 
                ( (Oct.toOctets (0xFFFFFFFFFFFFFFFF :: Oct.Word64)) HU.@?= [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF] )
        ]

tgInstanceChar :: T.TestTree
tgInstanceChar = 
    T.testGroup 
        "instance Char" 
        [
            QC.testProperty 
                "fromOctets #1" 
                (\ch -> (True QC.==> (Oct.fromOctets (Oct.toOctets ch)) == (ch :: Char))), 
            HU.testCase 
                "fromOctets #2" 
                ( ((Oct.fromOctets [0x00]) :: Char) HU.@?= '\0' ), 
            HU.testCase 
                "fromOctets #3" 
                ( ((Oct.fromOctets [0x01]) :: Char) HU.@?= '\x01' ), 
            HU.testCase 
                "fromOctets #4" 
                ( ((Oct.fromOctets [0x02]) :: Char) HU.@?= '\x02' ), 
            HU.testCase 
                "fromOctets #5" 
                ( ((Oct.fromOctets [0x20]) :: Char) HU.@?= ' ' ), 
            HU.testCase 
                "fromOctets #6" 
                ( ((Oct.fromOctets [0x21]) :: Char) HU.@?= '!' ), 
            HU.testCase 
                "fromOctets #7" 
                ( ((Oct.fromOctets [0x7E]) :: Char) HU.@?= '~' ), 
            HU.testCase 
                "fromOctets #8" 
                ( ((Oct.fromOctets [0x00, 0x80]) :: Char) HU.@?= '\x80' ), 
            HU.testCase 
                "fromOctets #9" 
                ( ((Oct.fromOctets [0x00, 0x81]) :: Char) HU.@?= '\x81' ), 
            HU.testCase 
                "fromOctets #10" 
                ( ((Oct.fromOctets [0x00, 0x9F]) :: Char) HU.@?= '\x9F' ), 
            HU.testCase 
                "fromOctets #11" 
                ( ((Oct.fromOctets [0x00, 0xA0]) :: Char) HU.@?= '\xA0' ), 
            HU.testCase 
                "fromOctets #12" 
                ( ((Oct.fromOctets [0x00, 0xA1]) :: Char) HU.@?= '¡' ), 
            HU.testCase 
                "fromOctets #13" 
                ( ((Oct.fromOctets [0x00, 0xFF]) :: Char) HU.@?= 'ÿ' ), 
            HU.testCase 
                "fromOctets #14" 
                ( ((Oct.fromOctets [0x01, 0x00]) :: Char) HU.@?= 'Ā' ), 
            HU.testCase 
                "fromOctets #15" 
                ( ((Oct.fromOctets [0x01, 0x7F]) :: Char) HU.@?= 'ſ' ), 
            HU.testCase 
                "fromOctets #16" 
                ( ((Oct.fromOctets [0x01, 0x80]) :: Char) HU.@?= 'ƀ' ), 
            HU.testCase 
                "fromOctets #17" 
                ( ((Oct.fromOctets [0x02, 0x4F]) :: Char) HU.@?= 'ɏ' ), 
            HU.testCase 
                "fromOctets #18" 
                ( ((Oct.fromOctets [0x02, 0x50]) :: Char) HU.@?= 'ɐ' ), 
            HU.testCase 
                "fromOctets #19" 
                ( ((Oct.fromOctets [0x02, 0xAF]) :: Char) HU.@?= 'ʯ' ), 
            HU.testCase 
                "fromOctets #20" 
                ( ((Oct.fromOctets [0x0F, 0xDA]) :: Char) HU.@?= '࿚' ), 
            HU.testCase 
                "fromOctets #21" 
                ( ((Oct.fromOctets [0x7F, 0xFF]) :: Char) HU.@?= '翿' ), 
            HU.testCase 
                "fromOctets #22" 
                ( ((Oct.fromOctets [0x00, 0x80, 0x00]) :: Char) HU.@?= '耀' ), 
            HU.testCase 
                "fromOctets #23" 
                ( ((Oct.fromOctets [0x10, 0xFF, 0xFE]) :: Char) HU.@?= '\x10FFFE' ), 
            HU.testCase 
                "fromOctets #24" 
                ( ((Oct.fromOctets [0x10, 0xFF, 0xFF]) :: Char) HU.@?= '\x10FFFF' ), 
            HU.testCase 
                "fromOctets #25" 
                ( ((Oct.fromOctets [0x80]) :: Char) HU.@?= '\0' ), 
            HU.testCase 
                "fromOctets #26" 
                ( ((Oct.fromOctets [0x80, 0x00]) :: Char) HU.@?= '\0' ), 
            HU.testCase 
                "fromOctets #27" 
                ( ((Oct.fromOctets [0x80, 0x00, 0x00]) :: Char) HU.@?= '\0' ), 
            HU.testCase 
                "fromOctets #28" 
                ( ((Oct.fromOctets [0x80, 0x00, 0x00, 0x00]) :: Char) HU.@?= '\0' )
        ]
