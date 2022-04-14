{-|
Description : test.
Copyright   : (c) Jörg K.-H. W. Brüggmann, 2021-2022
License     : proprietary, to be dual licensed
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX
-}

module SpecTypedCodeOctets
    (
        testGroup
    ) where

import qualified Test.Tasty as T
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit as HU

import qualified TestCaseUnwrap as TCU

import qualified TypedCodeOctets as TCO

testGroup :: T.TestTree
testGroup = 
    T.testGroup 
        "TypedCodeOctets" 
        (
            (TCU.testCasesFromTuple2 TCO.lTestCasesTuple2) ++ 
            [
                tgDetectBOM, 
                tgDetectEncoding
            ]
        )

tgDetectBOM :: T.TestTree
tgDetectBOM = 
    T.testGroup 
        "detectBOM" 
        [
            HU.testCase "#1" $ 
                (TCO.detectBOM [0xEF, 0xBB, 0xBF]) HU.@?= 
                ([], TCO.CodeTypeUtf8Char (Just True)), 
            HU.testCase "#2" $ 
                (TCO.detectBOM 
                ([0xEF, 0xBB, 0xBF, 0x33])) HU.@?= 
                ([0x33], TCO.CodeTypeUtf8Char (Just True)), 
            HU.testCase "#3" $ 
                (TCO.detectBOM ([0xEF, 0xBB, 0xBF, 0x34, 0x35])) HU.@?= 
                ([0x34, 0x35], TCO.CodeTypeUtf8Char (Just True)), 
            HU.testCase "#4" $ 
                (TCO.detectBOM ([0xEF, 0xBB, 0xBE, 0x36])) HU.@?= 
                ([0xEF, 0xBB, 0xBE, 0x36], TCO.CodeTypeSelfDetecting TCO.All), 
            HU.testCase "#5" $ 
                (TCO.detectBOM ([0xEF, 0xBA, 0xBF, 0x37])) HU.@?= 
                ([0xEF, 0xBA, 0xBF, 0x37], TCO.CodeTypeSelfDetecting TCO.All), 
            HU.testCase "#6" $ 
                (TCO.detectBOM ([0xEE, 0xBB, 0xBF, 0x38])) HU.@?= 
                ([0xEE, 0xBB, 0xBF, 0x38], TCO.CodeTypeSelfDetecting TCO.All), 
            HU.testCase "#7" $ 
                (TCO.detectBOM ([])) HU.@?= 
                ([], TCO.CodeTypeSelfDetecting TCO.All)
            ]

tgDetectEncoding :: T.TestTree
tgDetectEncoding = 
    T.testGroup 
        "detectEncoding" 
        [
            HU.testCase "#1" $ 
                (TCO.detectEncoding TCO.All [0xEF, 0xBB, 0xBF]) HU.@?= (TCO.Utf8CharOctets (Just True) []), 
            HU.testCase "#2" $ 
                (TCO.detectEncoding TCO.All [0xEF, 0xBB, 0xBF, 0x00]) HU.@?= (TCO.Utf8CharOctets (Just True) [0x00]), 
            HU.testCase "#3" $ 
                (TCO.detectEncoding TCO.All ([0xF0, 0x80, 0x80, 0xBE, 0xE0, 0x80, 0xBE, 0xC0, 0xBE, 0x00])) HU.@?= 
                (TCO.Utf8CharOctets (Just False) ([0xF0, 0x80, 0x80, 0xBE, 0xE0, 0x80, 0xBE, 0xC0, 0xBE, 0x00])), 
            HU.testCase "#4" $ 
                (TCO.detectEncoding TCO.All ([0x48, 0x61, 0x6C, 0x6C, 0x6F, 0x21, 0x21, 0x21, 0x21, 0xA0])) HU.@?= 
                (TCO.CI1CharOctets ([0x48, 0x61, 0x6C, 0x6C, 0x6F, 0x21, 0x21, 0x21, 0x21, 0xA0])), 
            HU.testCase "#5" $ 
                (TCO.detectEncoding TCO.All ([0x48, 0x61, 0x6C, 0x6C, 0x6F, 0x21, 0x21, 0x21, 0x21, 0xFF])) HU.@?= 
                (TCO.CI1CharOctets ([0x48, 0x61, 0x6C, 0x6C, 0x6F, 0x21, 0x21, 0x21, 0x21, 0xFF])), 
            HU.testCase "#6" $ 
                (TCO.detectEncoding TCO.All ([0x48, 0x61, 0x6C, 0x6C, 0x6F, 0x21, 0x21, 0x21, 0x21, 0x80])) HU.@?= 
                (TCO.CW1CharOctets ([0x48, 0x61, 0x6C, 0x6C, 0x6F, 0x21, 0x21, 0x21, 0x21, 0x80])), 
            HU.testCase "#7" $ 
                (TCO.detectEncoding TCO.All ([0x48, 0x61, 0x6C, 0x6C, 0x6F, 0x21, 0x21, 0x21, 0x21, 0x81])) HU.@?= 
                (TCO.CI1CharOctets ([0x48, 0x61, 0x6C, 0x6C, 0x6F, 0x21, 0x21, 0x21, 0x21, 0x81])), 
            HU.testCase "#8" $ 
                (TCO.detectEncoding TCO.All ([0x48, 0x61, 0x6C, 0x6C, 0x6F, 0x21, 0x21, 0x21, 0x21, 0x82])) HU.@?= 
                (TCO.CW1CharOctets ([0x48, 0x61, 0x6C, 0x6C, 0x6F, 0x21, 0x21, 0x21, 0x21, 0x82]))
        ]
