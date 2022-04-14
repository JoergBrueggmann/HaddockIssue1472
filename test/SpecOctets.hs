{-|
Description : test.
Copyright   : (c) Jörg K.-H. W. Brüggmann, 2021-2022
License     : proprietary, to be dual licensed
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE BinaryLiterals #-}

module SpecOctets
    (
        testGroup
    ) where

import qualified Test.Tasty as T
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit as HU

import qualified TestCaseUnwrap as TCU

import qualified Octets as Oct

testGroup :: T.TestTree
testGroup = 
    T.testGroup 
        "Octets" 
        (
            (TCU.testCasesFromTuple2 Oct.lTestCasesTuple2) ++ 
            [
                tgLoctFix, 
                tgLoctBitField, 
                tgLoctBitFieldTakeEnd, 
                tgOctClearMsbs, 
                tgLoctBitShiftR, 
                tgLoctShiftOctR, 
                tgOctBitMaskL
            ]
        )

tgLoctFix :: T.TestTree
tgLoctFix = 
    T.testGroup 
        "loctFix" 
        [
            HU.testCase  "#1" ( (Oct.loctFix (-2)        []) HU.@?=              []), 
            HU.testCase  "#2" ( (Oct.loctFix (-1)        []) HU.@?=              []), 
            HU.testCase  "#3" ( (Oct.loctFix    0        []) HU.@?=              []), 
            HU.testCase  "#4" ( (Oct.loctFix    1        []) HU.@?=             [0]), 
            HU.testCase  "#5" ( (Oct.loctFix    2        []) HU.@?=          [0, 0]), 
            HU.testCase  "#6" ( (Oct.loctFix (-2)       [1]) HU.@?=              []), 
            HU.testCase  "#7" ( (Oct.loctFix (-1)       [1]) HU.@?=              []), 
            HU.testCase  "#8" ( (Oct.loctFix    0       [1]) HU.@?=              []), 
            HU.testCase  "#9" ( (Oct.loctFix    1       [1]) HU.@?=             [1]), 
            HU.testCase "#10" ( (Oct.loctFix    2       [1]) HU.@?=          [0, 1]), 
            HU.testCase "#11" ( (Oct.loctFix    3       [1]) HU.@?=       [0, 0, 1]), 
            HU.testCase "#12" ( (Oct.loctFix (-2)    [1, 2]) HU.@?=              []), 
            HU.testCase "#13" ( (Oct.loctFix (-1)    [1, 2]) HU.@?=              []), 
            HU.testCase "#14" ( (Oct.loctFix    0    [1, 2]) HU.@?=              []), 
            HU.testCase "#15" ( (Oct.loctFix    1    [1, 2]) HU.@?=             [2]), 
            HU.testCase "#16" ( (Oct.loctFix    2    [1, 2]) HU.@?=          [1, 2]), 
            HU.testCase "#17" ( (Oct.loctFix    3    [1, 2]) HU.@?=       [0, 1, 2]), 
            HU.testCase "#18" ( (Oct.loctFix    4    [1, 2]) HU.@?=    [0, 0, 1, 2]), 
            HU.testCase "#19" ( (Oct.loctFix (-2) [1, 2, 3]) HU.@?=              []), 
            HU.testCase "#20" ( (Oct.loctFix (-1) [1, 2, 3]) HU.@?=              []), 
            HU.testCase "#21" ( (Oct.loctFix    0 [1, 2, 3]) HU.@?=              []), 
            HU.testCase "#22" ( (Oct.loctFix    1 [1, 2, 3]) HU.@?=             [3]), 
            HU.testCase "#23" ( (Oct.loctFix    2 [1, 2, 3]) HU.@?=          [2, 3]), 
            HU.testCase "#24" ( (Oct.loctFix    3 [1, 2, 3]) HU.@?=       [1, 2, 3]), 
            HU.testCase "#25" ( (Oct.loctFix    4 [1, 2, 3]) HU.@?=    [0, 1, 2, 3]), 
            HU.testCase "#26" ( (Oct.loctFix    5 [1, 2, 3]) HU.@?= [0, 0, 1, 2, 3])
        ]

tgLoctBitField :: T.TestTree
tgLoctBitField = 
    T.testGroup 
        "loctBitField" 
        [
            HU.testCase "#a1"  ( (Oct.loctBitField (-2, 0) [0b11111111, 0b11111111, 0b11111111, 0b11111111]) HU.@?= []), 
            HU.testCase "#a2"  ( (Oct.loctBitField (-1, 0) [0b11111111, 0b11111111, 0b11111111, 0b11111111]) HU.@?= []), 
            HU.testCase "#a3"  ( (Oct.loctBitField  (0, 0) [0b11111111, 0b11111111, 0b11111111, 0b11111111]) HU.@?= [0b00000001]), 
            HU.testCase "#a4"  ( (Oct.loctBitField  (1, 0) [0b11111111, 0b11111111, 0b11111111, 0b11111111]) HU.@?= [0b00000011]), 
            HU.testCase "#a5"  ( (Oct.loctBitField  (2, 0) [0b11111111, 0b11111111, 0b11111111, 0b11111111]) HU.@?= [0b00000111]), 
            HU.testCase "#a6"  ( (Oct.loctBitField  (3, 0) [0b11111111, 0b11111111, 0b11111111, 0b11111111]) HU.@?= [0b00001111]), 
            HU.testCase "#a7"  ( (Oct.loctBitField  (4, 0) [0b11111111, 0b11111111, 0b11111111, 0b11111111]) HU.@?= [0b00011111]), 
            HU.testCase "#a8"  ( (Oct.loctBitField  (5, 0) [0b11111111, 0b11111111, 0b11111111, 0b11111111]) HU.@?= [0b00111111]), 
            HU.testCase "#a9"  ( (Oct.loctBitField  (6, 0) [0b11111111, 0b11111111, 0b11111111, 0b11111111]) HU.@?= [0b01111111]), 
            HU.testCase "#a10" ( (Oct.loctBitField  (7, 0) [0b11111111, 0b11111111, 0b11111111, 0b11111111]) HU.@?= [0b11111111]), 
            HU.testCase "#a11" ( (Oct.loctBitField  (8, 0) [0b11111111, 0b11111111, 0b11111111, 0b11111111]) HU.@?= [0b00000001, 0b11111111]), 
            HU.testCase "#a12" ( (Oct.loctBitField  (9, 0) [0b11111111, 0b11111111, 0b11111111, 0b11111111]) HU.@?= [0b00000011, 0b11111111]), 
            HU.testCase "#a13" ( (Oct.loctBitField (10, 0) [0b11111111, 0b11111111, 0b11111111, 0b11111111]) HU.@?= [0b00000111, 0b11111111]), 
            HU.testCase "#a14" ( (Oct.loctBitField (15, 0) [0b11111111, 0b11111111, 0b11111111, 0b11111111]) HU.@?= [0b11111111, 0b11111111]), 
            HU.testCase "#a15" ( (Oct.loctBitField (16, 0) [0b11111111, 0b11111111, 0b11111111, 0b11111111]) HU.@?= [0b00000001, 0b11111111, 0b11111111]), 
            HU.testCase "#a16" ( (Oct.loctBitField (30, 0) [0b11111111, 0b11111111, 0b11111111, 0b11111111]) HU.@?= [0b01111111, 0b11111111, 0b11111111, 0b11111111]), 
            HU.testCase "#a17" ( (Oct.loctBitField (31, 0) [0b11111111, 0b11111111, 0b11111111, 0b11111111]) HU.@?= [0b11111111, 0b11111111, 0b11111111, 0b11111111]), 
            HU.testCase "#a18" ( (Oct.loctBitField (32, 0) [0b11111111, 0b11111111, 0b11111111, 0b11111111]) HU.@?= [0b11111111, 0b11111111, 0b11111111, 0b11111111]), 
            HU.testCase "#b1"  ( (Oct.loctBitField (-2, 0) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= []), 
            HU.testCase "#b2"  ( (Oct.loctBitField (-1, 0) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= []), 
            HU.testCase "#b3"  ( (Oct.loctBitField  (0, 0) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00000000]), 
            HU.testCase "#b4"  ( (Oct.loctBitField  (1, 0) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00000010]), 
            HU.testCase "#b5"  ( (Oct.loctBitField  (2, 0) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00000110]), 
            HU.testCase "#b6"  ( (Oct.loctBitField  (3, 0) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00000110]), 
            HU.testCase "#b7"  ( (Oct.loctBitField  (4, 0) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00010110]), 
            HU.testCase "#b8"  ( (Oct.loctBitField  (5, 0) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00010110]), 
            HU.testCase "#b9"  ( (Oct.loctBitField  (6, 0) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00010110]), 
            HU.testCase "#b10" ( (Oct.loctBitField  (7, 0) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b10010110]), 
            HU.testCase "#b11" ( (Oct.loctBitField  (8, 0) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00000001, 0b10010110]), 
            HU.testCase "#b12" ( (Oct.loctBitField  (9, 0) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00000001, 0b10010110]), 
            HU.testCase "#b13" ( (Oct.loctBitField (10, 0) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00000001, 0b10010110]), 
            HU.testCase "#b14" ( (Oct.loctBitField (15, 0) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b01101001, 0b10010110]), 
            HU.testCase "#b15" ( (Oct.loctBitField (16, 0) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00000001, 0b01101001, 0b10010110]), 
            HU.testCase "#b16" ( (Oct.loctBitField (30, 0) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00010110, 0b01101001, 0b01101001, 0b10010110]), 
            HU.testCase "#b17" ( (Oct.loctBitField (31, 0) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b10010110, 0b01101001, 0b01101001, 0b10010110]), 
            HU.testCase "#b18" ( (Oct.loctBitField (32, 0) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b10010110, 0b01101001, 0b01101001, 0b10010110]), 
            HU.testCase "#c1"  ( (Oct.loctBitField (-2, 1) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= []), 
            HU.testCase "#c2"  ( (Oct.loctBitField (-1, 1) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= []), 
            HU.testCase "#c3"  ( (Oct.loctBitField  (0, 1) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= []), 
            HU.testCase "#c4"  ( (Oct.loctBitField  (1, 1) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00000001]), 
            HU.testCase "#c5"  ( (Oct.loctBitField  (2, 1) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00000011]), 
            HU.testCase "#c6"  ( (Oct.loctBitField  (3, 1) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00000011]), 
            HU.testCase "#c7"  ( (Oct.loctBitField  (4, 1) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00001011]), 
            HU.testCase "#c8"  ( (Oct.loctBitField  (5, 1) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00001011]), 
            HU.testCase "#c9"  ( (Oct.loctBitField  (6, 1) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00001011]), 
            HU.testCase "#c10" ( (Oct.loctBitField  (7, 1) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b01001011]), 
            HU.testCase "#c11" ( (Oct.loctBitField  (8, 1) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b11001011]), 
            HU.testCase "#c12" ( (Oct.loctBitField  (9, 1) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00000000, 0b11001011]), 
            HU.testCase "#c13" ( (Oct.loctBitField (10, 1) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00000000, 0b11001011]), 
            HU.testCase "#c15" ( (Oct.loctBitField (16, 1) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b10110100, 0b11001011]), 
            HU.testCase "#c15" ( (Oct.loctBitField (17, 1) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00000000, 0b10110100, 0b11001011]), 
            HU.testCase "#c16" ( (Oct.loctBitField (30, 1) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00001011, 0b00110100, 0b10110100, 0b11001011]), 
            HU.testCase "#c17" ( (Oct.loctBitField (31, 1) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b01001011, 0b00110100, 0b10110100, 0b11001011]), 
            HU.testCase "#c18" ( (Oct.loctBitField (32, 1) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b01001011, 0b00110100, 0b10110100, 0b11001011])
        ]

tgLoctBitFieldTakeEnd :: T.TestTree
tgLoctBitFieldTakeEnd = 
    T.testGroup 
        "loctBitFieldTakeEnd" 
        [
            HU.testCase "#a1"   ( (Oct.loctBitFieldTakeEnd (-1) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?=                                               []), 
            HU.testCase "#a2"   ( (Oct.loctBitFieldTakeEnd (-2) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?=                                               []), 
            HU.testCase "#a3"   ( (Oct.loctBitFieldTakeEnd    0 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?=                                               []), 
            HU.testCase "#a4"   ( (Oct.loctBitFieldTakeEnd    1 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?=                                     [0b00000000]), 
            HU.testCase "#a5"   ( (Oct.loctBitFieldTakeEnd    2 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?=                                     [0b00000010]), 
            HU.testCase "#a6"   ( (Oct.loctBitFieldTakeEnd    3 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?=                                     [0b00000110]), 
            HU.testCase "#a7"   ( (Oct.loctBitFieldTakeEnd    4 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?=                                     [0b00000110]), 
            HU.testCase "#a8"   ( (Oct.loctBitFieldTakeEnd    5 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?=                                     [0b00010110]), 
            HU.testCase "#a9"   ( (Oct.loctBitFieldTakeEnd    6 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?=                                     [0b00010110]), 
            HU.testCase "#a10"  ( (Oct.loctBitFieldTakeEnd    7 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?=                                     [0b00010110]), 
            HU.testCase "#a11"  ( (Oct.loctBitFieldTakeEnd    8 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?=                                     [0b10010110]), 
            HU.testCase "#a12"  ( (Oct.loctBitFieldTakeEnd    9 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?=                         [0b00000001, 0b10010110]), 
            HU.testCase "#a13"  ( (Oct.loctBitFieldTakeEnd   10 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?=                         [0b00000001, 0b10010110]), 
            HU.testCase "#a14"  ( (Oct.loctBitFieldTakeEnd   11 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?=                         [0b00000001, 0b10010110]), 
            HU.testCase "#a15"  ( (Oct.loctBitFieldTakeEnd   12 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?=                         [0b00001001, 0b10010110]), 
            HU.testCase "#a16"  ( (Oct.loctBitFieldTakeEnd   13 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?=                         [0b00001001, 0b10010110]), 
            HU.testCase "#a17"  ( (Oct.loctBitFieldTakeEnd   14 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?=                         [0b00101001, 0b10010110]), 
            HU.testCase "#a18"  ( (Oct.loctBitFieldTakeEnd   15 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?=                         [0b01101001, 0b10010110]), 
            HU.testCase "#a19"  ( (Oct.loctBitFieldTakeEnd   16 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?=                         [0b01101001, 0b10010110]), 
            HU.testCase "#a20"  ( (Oct.loctBitFieldTakeEnd   17 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?=             [0b00000001, 0b01101001, 0b10010110]), 
            HU.testCase "#a21"  ( (Oct.loctBitFieldTakeEnd   28 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00000110, 0b01101001, 0b01101001, 0b10010110]), 
            HU.testCase "#a22"  ( (Oct.loctBitFieldTakeEnd   29 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00010110, 0b01101001, 0b01101001, 0b10010110]), 
            HU.testCase "#a23"  ( (Oct.loctBitFieldTakeEnd   30 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00010110, 0b01101001, 0b01101001, 0b10010110]), 
            HU.testCase "#a24"  ( (Oct.loctBitFieldTakeEnd   31 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00010110, 0b01101001, 0b01101001, 0b10010110]), 
            HU.testCase "#a25"  ( (Oct.loctBitFieldTakeEnd   32 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b10010110, 0b01101001, 0b01101001, 0b10010110]), 
            HU.testCase "#a26"  ( (Oct.loctBitFieldTakeEnd   33 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b10010110, 0b01101001, 0b01101001, 0b10010110]), 
            HU.testCase "#a27"  ( (Oct.loctBitFieldTakeEnd   34 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b10010110, 0b01101001, 0b01101001, 0b10010110]), 
            HU.testCase "#b1"   ( (Oct.loctBitFieldTakeEnd (-2) [0b00000000, 0b00000000, 0b00000000, 0b00000000]) HU.@?=                                               []), 
            HU.testCase "#b2"   ( (Oct.loctBitFieldTakeEnd (-1) [0b00000000, 0b00000000, 0b00000000, 0b00000000]) HU.@?=                                               []), 
            HU.testCase "#b3"   ( (Oct.loctBitFieldTakeEnd    0 [0b00000000, 0b00000000, 0b00000000, 0b00000000]) HU.@?=                                               []), 
            HU.testCase "#b4"   ( (Oct.loctBitFieldTakeEnd    1 [0b00000000, 0b00000000, 0b00000000, 0b00000000]) HU.@?=                                     [0b00000000]), 
            HU.testCase "#b5"   ( (Oct.loctBitFieldTakeEnd    2 [0b00000000, 0b00000000, 0b00000000, 0b00000000]) HU.@?=                                     [0b00000000]), 
            HU.testCase "#b6"   ( (Oct.loctBitFieldTakeEnd    3 [0b00000000, 0b00000000, 0b00000000, 0b00000000]) HU.@?=                                     [0b00000000]), 
            HU.testCase "#b7"   ( (Oct.loctBitFieldTakeEnd    4 [0b00000000, 0b00000000, 0b00000000, 0b00000000]) HU.@?=                                     [0b00000000]), 
            HU.testCase "#b8"   ( (Oct.loctBitFieldTakeEnd    5 [0b00000000, 0b00000000, 0b00000000, 0b00000000]) HU.@?=                                     [0b00000000]), 
            HU.testCase "#b9"   ( (Oct.loctBitFieldTakeEnd    6 [0b00000000, 0b00000000, 0b00000000, 0b00000000]) HU.@?=                                     [0b00000000]), 
            HU.testCase "#b10"  ( (Oct.loctBitFieldTakeEnd    7 [0b00000000, 0b00000000, 0b00000000, 0b00000000]) HU.@?=                                     [0b00000000]), 
            HU.testCase "#b11"  ( (Oct.loctBitFieldTakeEnd    8 [0b00000000, 0b00000000, 0b00000000, 0b00000000]) HU.@?=                                     [0b00000000]), 
            HU.testCase "#b12"  ( (Oct.loctBitFieldTakeEnd    9 [0b00000000, 0b00000000, 0b00000000, 0b00000000]) HU.@?=                         [0b00000000, 0b00000000]), 
            HU.testCase "#b13"  ( (Oct.loctBitFieldTakeEnd   10 [0b00000000, 0b00000000, 0b00000000, 0b00000000]) HU.@?=                         [0b00000000, 0b00000000]), 
            HU.testCase "#b14"  ( (Oct.loctBitFieldTakeEnd   11 [0b00000000, 0b00000000, 0b00000000, 0b00000000]) HU.@?=                         [0b00000000, 0b00000000]), 
            HU.testCase "#b15"  ( (Oct.loctBitFieldTakeEnd   12 [0b00000000, 0b00000000, 0b00000000, 0b00000000]) HU.@?=                         [0b00000000, 0b00000000]), 
            HU.testCase "#b16"  ( (Oct.loctBitFieldTakeEnd   13 [0b00000000, 0b00000000, 0b00000000, 0b00000000]) HU.@?=                         [0b00000000, 0b00000000]), 
            HU.testCase "#b17"  ( (Oct.loctBitFieldTakeEnd   14 [0b00000000, 0b00000000, 0b00000000, 0b00000000]) HU.@?=                         [0b00000000, 0b00000000]), 
            HU.testCase "#b18"  ( (Oct.loctBitFieldTakeEnd   15 [0b00000000, 0b00000000, 0b00000000, 0b00000000]) HU.@?=                         [0b00000000, 0b00000000]), 
            HU.testCase "#b19"  ( (Oct.loctBitFieldTakeEnd   16 [0b00000000, 0b00000000, 0b00000000, 0b00000000]) HU.@?=                         [0b00000000, 0b00000000]), 
            HU.testCase "#b20"  ( (Oct.loctBitFieldTakeEnd   17 [0b00000000, 0b00000000, 0b00000000, 0b00000000]) HU.@?=             [0b00000000, 0b00000000, 0b00000000]), 
            HU.testCase "#b21"  ( (Oct.loctBitFieldTakeEnd   28 [0b00000000, 0b00000000, 0b00000000, 0b00000000]) HU.@?= [0b00000000, 0b00000000, 0b00000000, 0b00000000]), 
            HU.testCase "#b21"  ( (Oct.loctBitFieldTakeEnd   29 [0b00000000, 0b00000000, 0b00000000, 0b00000000]) HU.@?= [0b00000000, 0b00000000, 0b00000000, 0b00000000]), 
            HU.testCase "#b23"  ( (Oct.loctBitFieldTakeEnd   30 [0b00000000, 0b00000000, 0b00000000, 0b00000000]) HU.@?= [0b00000000, 0b00000000, 0b00000000, 0b00000000]), 
            HU.testCase "#b24"  ( (Oct.loctBitFieldTakeEnd   31 [0b00000000, 0b00000000, 0b00000000, 0b00000000]) HU.@?= [0b00000000, 0b00000000, 0b00000000, 0b00000000]), 
            HU.testCase "#b25"  ( (Oct.loctBitFieldTakeEnd   32 [0b00000000, 0b00000000, 0b00000000, 0b00000000]) HU.@?= [0b00000000, 0b00000000, 0b00000000, 0b00000000]), 
            HU.testCase "#b26"  ( (Oct.loctBitFieldTakeEnd   33 [0b00000000, 0b00000000, 0b00000000, 0b00000000]) HU.@?= [0b00000000, 0b00000000, 0b00000000, 0b00000000]), 
            HU.testCase "#b27"  ( (Oct.loctBitFieldTakeEnd   34 [0b00000000, 0b00000000, 0b00000000, 0b00000000]) HU.@?= [0b00000000, 0b00000000, 0b00000000, 0b00000000])
        ]

tgOctClearMsbs :: T.TestTree
tgOctClearMsbs = 
    T.testGroup 
        "octClearMsbs" 
        [
            HU.testCase "#a1"   ( (Oct.octClearMsbs (-2) 0b10010110) HU.@?= 0b10010110), 
            HU.testCase "#a2"   ( (Oct.octClearMsbs (-1) 0b10010110) HU.@?= 0b10010110), 
            HU.testCase "#a3"   ( (Oct.octClearMsbs    0 0b10010110) HU.@?= 0b10010110), 
            HU.testCase "#a4"   ( (Oct.octClearMsbs    1 0b10010110) HU.@?= 0b00010110), 
            HU.testCase "#a5"   ( (Oct.octClearMsbs    2 0b10010110) HU.@?= 0b00010110), 
            HU.testCase "#a6"   ( (Oct.octClearMsbs    3 0b10010110) HU.@?= 0b00010110), 
            HU.testCase "#a7"   ( (Oct.octClearMsbs    4 0b10010110) HU.@?= 0b00000110), 
            HU.testCase "#a8"   ( (Oct.octClearMsbs    5 0b10010110) HU.@?= 0b00000110), 
            HU.testCase "#a9"   ( (Oct.octClearMsbs    6 0b10010110) HU.@?= 0b00000010), 
            HU.testCase "#a10"  ( (Oct.octClearMsbs    7 0b10010110) HU.@?= 0b00000000), 
            HU.testCase "#a11"  ( (Oct.octClearMsbs    8 0b10010110) HU.@?= 0b00000000), 
            HU.testCase "#a12"  ( (Oct.octClearMsbs    9 0b10010110) HU.@?= 0b00000000), 
            HU.testCase "#b1"   ( (Oct.octClearMsbs (-2) 0b11111111) HU.@?= 0b11111111), 
            HU.testCase "#b2"   ( (Oct.octClearMsbs (-1) 0b11111111) HU.@?= 0b11111111), 
            HU.testCase "#b3"   ( (Oct.octClearMsbs    0 0b11111111) HU.@?= 0b11111111), 
            HU.testCase "#b4"   ( (Oct.octClearMsbs    1 0b11111111) HU.@?= 0b01111111), 
            HU.testCase "#b5"   ( (Oct.octClearMsbs    2 0b11111111) HU.@?= 0b00111111), 
            HU.testCase "#b6"   ( (Oct.octClearMsbs    3 0b11111111) HU.@?= 0b00011111), 
            HU.testCase "#b7"   ( (Oct.octClearMsbs    4 0b11111111) HU.@?= 0b00001111), 
            HU.testCase "#b8"   ( (Oct.octClearMsbs    5 0b11111111) HU.@?= 0b00000111), 
            HU.testCase "#b9"   ( (Oct.octClearMsbs    6 0b11111111) HU.@?= 0b00000011), 
            HU.testCase "#b10"  ( (Oct.octClearMsbs    7 0b11111111) HU.@?= 0b00000001), 
            HU.testCase "#b11"  ( (Oct.octClearMsbs    8 0b11111111) HU.@?= 0b00000000), 
            HU.testCase "#b12"  ( (Oct.octClearMsbs    9 0b11111111) HU.@?= 0b00000000)
        ]

tgLoctBitShiftR :: T.TestTree
tgLoctBitShiftR = 
    T.testGroup 
        "loctBitShiftR" 
        [
            HU.testCase "#e1"   ( (Oct.loctBitShiftR (-2) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b10010110, 0b01101001, 0b01101001, 0b10010110]), 
            HU.testCase "#e2"   ( (Oct.loctBitShiftR (-1) [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b10010110, 0b01101001, 0b01101001, 0b10010110]), 
            HU.testCase "#e3"   ( (Oct.loctBitShiftR    0 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b10010110, 0b01101001, 0b01101001, 0b10010110]), 
            HU.testCase "#e4"   ( (Oct.loctBitShiftR    1 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b01001011, 0b00110100, 0b10110100, 0b11001011]), 
            HU.testCase "#e5"   ( (Oct.loctBitShiftR    2 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00100101, 0b10011010, 0b01011010, 0b01100101]), 
            HU.testCase "#e6"   ( (Oct.loctBitShiftR    3 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00010010, 0b11001101, 0b00101101, 0b00110010]), 
            HU.testCase "#e7"   ( (Oct.loctBitShiftR    4 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00001001, 0b01100110, 0b10010110, 0b10011001]), 
            HU.testCase "#e8"   ( (Oct.loctBitShiftR    5 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00000100, 0b10110011, 0b01001011, 0b01001100]), 
            HU.testCase "#e9"   ( (Oct.loctBitShiftR    6 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00000010, 0b01011001, 0b10100101, 0b10100110]), 
            HU.testCase "#e10"  ( (Oct.loctBitShiftR    7 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00000001, 0b00101100, 0b11010010, 0b11010011]), 
            HU.testCase "#e11"  ( (Oct.loctBitShiftR    8 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00000000, 0b10010110, 0b01101001, 0b01101001]), 
            HU.testCase "#e12"  ( (Oct.loctBitShiftR    9 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00000000, 0b01001011, 0b00110100, 0b10110100]), 
            HU.testCase "#e13"  ( (Oct.loctBitShiftR   10 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00000000, 0b00100101, 0b10011010, 0b01011010]), 
            HU.testCase "#e14"  ( (Oct.loctBitShiftR   11 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00000000, 0b00010010, 0b11001101, 0b00101101]), 
            HU.testCase "#e15"  ( (Oct.loctBitShiftR   28 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00000000, 0b00000000, 0b00000000, 0b00001001]), 
            HU.testCase "#e16"  ( (Oct.loctBitShiftR   29 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00000000, 0b00000000, 0b00000000, 0b00000100]), 
            HU.testCase "#e17"  ( (Oct.loctBitShiftR   30 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00000000, 0b00000000, 0b00000000, 0b00000010]), 
            HU.testCase "#e18"  ( (Oct.loctBitShiftR   31 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00000000, 0b00000000, 0b00000000, 0b00000001]), 
            HU.testCase "#e19"  ( (Oct.loctBitShiftR   32 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) HU.@?= [0b00000000, 0b00000000, 0b00000000, 0b00000000])
        ]

tgLoctShiftOctR :: T.TestTree
tgLoctShiftOctR = 
    T.testGroup 
        "loctShiftOctR" 
        [
            HU.testCase  "#1"  ( (Oct.loctShiftOctR (-2) [])        HU.@?=       []), 
            HU.testCase  "#2"  ( (Oct.loctShiftOctR (-1) [])        HU.@?=       []), 
            HU.testCase  "#3"  ( (Oct.loctShiftOctR    0 [])        HU.@?=       []), 
            HU.testCase  "#4"  ( (Oct.loctShiftOctR    1 [])        HU.@?=       []), 
            HU.testCase  "#5"  ( (Oct.loctShiftOctR    2 [])        HU.@?=       []), 
            HU.testCase  "#6"  ( (Oct.loctShiftOctR (-2) [1])       HU.@?=       [1]), 
            HU.testCase  "#7"  ( (Oct.loctShiftOctR (-1) [1])       HU.@?=       [1]), 
            HU.testCase  "#8"  ( (Oct.loctShiftOctR    0 [1])       HU.@?=       [1]), 
            HU.testCase  "#9"  ( (Oct.loctShiftOctR    1 [1])       HU.@?=       [0]), 
            HU.testCase "#10"  ( (Oct.loctShiftOctR    2 [1])       HU.@?=       [0]), 
            HU.testCase "#11"  ( (Oct.loctShiftOctR    3 [1])       HU.@?=       [0]), 
            HU.testCase "#12"  ( (Oct.loctShiftOctR (-2) [1, 2])    HU.@?=    [1, 2]), 
            HU.testCase "#13"  ( (Oct.loctShiftOctR (-1) [1, 2])    HU.@?=    [1, 2]), 
            HU.testCase "#14"  ( (Oct.loctShiftOctR    0 [1, 2])    HU.@?=    [1, 2]), 
            HU.testCase "#15"  ( (Oct.loctShiftOctR    1 [1, 2])    HU.@?=    [0, 1]), 
            HU.testCase "#16"  ( (Oct.loctShiftOctR    2 [1, 2])    HU.@?=    [0, 0]), 
            HU.testCase "#17"  ( (Oct.loctShiftOctR    3 [1, 2])    HU.@?=    [0, 0]), 
            HU.testCase "#18"  ( (Oct.loctShiftOctR    4 [1, 2])    HU.@?=    [0, 0]), 
            HU.testCase "#19"  ( (Oct.loctShiftOctR (-2) [1, 2, 3]) HU.@?= [1, 2, 3]), 
            HU.testCase "#20"  ( (Oct.loctShiftOctR (-1) [1, 2, 3]) HU.@?= [1, 2, 3]), 
            HU.testCase "#21"  ( (Oct.loctShiftOctR    0 [1, 2, 3]) HU.@?= [1, 2, 3]), 
            HU.testCase "#22"  ( (Oct.loctShiftOctR    1 [1, 2, 3]) HU.@?= [0, 1, 2]), 
            HU.testCase "#23"  ( (Oct.loctShiftOctR    2 [1, 2, 3]) HU.@?= [0, 0, 1]), 
            HU.testCase "#24"  ( (Oct.loctShiftOctR    3 [1, 2, 3]) HU.@?= [0, 0, 0]), 
            HU.testCase "#25"  ( (Oct.loctShiftOctR    4 [1, 2, 3]) HU.@?= [0, 0, 0]), 
            HU.testCase "#26"  ( (Oct.loctShiftOctR    5 [1, 2, 3]) HU.@?= [0, 0, 0])
        ]

tgOctBitMaskL :: T.TestTree
tgOctBitMaskL = 
    T.testGroup 
        "octBitMaskL" 
        [
            HU.testCase  "#1"  ( (Oct.octBitMaskL (-2)) HU.@?= 0b00000000), 
            HU.testCase  "#2"  ( (Oct.octBitMaskL (-1)) HU.@?= 0b00000000), 
            HU.testCase  "#3"  ( (Oct.octBitMaskL    0) HU.@?= 0b00000000), 
            HU.testCase  "#4"  ( (Oct.octBitMaskL    1) HU.@?= 0b10000000), 
            HU.testCase  "#5"  ( (Oct.octBitMaskL    2) HU.@?= 0b11000000), 
            HU.testCase  "#6"  ( (Oct.octBitMaskL    3) HU.@?= 0b11100000), 
            HU.testCase  "#7"  ( (Oct.octBitMaskL    4) HU.@?= 0b11110000), 
            HU.testCase  "#8"  ( (Oct.octBitMaskL    5) HU.@?= 0b11111000), 
            HU.testCase  "#9"  ( (Oct.octBitMaskL    6) HU.@?= 0b11111100), 
            HU.testCase "#10"  ( (Oct.octBitMaskL    7) HU.@?= 0b11111110), 
            HU.testCase "#11"  ( (Oct.octBitMaskL    8) HU.@?= 0b11111111), 
            HU.testCase "#12"  ( (Oct.octBitMaskL    9) HU.@?= 0b11111111), 
            HU.testCase "#13"  ( (Oct.octBitMaskL   10) HU.@?= 0b11111111)
        ]
