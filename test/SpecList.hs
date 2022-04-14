{-|
Description : test.
Copyright   : (c) Jörg K.-H. W. Brüggmann, 2021-2022
License     : proprietary, to be dual licensed
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE BinaryLiterals #-}

module SpecList
    (
        testGroup
    ) where

import qualified Test.Tasty as T
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit as HU

import qualified List as Lst

testGroup :: T.TestTree
testGroup =
    T.testGroup
        "List"
        [
            tgUnitareAllOrdUnique,
            tgUnitisMinAvailable,
            tgUnittakeEnd,
            tgUnitdropEnd,
            tgUnitshiftR
        ]

tgUnitareAllOrdUnique :: T.TestTree
tgUnitareAllOrdUnique =
    T.testGroup
        "areAllOrdUnique"
        [
        {-  * validated ✅
                * justification:
                    * completeness: ✅ (the complete set of possible test values/data is used in unit tests, or values for all possible equivalence classes are used or ramdom sample values/data from complete set of possible values/data is used in case of high-volume cases)
                    * independence: ✅ (test method is independent from library functions that are under test, or dependent on tested functions)
                    * boundaries  : ✅ (corner cases and boundaries are tested by unit tests)
                    * conform doc.: ✅ (all tests are conform to documentation, e.g. source code comments by haddock) -}
            HU.testCase  "#1" $ Lst.areAllOrdUnique ([] :: [Int]) HU.@?= True,
            HU.testCase  "#2" $ Lst.areAllOrdUnique ([6] :: [Int]) HU.@?= True,
            HU.testCase  "#3" $ Lst.areAllOrdUnique ([6, 7] :: [Int]) HU.@?= True,
            HU.testCase  "#4" $ Lst.areAllOrdUnique ([7, 6] :: [Int]) HU.@?= True,
            HU.testCase  "#5" $ Lst.areAllOrdUnique ([7, 7] :: [Int]) HU.@?= False,
            HU.testCase  "#6" $ Lst.areAllOrdUnique ([7, 2, 9] :: [Int]) HU.@?= True,
            HU.testCase  "#7" $ Lst.areAllOrdUnique ([7, 9, 2] :: [Int]) HU.@?= True,
            HU.testCase  "#8" $ Lst.areAllOrdUnique ([9, 7, 2] :: [Int]) HU.@?= True,
            HU.testCase  "#9" $ Lst.areAllOrdUnique ([9, 2, 7] :: [Int]) HU.@?= True,
            HU.testCase "#10" $ Lst.areAllOrdUnique ([2, 9, 7] :: [Int]) HU.@?= True,
            HU.testCase "#11" $ Lst.areAllOrdUnique ([2, 7, 9] :: [Int]) HU.@?= True,
            HU.testCase "#12" $ Lst.areAllOrdUnique ([2, 7, 7] :: [Int]) HU.@?= False,
            HU.testCase "#13" $ Lst.areAllOrdUnique ([7, 2, 7] :: [Int]) HU.@?= False,
            HU.testCase "#14" $ Lst.areAllOrdUnique ([7, 7, 2] :: [Int]) HU.@?= False,
            HU.testCase "#14" $ Lst.areAllOrdUnique ([7, 7, 7] :: [Int]) HU.@?= False,
            HU.testCase "#15" $ Lst.areAllOrdUnique ([-3, 2, 5, 8] :: [Int]) HU.@?= True,
            HU.testCase "#16" $ Lst.areAllOrdUnique ([-3, 2, 8, 5] :: [Int]) HU.@?= True,
            HU.testCase "#17" $ Lst.areAllOrdUnique ([-3, 5, 8, 2] :: [Int]) HU.@?= True,
            HU.testCase "#18" $ Lst.areAllOrdUnique ([-3, 5, 2, 8] :: [Int]) HU.@?= True,
            HU.testCase "#19" $ Lst.areAllOrdUnique ([-3, 8, 2, 5] :: [Int]) HU.@?= True,
            HU.testCase "#20" $ Lst.areAllOrdUnique ([-3, 8, 5, 2] :: [Int]) HU.@?= True,
            HU.testCase "#21" $ Lst.areAllOrdUnique ([2, -3, 5, 8] :: [Int]) HU.@?= True,
            HU.testCase "#22" $ Lst.areAllOrdUnique ([2, -3, 8, 5] :: [Int]) HU.@?= True,
            HU.testCase "#23" $ Lst.areAllOrdUnique ([2, 5, 8, -3] :: [Int]) HU.@?= True,
            HU.testCase "#24" $ Lst.areAllOrdUnique ([2, 5, -3, 8] :: [Int]) HU.@?= True,
            HU.testCase "#25" $ Lst.areAllOrdUnique ([2, 8, -3, 5] :: [Int]) HU.@?= True,
            HU.testCase "#26" $ Lst.areAllOrdUnique ([2, 8, 5, -3] :: [Int]) HU.@?= True,
            HU.testCase "#27" $ Lst.areAllOrdUnique ([5, -3, 2, 8] :: [Int]) HU.@?= True,
            HU.testCase "#28" $ Lst.areAllOrdUnique ([5, -3, 8, 2] :: [Int]) HU.@?= True,
            HU.testCase "#29" $ Lst.areAllOrdUnique ([5, 2, -3, 8] :: [Int]) HU.@?= True,
            HU.testCase "#30" $ Lst.areAllOrdUnique ([5, 2, 8, -3] :: [Int]) HU.@?= True,
            HU.testCase "#31" $ Lst.areAllOrdUnique ([5, 8, 2, -3] :: [Int]) HU.@?= True,
            HU.testCase "#32" $ Lst.areAllOrdUnique ([5, 8, -3, 2] :: [Int]) HU.@?= True,
            HU.testCase "#33" $ Lst.areAllOrdUnique ([8, -3, 5, 2] :: [Int]) HU.@?= True,
            HU.testCase "#34" $ Lst.areAllOrdUnique ([8, -3, 2, 5] :: [Int]) HU.@?= True,
            HU.testCase "#35" $ Lst.areAllOrdUnique ([8, 2, -3, 5] :: [Int]) HU.@?= True,
            HU.testCase "#36" $ Lst.areAllOrdUnique ([8, 2, 5, -3] :: [Int]) HU.@?= True,
            HU.testCase "#37" $ Lst.areAllOrdUnique ([8, 5, 2, -3] :: [Int]) HU.@?= True,
            HU.testCase "#38" $ Lst.areAllOrdUnique ([8, 5, -3, 2] :: [Int]) HU.@?= True,
            HU.testCase "#39" $ Lst.areAllOrdUnique ([-3, 5, 5, 8] :: [Int]) HU.@?= False,
            HU.testCase "#40" $ Lst.areAllOrdUnique ([-3, 5, 8, 5] :: [Int]) HU.@?= False,
            HU.testCase "#41" $ Lst.areAllOrdUnique ([-3, 8, 5, 5] :: [Int]) HU.@?= False,
            HU.testCase "#42" $ Lst.areAllOrdUnique ([5, -3, 8, 5] :: [Int]) HU.@?= False,
            HU.testCase "#43" $ Lst.areAllOrdUnique ([5, -3, 5, 8] :: [Int]) HU.@?= False,
            HU.testCase "#44" $ Lst.areAllOrdUnique ([5, 5, -3, 8] :: [Int]) HU.@?= False,
            HU.testCase "#45" $ Lst.areAllOrdUnique ([5, 5, 8, -3] :: [Int]) HU.@?= False,
            HU.testCase "#46" $ Lst.areAllOrdUnique ([5, 8, 5, -3] :: [Int]) HU.@?= False,
            HU.testCase "#47" $ Lst.areAllOrdUnique ([5, 8, -3, 5] :: [Int]) HU.@?= False,
            HU.testCase "#48" $ Lst.areAllOrdUnique ([8, 5, -3, 5] :: [Int]) HU.@?= False,
            HU.testCase "#49" $ Lst.areAllOrdUnique ([8, 5, 5, -3] :: [Int]) HU.@?= False,
            HU.testCase "#50" $ Lst.areAllOrdUnique ([8, -3, 5, 5] :: [Int]) HU.@?= False,
            HU.testCase "#51" $ Lst.areAllOrdUnique ([-3, 5, 5, 5] :: [Int]) HU.@?= False,
            HU.testCase "#52" $ Lst.areAllOrdUnique ([5, -3, 5, 5] :: [Int]) HU.@?= False,
            HU.testCase "#53" $ Lst.areAllOrdUnique ([5, 5, -3, 5] :: [Int]) HU.@?= False,
            HU.testCase "#54" $ Lst.areAllOrdUnique ([5, 5, 5, -3] :: [Int]) HU.@?= False,
            HU.testCase "#55" $ Lst.areAllOrdUnique ([5, 5, 5, 5] :: [Int]) HU.@?= False
        ]

tgUnitisMinAvailable :: T.TestTree
tgUnitisMinAvailable =
    T.testGroup
        "isMinAvailable"
        [
            HU.testCase  "#1" $ Lst.isMinAvailable (-2) ([1,2,3]::[Integer]) HU.@?= True,
            HU.testCase  "#2" $ Lst.isMinAvailable (-1) ([1,2,3]::[Integer]) HU.@?= True,
            HU.testCase  "#3" $ Lst.isMinAvailable    0 ([1,2,3]::[Integer]) HU.@?= True,
            HU.testCase  "#4" $ Lst.isMinAvailable    1 ([1,2,3]::[Integer]) HU.@?= True,
            HU.testCase  "#5" $ Lst.isMinAvailable    2 ([1,2,3]::[Integer]) HU.@?= True,
            HU.testCase  "#6" $ Lst.isMinAvailable    3 ([1,2,3]::[Integer]) HU.@?= True,
            HU.testCase  "#7" $ Lst.isMinAvailable    4 ([1,2,3]::[Integer]) HU.@?= False,
            HU.testCase  "#8" $ Lst.isMinAvailable    5 ([1,2,3]::[Integer]) HU.@?= False,
            HU.testCase  "#9" $ Lst.isMinAvailable (-2) ([1,2]::[Integer])   HU.@?= True,
            HU.testCase "#10" $ Lst.isMinAvailable (-1) ([1,2]::[Integer])   HU.@?= True,
            HU.testCase "#11" $ Lst.isMinAvailable    0 ([1,2]::[Integer])   HU.@?= True,
            HU.testCase "#12" $ Lst.isMinAvailable    1 ([1,2]::[Integer])   HU.@?= True,
            HU.testCase "#13" $ Lst.isMinAvailable    2 ([1,2]::[Integer])   HU.@?= True,
            HU.testCase "#14" $ Lst.isMinAvailable    3 ([1,2]::[Integer])   HU.@?= False,
            HU.testCase "#15" $ Lst.isMinAvailable    4 ([1,2]::[Integer])   HU.@?= False,
            HU.testCase "#16" $ Lst.isMinAvailable (-2) ([1]::[Integer])     HU.@?= True,
            HU.testCase "#17" $ Lst.isMinAvailable (-1) ([1]::[Integer])     HU.@?= True,
            HU.testCase "#18" $ Lst.isMinAvailable    0 ([1]::[Integer])     HU.@?= True,
            HU.testCase "#19" $ Lst.isMinAvailable    1 ([1]::[Integer])     HU.@?= True,
            HU.testCase "#20" $ Lst.isMinAvailable    2 ([1]::[Integer])     HU.@?= False,
            HU.testCase "#21" $ Lst.isMinAvailable    3 ([1]::[Integer])     HU.@?= False,
            HU.testCase "#22" $ Lst.isMinAvailable (-2) ([]::[Integer])      HU.@?= True,
            HU.testCase "#23" $ Lst.isMinAvailable (-1) ([]::[Integer])      HU.@?= True,
            HU.testCase "#24" $ Lst.isMinAvailable    0 ([]::[Integer])      HU.@?= True,
            HU.testCase "#25" $ Lst.isMinAvailable    1 ([]::[Integer])      HU.@?= False,
            HU.testCase "#26" $ Lst.isMinAvailable    2 ([]::[Integer])      HU.@?= False
        ]

tgUnittakeEnd :: T.TestTree
tgUnittakeEnd =
    T.testGroup
        "takeEnd"
        [
            HU.testCase  "#1" $ Lst.takeEnd (-2) ([1,2,3]::[Integer]) HU.@?= (     []::[Integer]),
            HU.testCase  "#2" $ Lst.takeEnd (-1) ([1,2,3]::[Integer]) HU.@?= (     []::[Integer]),
            HU.testCase  "#3" $ Lst.takeEnd    0 ([1,2,3]::[Integer]) HU.@?= (     []::[Integer]),
            HU.testCase  "#4" $ Lst.takeEnd    1 ([1,2,3]::[Integer]) HU.@?= (    [3]::[Integer]),
            HU.testCase  "#5" $ Lst.takeEnd    2 ([1,2,3]::[Integer]) HU.@?= (  [2,3]::[Integer]),
            HU.testCase  "#6" $ Lst.takeEnd    3 ([1,2,3]::[Integer]) HU.@?= ([1,2,3]::[Integer]),
            HU.testCase  "#7" $ Lst.takeEnd    4 ([1,2,3]::[Integer]) HU.@?= ([1,2,3]::[Integer]),
            HU.testCase  "#8" $ Lst.takeEnd    5 ([1,2,3]::[Integer]) HU.@?= ([1,2,3]::[Integer]),
            HU.testCase  "#9" $ Lst.takeEnd (-2) ([1,2]::[Integer])   HU.@?= (     []::[Integer]),
            HU.testCase "#10" $ Lst.takeEnd (-1) ([1,2]::[Integer])   HU.@?= (     []::[Integer]),
            HU.testCase "#11" $ Lst.takeEnd    0 ([1,2]::[Integer])   HU.@?= (     []::[Integer]),
            HU.testCase "#12" $ Lst.takeEnd    1 ([1,2]::[Integer])   HU.@?= (    [2]::[Integer]),
            HU.testCase "#13" $ Lst.takeEnd    2 ([1,2]::[Integer])   HU.@?= (  [1,2]::[Integer]),
            HU.testCase "#14" $ Lst.takeEnd    3 ([1,2]::[Integer])   HU.@?= (  [1,2]::[Integer]),
            HU.testCase "#15" $ Lst.takeEnd    4 ([1,2]::[Integer])   HU.@?= (  [1,2]::[Integer]),
            HU.testCase "#16" $ Lst.takeEnd (-2) ([1]::[Integer])     HU.@?= (     []::[Integer]),
            HU.testCase "#17" $ Lst.takeEnd (-1) ([1]::[Integer])     HU.@?= (     []::[Integer]),
            HU.testCase "#18" $ Lst.takeEnd    0 ([1]::[Integer])     HU.@?= (     []::[Integer]),
            HU.testCase "#19" $ Lst.takeEnd    1 ([1]::[Integer])     HU.@?= (    [1]::[Integer]),
            HU.testCase "#20" $ Lst.takeEnd    2 ([1]::[Integer])     HU.@?= (    [1]::[Integer]),
            HU.testCase "#21" $ Lst.takeEnd    3 ([1]::[Integer])     HU.@?= (    [1]::[Integer]),
            HU.testCase "#22" $ Lst.takeEnd (-2) ([]::[Integer])      HU.@?= (    []::[Integer]),
            HU.testCase "#23" $ Lst.takeEnd (-1) ([]::[Integer])      HU.@?= (    []::[Integer]),
            HU.testCase "#24" $ Lst.takeEnd    0 ([]::[Integer])      HU.@?= (    []::[Integer]),
            HU.testCase "#25" $ Lst.takeEnd    1 ([]::[Integer])      HU.@?= (    []::[Integer]),
            HU.testCase "#26" $ Lst.takeEnd    2 ([]::[Integer])      HU.@?= (    []::[Integer])
        ]

tgUnitdropEnd :: T.TestTree
tgUnitdropEnd =
    T.testGroup
        "dropEnd"
        [
            HU.testCase  "#1" $ Lst.dropEnd (-2) ([1,2,3]::[Integer]) HU.@?= ([1,2,3]::[Integer]),
            HU.testCase  "#2" $ Lst.dropEnd (-1) ([1,2,3]::[Integer]) HU.@?= ([1,2,3]::[Integer]),
            HU.testCase  "#3" $ Lst.dropEnd    0 ([1,2,3]::[Integer]) HU.@?= ([1,2,3]::[Integer]),
            HU.testCase  "#4" $ Lst.dropEnd    1 ([1,2,3]::[Integer]) HU.@?= ([1,2]::[Integer]),
            HU.testCase  "#5" $ Lst.dropEnd    2 ([1,2,3]::[Integer]) HU.@?= ([1]::[Integer]),
            HU.testCase  "#6" $ Lst.dropEnd    3 ([1,2,3]::[Integer]) HU.@?= ([]::[Integer]),
            HU.testCase  "#7" $ Lst.dropEnd    4 ([1,2,3]::[Integer]) HU.@?= ([]::[Integer]),
            HU.testCase  "#8" $ Lst.dropEnd    5 ([1,2,3]::[Integer]) HU.@?= ([]::[Integer]),
            HU.testCase  "#9" $ Lst.dropEnd (-2) ([1,2]::[Integer])   HU.@?= ([1,2]::[Integer]),
            HU.testCase "#10" $ Lst.dropEnd (-1) ([1,2]::[Integer])   HU.@?= ([1,2]::[Integer]),
            HU.testCase "#11" $ Lst.dropEnd    0 ([1,2]::[Integer])   HU.@?= ([1,2]::[Integer]),
            HU.testCase "#12" $ Lst.dropEnd    1 ([1,2]::[Integer])   HU.@?= ([1]::[Integer]),
            HU.testCase "#13" $ Lst.dropEnd    2 ([1,2]::[Integer])   HU.@?= ([]::[Integer]),
            HU.testCase "#14" $ Lst.dropEnd    3 ([1,2]::[Integer])   HU.@?= ([]::[Integer]),
            HU.testCase "#15" $ Lst.dropEnd    4 ([1,2]::[Integer])   HU.@?= ([]::[Integer]),
            HU.testCase "#16" $ Lst.dropEnd (-2) ([1]::[Integer])     HU.@?= ([1]::[Integer]),
            HU.testCase "#17" $ Lst.dropEnd (-1) ([1]::[Integer])     HU.@?= ([1]::[Integer]),
            HU.testCase "#18" $ Lst.dropEnd    0 ([1]::[Integer])     HU.@?= ([1]::[Integer]),
            HU.testCase "#19" $ Lst.dropEnd    1 ([1]::[Integer])     HU.@?= ([]::[Integer]),
            HU.testCase "#20" $ Lst.dropEnd    2 ([1]::[Integer])     HU.@?= ([]::[Integer]),
            HU.testCase "#21" $ Lst.dropEnd    3 ([1]::[Integer])     HU.@?= ([]::[Integer]),
            HU.testCase "#22" $ Lst.dropEnd (-2) ([]::[Integer])      HU.@?= ([]::[Integer]),
            HU.testCase "#23" $ Lst.dropEnd (-1) ([]::[Integer])      HU.@?= ([]::[Integer]),
            HU.testCase "#24" $ Lst.dropEnd    0 ([]::[Integer])      HU.@?= ([]::[Integer]),
            HU.testCase "#25" $ Lst.dropEnd    1 ([]::[Integer])      HU.@?= ([]::[Integer]),
            HU.testCase "#26" $ Lst.dropEnd    2 ([]::[Integer])      HU.@?= ([]::[Integer])
        ]

tgUnitshiftR :: T.TestTree
tgUnitshiftR =
    T.testGroup
        "shiftR"
        [
            HU.testCase  "#1" $ Lst.shiftR 0 (-2) ([1,2,3]::[Integer]) HU.@?= ([1,2,3]::[Integer]),
            HU.testCase  "#2" $ Lst.shiftR 0 (-1) ([1,2,3]::[Integer]) HU.@?= ([1,2,3]::[Integer]),
            HU.testCase  "#3" $ Lst.shiftR 0    0 ([1,2,3]::[Integer]) HU.@?= ([1,2,3]::[Integer]),
            HU.testCase  "#4" $ Lst.shiftR 0    1 ([1,2,3]::[Integer]) HU.@?= ([0,1,2]::[Integer]),
            HU.testCase  "#5" $ Lst.shiftR 0    2 ([1,2,3]::[Integer]) HU.@?= ([0,0,1]::[Integer]),
            HU.testCase  "#6" $ Lst.shiftR 0    3 ([1,2,3]::[Integer]) HU.@?= ([0,0,0]::[Integer]),
            HU.testCase  "#7" $ Lst.shiftR 0    4 ([1,2,3]::[Integer]) HU.@?= ([0,0,0]::[Integer]),
            HU.testCase  "#8" $ Lst.shiftR 0    5 ([1,2,3]::[Integer]) HU.@?= ([0,0,0]::[Integer]),
            HU.testCase  "#9" $ Lst.shiftR 0 (-2) ([1,2]::[Integer])   HU.@?= ([1,2]::[Integer]),
            HU.testCase "#10" $ Lst.shiftR 0 (-1) ([1,2]::[Integer])   HU.@?= ([1,2]::[Integer]),
            HU.testCase "#11" $ Lst.shiftR 0    0 ([1,2]::[Integer])   HU.@?= ([1,2]::[Integer]),
            HU.testCase "#12" $ Lst.shiftR 0    1 ([1,2]::[Integer])   HU.@?= ([0,1]::[Integer]),
            HU.testCase "#13" $ Lst.shiftR 0    2 ([1,2]::[Integer])   HU.@?= ([0,0]::[Integer]),
            HU.testCase "#14" $ Lst.shiftR 0    3 ([1,2]::[Integer])   HU.@?= ([0,0]::[Integer]),
            HU.testCase "#15" $ Lst.shiftR 0    4 ([1,2]::[Integer])   HU.@?= ([0,0]::[Integer]),
            HU.testCase "#16" $ Lst.shiftR 0 (-2) ([1]::[Integer])     HU.@?= ([1]::[Integer]),
            HU.testCase "#17" $ Lst.shiftR 0 (-1) ([1]::[Integer])     HU.@?= ([1]::[Integer]),
            HU.testCase "#18" $ Lst.shiftR 0    0 ([1]::[Integer])     HU.@?= ([1]::[Integer]),
            HU.testCase "#19" $ Lst.shiftR 0    1 ([1]::[Integer])     HU.@?= ([0]::[Integer]),
            HU.testCase "#20" $ Lst.shiftR 0    2 ([1]::[Integer])     HU.@?= ([0]::[Integer]),
            HU.testCase "#21" $ Lst.shiftR 0    3 ([1]::[Integer])     HU.@?= ([0]::[Integer]),
            HU.testCase "#22" $ Lst.shiftR 0 (-2) ([]::[Integer])      HU.@?= ([]::[Integer]),
            HU.testCase "#23" $ Lst.shiftR 0 (-1) ([]::[Integer])      HU.@?= ([]::[Integer]),
            HU.testCase "#24" $ Lst.shiftR 0    0 ([]::[Integer])      HU.@?= ([]::[Integer]),
            HU.testCase "#25" $ Lst.shiftR 0    1 ([]::[Integer])      HU.@?= ([]::[Integer]),
            HU.testCase "#26" $ Lst.shiftR 0    2 ([]::[Integer])      HU.@?= ([]::[Integer])
        ]
