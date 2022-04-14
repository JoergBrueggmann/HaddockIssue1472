{-|
Description : provides functions to unfold tests that have been wrapped by data types as defined in module TestCaseWrap.
Copyright   : (c) Jörg K.-H. W. Brüggmann, 2021-2022
License     : proprietary, to be dual licensed
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX
-}

module TestCaseUnwrap
    (
        testCaseFromTestCaseWrap, 
        testCaseFromTestCaseWrapList, 
        testCasesFromTuple2, 
        testCasesFromTuple3, 
        testCasesFromTuple4
    ) where

import qualified TestCaseWrap as TCW

import qualified Test.Tasty as T
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit as HU

testCaseFromTestCaseWrap :: (Eq a, Show a) => (TCW.TestCaseWrap a) -> T.TestTree 
testCaseFromTestCaseWrap (TCW.TestAssertion sName xActual xExpected) = HU.testCase  sName (xActual HU.@?= xExpected)

testCaseFromTestCaseWrapList :: (Eq a, Show a) => [TCW.TestCaseWrap a] -> [T.TestTree]
testCaseFromTestCaseWrapList ltcwA = fmap testCaseFromTestCaseWrap ltcwA

testCasesFromTuple2 :: (Eq a, Show a, Eq b, Show b) => ([TCW.TestCaseWrap a], [TCW.TestCaseWrap b]) -> [T.TestTree]
testCasesFromTuple2 (ltcwA, ltcwB) = (testCaseFromTestCaseWrapList ltcwA) ++ (testCaseFromTestCaseWrapList ltcwB)

testCasesFromTuple3 :: (Eq a, Show a, Eq b, Show b, Eq c, Show c) => ([TCW.TestCaseWrap a], [TCW.TestCaseWrap b], [TCW.TestCaseWrap c]) -> [T.TestTree]
testCasesFromTuple3 (ltcwA, ltcwB, ltcwC) = (testCaseFromTestCaseWrapList ltcwA) ++ (testCaseFromTestCaseWrapList ltcwB) ++ (testCaseFromTestCaseWrapList ltcwC)

testCasesFromTuple4 :: (Eq a, Show a, Eq b, Show b, Eq c, Show c, Eq d, Show d) => ([TCW.TestCaseWrap a], [TCW.TestCaseWrap b], [TCW.TestCaseWrap c], [TCW.TestCaseWrap d]) -> [T.TestTree]
testCasesFromTuple4 (ltcwA, ltcwB, ltcwC, ltcwD) = (testCaseFromTestCaseWrapList ltcwA) ++ (testCaseFromTestCaseWrapList ltcwB) ++ (testCaseFromTestCaseWrapList ltcwC) ++ (testCaseFromTestCaseWrapList ltcwD)
