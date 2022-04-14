{-|
Description : test.
Copyright   : (c) Jörg K.-H. W. Brüggmann, 2021-2022
License     : proprietary, to be dual licensed
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX
-}

import qualified Test.Tasty as T

import qualified SpecTypedCodeOctets
import qualified SpecConvertibleCode
import qualified SpecOctetable
import qualified SpecOctets
import qualified SpecCode
import qualified SpecSafer
import qualified SpecList

main :: IO ()
main = 
    T.defaultMain tests

tests :: T.TestTree
tests = 
    T.testGroup 
        "Tests" 
        [
            SpecTypedCodeOctets.testGroup,
            SpecConvertibleCode.testGroup,
            SpecOctetable.testGroup,
            SpecCode.testGroup,
            SpecSafer.testGroup,
            SpecList.testGroup,
            SpecOctets.testGroup
        ]
