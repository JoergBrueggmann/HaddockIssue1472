{-|
Description : provides functions to wrap test definitions that can be unfolded by functions in module TestCaseUnwrap.
Copyright   : (c) Jörg K.-H. W. Brüggmann, 2021-2022
License     : proprietary, to be dual licensed
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX
-}

module TestCaseWrap
    (
        TestCaseWrap(..)
    ) where

{-| wraps a test case

* prefix: tcw, TCW
* for an assertion
  * to hold/evaluate the test case name
  * to hold/evaluate the actual value of test case
  * to hold/evaluate the expected value of test case
-}
data TestCaseWrap a = 
    TestAssertion {
        -- | name of the test case, needed to reference and find the test case if failed
        rsName :: String, 
        -- | function and actual value evaluated by test case, respectively
        rxActual :: a, 
        -- | expected value evaluated by test case, respectively
        rxExpected :: a }
