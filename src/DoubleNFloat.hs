{-|
Description : provides functions to convert Float and Double to Word32 and Word64.
Copyright   : (c) Jörg K.-H. W. Brüggmann, 2021-2022
License     : proprietary, to be dual licensed
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE FlexibleContexts #-}
 
module DoubleNFloat
    (
        wordToFloat, 
        floatToWord, 
        wordToDouble, 
        doubleToWord
    ) where

import Data.Word (Word32, Word64)
import Data.Array.Unsafe (castSTUArray)
import Data.Array.ST (newArray, readArray, MArray, STUArray)
import GHC.ST (runST, ST)

wordToFloat :: Word32 -> Float
wordToFloat x = runST (cast x)

floatToWord :: Float -> Word32
floatToWord x = runST (cast x)

wordToDouble :: Word64 -> Double
wordToDouble x = runST (cast x)

doubleToWord :: Double -> Word64
doubleToWord x = runST (cast x)

{-# INLINE cast #-}
cast :: (MArray (STUArray s) a (ST s),
         MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0
