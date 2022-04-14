{-|
Description : provides functions to convert Float and Double to Word32 and Word64.
Copyright   : (c) Jörg K.-H. W. Brüggmann, 2021-2022
License     : proprietary, to be dual licensed
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}

module Randomisable (
        Randomisable(..), 
        Rnd.Random, 
        Rnd.StdGen, 
        randomSelectS, 
        randomSelectOneS, 
        randomReplaceOneS, 
        randomReplaceSomeS, 
        randomValueS, 
        randomListS, 
        randomExtendListS, 
        randomiseValueProbablyS, 
        randomiseListProbablyS, 
        getGenS, 
        splitGenS, 
        tplSplitGenS2, 
        tplSplitGenS3, 
        tplSplitGenS4, 
        tplSplitGenS5, 
        tplSplitGenS6, 
        tplSplitGenS7, 
        tplSplitGenS8, 
        tplSplitGenS9, 
        lPrngSplit, 
        splitPrng, 
        tplPrngSplit2, 
        tplPrngSplit3, 
        tplPrngSplit4, 
        tplPrngSplit5, 
        tplPrngSplit6, 
        tplPrngSplit7, 
        tplPrngSplit8, 
        tplPrngSplit9, 
        Rnd.newStdGen
    ) where

import qualified System.Random as Rnd
import qualified Control.Monad.State as S

import qualified Safer as Sfr
import qualified List as Lst

{-import Debug.Trace
trace :: String -> a -> a
trace _ = id-}

randomSelectS :: Double -> [a] -> S.State Rnd.StdGen ([a], [a])
randomSelectS ndPrb lx = randomSelectS' ndPrb lx ([], [])
    where
        randomSelectS' :: Double -> [a] -> ([a], [a]) -> S.State Rnd.StdGen ([a], [a])
        randomSelectS' _ [] (lxAcc1, lxAcc2)  = return (lxAcc1, lxAcc2)
        randomSelectS' ndPrb' (x:lrx) (lxAcc1, lxAcc2) = 
            do
                ndDice <- randomValueS (0.0, 1.0)
                if ndDice < ndPrb'
                    then randomSelectS' ndPrb' lrx (x : lxAcc1, lxAcc2)
                    else randomSelectS' ndPrb' lrx (lxAcc1, x : lxAcc2)

randomSelectOneS :: [a] -> S.State Rnd.StdGen (Maybe a)
randomSelectOneS lx = 
    do
        nLen <- pure (Sfr.niLen lx)
        if nLen > 0
            then
                do
                    ndIndex <- randomValueS (0.0, ((fromIntegral nLen) :: Double))
                    niIndex <- pure (round ndIndex)
                    mx <- pure (Lst.atMay lx (max 0 (min (nLen - 1) niIndex)))
                    return mx
            else
                return Nothing

randomReplaceOneS :: a -> [a] -> S.State Rnd.StdGen [a]
randomReplaceOneS xReplacement lx = 
    do
        nLen <- pure (Sfr.niLen lx)
        if nLen > 0
            then
                do
                    ndIndex <- randomValueS (0.0, ((fromIntegral nLen) :: Double))
                    niIndex <- pure (round ndIndex)
                    lx' <- pure (Lst.setAt niIndex xReplacement lx)
                    return lx'
            else
                return []

randomReplaceSomeS :: [a] -> [a] -> S.State Rnd.StdGen [a]
randomReplaceSomeS [] lx = return lx
randomReplaceSomeS (xRplmt:lrxRplmt) lx = 
    do
        lx' <- randomReplaceOneS xRplmt lx
        lx'' <- randomReplaceSomeS lrxRplmt lx'
        return lx''

class Randomisable r where
    random :: Rnd.StdGen -> r
    random prng = S.evalState randomS prng
    randomS :: S.State Rnd.StdGen r
    randomise :: Rnd.StdGen -> r -> r
    randomise prng r = S.evalState (randomiseS r) prng
    randomiseS :: r -> S.State Rnd.StdGen r
    randomiseS x = return x -- standard implemetation, can be overriden

randomValueS :: Rnd.Random a => (a, a) -> S.State Rnd.StdGen a
randomValueS range = S.state (Rnd.randomR range)

randomIntegralValueS :: (Rnd.Random a, Integral a) => (a, a) -> S.State Rnd.StdGen a
randomIntegralValueS range = 
    do
        ndIndex <- randomValueS (fromIntegral (fst range), fromIntegral (snd range) :: Double)
        niIndex <- pure (round ndIndex)
        niIndexSafe <- pure ((max (fst range) (min (snd range) niIndex)))
        return niIndexSafe

randomiseValueProbablyS :: (Rnd.Random a, Fractional a, Ord a) => Double -> (a, a) -> a -> S.State Rnd.StdGen a
randomiseValueProbablyS ndPrb rng nValue0 = 
        do
            nValue1 <- randomiseValueProbablyS' 10 ndPrb rng nValue0
            return (max (fst rng) (min (snd rng) nValue1))
    where
        randomiseValueProbablyS' :: (Fractional a) => Rnd.Random a => Integer -> Double -> (a, a) -> a -> S.State Rnd.StdGen a
        randomiseValueProbablyS' nCount ndPrb' rng' nValue0' 
            | nCount > 0 = 
                do
                    ndDice <- randomValueS (0.0, 1.0)
                    if ndDice < ndPrb
                        then
                            do
                                nAdd <- randomValueS (shiftRange rng')
                                return (nValue0' + nAdd)
                        else randomiseValueProbablyS' (nCount - 1) ndPrb' (shrinkRangeBy 2 rng') nValue0'
            | otherwise = return nValue0'
        shrinkRangeBy :: (Fractional a) => a -> (a, a) -> (a, a)
        shrinkRangeBy ndDiv (xFrom, xTo) = (xFrom, (xTo - xFrom) / ndDiv)
        shiftRange :: (Fractional a) => (a, a) -> (a, a)
        shiftRange (xFrom, xTo) = let xHalf = (xTo - xFrom) / 2 in (xFrom - xHalf, xTo - xHalf)


randomiseListProbablyS :: Randomisable a => Double -> (Integer, Integer) -> [a] -> S.State Rnd.StdGen [a]
randomiseListProbablyS ndPrb tplLengthRange lValues0 = 
    do
        ndDice <- randomValueS (0.0, 1.0)
        if ndDice < ndPrb
            then
                do
                    nLength <- randomIntegralValueS tplLengthRange
                    lValues1 <- (adjustRandomListS nLength lValues0)
                    lValues2 <- (randomiseListElementsProbablyS lValues1 [])
                    return lValues2
            else
                do
                    lValues1 <- (randomiseListElementsProbablyS lValues0 [])
                    return lValues1
    where
        adjustRandomListS :: Randomisable a => Integer -> [a] -> S.State Rnd.StdGen [a]
        adjustRandomListS nLength lValues0'
            | nLength > (Sfr.niLen lValues0') = randomExtendListS (nLength - (Sfr.niLen lValues0')) lValues0'
            | nLength < (Sfr.niLen lValues0') = return (Sfr.take nLength lValues0')
            | otherwise                               = return lValues0'
        randomiseListElementsProbablyS :: Randomisable a => [a] -> [a] -> S.State Rnd.StdGen [a]
        randomiseListElementsProbablyS [] lxAcc = return lxAcc
        randomiseListElementsProbablyS (x:lrx) lAcc =
            do
                xRandomised <- (randomiseS x)
                r <- (randomiseListElementsProbablyS lrx (xRandomised : lAcc))
                return r

randomListS :: forall a . Randomisable a => (Integer, Integer) -> S.State Rnd.StdGen [a]
randomListS tplLengthRange@(nFrom, nTo) 
    | nFrom == nTo = randomExtendListS nFrom []
    | otherwise = 
        do
            nLength <- randomValueS tplLengthRange
            randomExtendListS nLength []

randomExtendListS :: Randomisable a => Integer -> [a] -> S.State Rnd.StdGen [a]
randomExtendListS nGrowth lgntyp 
    | nGrowth > 0 = 
        do
            r1 <- randomS
            result <- (randomExtendListS (nGrowth - 1) (r1 : lgntyp))
            return result
    | otherwise       = return lgntyp

getGenS :: S.State Rnd.StdGen Rnd.StdGen
getGenS = S.gets id

splitGenS :: S.State Rnd.StdGen (Rnd.StdGen)
splitGenS = 
    do
        prng <- getGenS
        return (splitPrng prng)

tplSplitGenS2 :: S.State Rnd.StdGen (Rnd.StdGen, Rnd.StdGen)
tplSplitGenS2 = 
    do
        prng <- getGenS
        return (tplPrngSplit2 prng)

tplSplitGenS3 :: S.State Rnd.StdGen (Rnd.StdGen, Rnd.StdGen, Rnd.StdGen)
tplSplitGenS3 = 
    do
        prng <- getGenS
        return (tplPrngSplit3 prng)

tplSplitGenS4 :: S.State Rnd.StdGen (Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen)
tplSplitGenS4 = 
    do
        prng <- getGenS
        return (tplPrngSplit4 prng)

tplSplitGenS5 :: S.State Rnd.StdGen (Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen)
tplSplitGenS5 = 
    do
        prng <- getGenS
        return (tplPrngSplit5 prng)

tplSplitGenS6 :: S.State Rnd.StdGen (Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen)
tplSplitGenS6 = 
    do
        prng <- getGenS
        return (tplPrngSplit6 prng)

tplSplitGenS7 :: S.State Rnd.StdGen (Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen)
tplSplitGenS7 = 
    do
        prng <- getGenS
        return (tplPrngSplit7 prng)

tplSplitGenS8 :: S.State Rnd.StdGen (Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen)
tplSplitGenS8 = 
    do
        prng <- getGenS
        return (tplPrngSplit8 prng)

tplSplitGenS9 :: S.State Rnd.StdGen (Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen)
tplSplitGenS9 = 
    do
        prng <- getGenS
        return (tplPrngSplit9 prng)

lPrngSplit :: Rnd.StdGen -> Integer -> [Rnd.StdGen]
lPrngSplit prng0 nCount
    | nCount > 0 = 
        let
            (prng1, prng2) = Rnd.split prng0
        in
            prng2 : (lPrngSplit prng1 (nCount - 1))
    | otherwise = []

splitPrng :: Rnd.StdGen -> (Rnd.StdGen)
splitPrng prng0 = 
        let
            [prng1] = lPrngSplit prng0 1
        in
            (prng1)

tplPrngSplit2 :: Rnd.StdGen -> (Rnd.StdGen, Rnd.StdGen)
tplPrngSplit2 prng0 = 
        let
            [prng1, prng2] = lPrngSplit prng0 2
        in
            (prng1, prng2)

tplPrngSplit3 :: Rnd.StdGen -> (Rnd.StdGen, Rnd.StdGen, Rnd.StdGen)
tplPrngSplit3 prng0 = 
        let
            [prng1, prng2, prng3] = lPrngSplit prng0 3
        in
            (prng1, prng2, prng3)

tplPrngSplit4 :: Rnd.StdGen -> (Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen)
tplPrngSplit4 prng0 = 
        let
            [prng1, prng2, prng3, prng4] = lPrngSplit prng0 4
        in
            (prng1, prng2, prng3, prng4)

tplPrngSplit5 :: Rnd.StdGen -> (Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen)
tplPrngSplit5 prng0 = 
        let
            [prng1, prng2, prng3, prng4, prng5] = lPrngSplit prng0 5
        in
            (prng1, prng2, prng3, prng4, prng5)

tplPrngSplit6 :: Rnd.StdGen -> (Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen)
tplPrngSplit6 prng0 = 
        let
            [prng1, prng2, prng3, prng4, prng5, prng6] = lPrngSplit prng0 6
        in
            (prng1, prng2, prng3, prng4, prng5, prng6)

tplPrngSplit7 :: Rnd.StdGen -> (Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen)
tplPrngSplit7 prng0 = 
        let
            [prng1, prng2, prng3, prng4, prng5, prng6, prng7] = lPrngSplit prng0 7
        in
            (prng1, prng2, prng3, prng4, prng5, prng6, prng7)

tplPrngSplit8 :: Rnd.StdGen -> (Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen)
tplPrngSplit8 prng0 = 
        let
            [prng1, prng2, prng3, prng4, prng5, prng6, prng7, prng8] = lPrngSplit prng0 8
        in
            (prng1, prng2, prng3, prng4, prng5, prng6, prng7, prng8)

tplPrngSplit9 :: Rnd.StdGen -> (Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen, Rnd.StdGen)
tplPrngSplit9 prng0 = 
        let
            [prng1, prng2, prng3, prng4, prng5, prng6, prng7, prng8, prng9] = lPrngSplit prng0 9
        in
            (prng1, prng2, prng3, prng4, prng5, prng6, prng7, prng8, prng9)

