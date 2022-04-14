{-|
Description : provides advanced helper functions for lists.
Copyright   : (c) Jörg K.-H. W. Brüggmann, 2021-2022
License     : proprietary, to be dual licensed
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX

The module Type provides advanced helper functions for lists.

See also more basic list functions in module Safer.

Suggested import line: 'import qualified List as Lst'

The module List provides:

    * all functions from 'Data.List'
    * 'areAllOrdUnique' - to test whether all elements are unique
    * 'atDefault' - to get an element at an index position, defaulted
    * 'atMaybe' - to get an element at an index position, may be
-}

module List
    (
        areAllOrdUnique,
        duplicates,
        atDef,
        atMay,
        setAt,
        takeEnd,
        dropEnd,
        isMinAvailable,
        shiftR,
        module Lst
    ) where

import qualified Safer as Sfr

import qualified Data.List as Lst

-- areAllUnique
{-| ...to test whether all elements are unique (no duplicates and not any element is equal to any other element in the list).
* empty lists result in True
* elements have to be of class Ord, to have computational complexity of 𝒪(n⋅log n)

    * sorts the list and then checks whether all neighbours are different
-}
areAllOrdUnique :: Ord a => [a] -> Bool
areAllOrdUnique = areAllNeighboursDifferent . Lst.sort

-- areAllNeighboursDifferent
-- | ...to test whether all neighbour of elements are different.
areAllNeighboursDifferent :: Eq a => [a] -> Bool
areAllNeighboursDifferent []  = True
areAllNeighboursDifferent [_] = True
areAllNeighboursDifferent (x0:lrx1@(x1:_))
    | x0 == x1  = False
    | otherwise = areAllNeighboursDifferent lrx1

duplicates :: Ord a => [a] -> [(a, Integer)]
duplicates = equalNeighboursAcc [] . lIndexedSort . lIndexedAcc 0

compareIndexed :: Ord a => (a, Integer) -> (a, Integer) -> Ordering
compareIndexed (x0,_) (x1,_) = compare x0 x1

lIndexedSort :: Ord a => [(a, Integer)] -> [(a, Integer)]
lIndexedSort = Lst.sortBy compareIndexed

lIndexedAcc :: Integer -> [a] -> [(a, Integer)]
lIndexedAcc _ [] = []
lIndexedAcc niIndex (x:lrx) = (x, niIndex) : lIndexedAcc (niIndex + 1) lrx

-- equalNeighboursAcc
-- | ...to test whether all neighbour of elements are different.
equalNeighboursAcc :: Eq a => [(a, Integer)] -> [(a, Integer)] -> [(a, Integer)]
equalNeighboursAcc acc []  = acc
equalNeighboursAcc acc [_] = acc
equalNeighboursAcc acc ((x0,ni0):lrx@((x1,ni1):_))
    | x0 == x1  = equalNeighboursAcc ((x0,ni0):(x1,ni1):acc) lrx
    | otherwise = equalNeighboursAcc acc lrx

-- setAt...
-- | ...sets (replaces) an elements at a indexed position within a list.
{- |
* if the index is negative or the index is >= length of the list

    * leaves the list unchanged
-}
setAt :: Integer -> a -> [a] -> [a]
setAt _ _ [] = []
setAt nPos xReplacement lx@(x:lrx)
    | nPos == 0 = xReplacement : lrx
    | nPos < 0 = lx
    | otherwise = x : setAt (nPos -  1) xReplacement lrx

atDef :: a -> [a] -> Integer -> a
atDef aDef [] _ = aDef                          -- case: is empty anyway
atDef _ (a:_) 0 = a                             -- case: index is 0 -> take it
atDef aDef (_:la) nIndex
    | nIndex > 0 = atDef aDef la (nIndex - 1)   -- case: index is positive
    | otherwise  = aDef                         -- case: index is negative

atMay :: [a] ->Integer ->  Maybe a
atMay []     _ = Nothing                        -- case: is empty anyway
atMay (a:_)  0 = Just a                         -- case: index is 0 -> take it
atMay (_:la) nIndex
    | nIndex > 0 = atMay la (nIndex - 1)        -- case: index is positive
    | otherwise  = Nothing                      -- case: index is negative

takeEnd :: Integer -> [a] -> [a]
takeEnd ni lx                                       -- algorithm adapted from Data.List.Extra
    | ni <= 0 = []
    | otherwise = takeEnd' lx (Sfr.drop ni lx)
    where
        takeEnd' (_:lrx) (_:ys) = takeEnd' lrx ys
        takeEnd' lx' _ = lx'

dropEnd :: Integer -> [a] -> [a]
dropEnd _ [] = []
dropEnd ni lx@(x:lrx)
    | ni <= 0               = lx
    | isMinAvailable ni lrx = x : dropEnd ni lrx
    | otherwise             = []

isMinAvailable :: Integer -> [a] -> Bool
isMinAvailable ni [] = ni <= 0
isMinAvailable ni (_:lrx)
    | ni <= 0   = True
    | otherwise = isMinAvailable (ni - 1) lrx

-- shiftR...
-- | ...shifts elements of a list to the right.
{- |
* keeps the amount of elements in the list
* to the right means towards tail of the list
-}
shiftR :: a -> Integer -> [a] -> [a]
shiftR xDflt ni lx = Sfr.pad (Sfr.niLen lx) xDflt (dropEnd ni lx)
