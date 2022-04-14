{-|
Description : provides classes and functions helping to to reduce runtime errors.
Copyright   : (c) Jörg K.-H. W. Brüggmann, 2021-2022
License     : proprietary, to be dual licensed
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX

The module Safer provides classes and functions helping to to reduce runtime errors.

* supports the following strategies/methods

    * usage of 'Integer' instead of 'Int'
    * usage of 'Maybe' for optional results (e.g. like in 'mHead')

*  provides classes to use 'Integer' instead of 'Int', which are:

    * 'Enum'

        * Available instances of 'Enum' are only for types that are defined in 'Prelude', at the moment:

            * 'Char'            - the standard 'Prelude' character code (internal unicode representation), 
            * 'Bool'            - to represent a bit.

        * Orphan instances of 'Enum' are explicitly encuraged, where the data type is defined.

* provides functions that replace

    * 'niLen' instead of 'length'
    * 'take' instead of 'Prelude.take'
    * 'drop' instead of 'Prelude.drop'
    * 'takeEnd' instead of 'Prelude.takeEnd'
    * 'mHead' instead of 'Prelude.head'
    * 'replicate' instead of 'Prelude.replicate'
    * 'pad' instead of 'ByteString.padded '

* additionally it provides functions to handle 'Maybe' values without using pattern matching and sometimes additional functions where clauses

    * example, usage of 'ifJust'

    @
parseS :: Syntax -> S.State Oct.Octets (Maybe SymbolTree)
parseS (SynSequence lsyn) = 
    do
        mlsym <- parseS' lsyn
        Sfr.ifJust mlsym
            {- then -} (\lsym -> return (Just (SymbolNode SymSequence lsym)))
            {- else -} (return Nothing)
    @

        * instead of

    @
parseS (SynSequence lsyn) = 
    do
        mlsym <- parseS' lsyn
        parseS'' mlsym
    where
        parseS'' :: (Maybe [SymbolNode]) -> S.State Oct.Octets (Maybe SymbolTree)
        parseS'' (Just lsym') = return (Just (SymbolNode SymSequence lsym'))
        parseS'' Nothing = return Nothing
    @
-}

module Safer (
        Enum(..),
        niLen,
        take,
        drop,
        mHead,
        headDefault,
        replicate,
        pad,
        ifJust,
        ifJust2,
        ifJust3,
        (.>>),
        (<<.),
        niCount
    ) where

import Prelude hiding (Enum(..), take, drop, head, replicate)
import qualified Prelude as Pre
import qualified Data.List as Lst
import qualified Data.Bits as Bts hiding ((.|.), (.&.))

-- import Debug.Trace

class Enum a where
    -- | the successor of a value.  For numeric types, 'succ' adds 1.
    succ                :: a -> a
    succ = toEnum . (+ 1) . fromEnum
    -- | the predecessor of a value.  For numeric types, 'pred' subtracts 1.
    pred                :: a -> a
    pred = toEnum . subtract 1 . fromEnum
    -- | Convert from an 'Integer'.
    toEnum              :: Integer -> a
    -- | Convert to an 'Integer'.
    fromEnum            :: a -> Integer

niLen :: [a] -> Integer
niLen = Lst.genericLength

take :: Integer -> [a] -> [a]
take = Lst.genericTake

drop :: Integer -> [a] -> [a]
drop = Lst.genericDrop

mHead :: [a] -> Maybe a
mHead [] = Nothing
mHead (x:_) = Just x

headDefault :: a -> [a] -> a
headDefault xDflt [] = xDflt
headDefault _ (x:_) = x

replicate :: Integer -> a -> [a]
replicate = Lst.genericReplicate

pad :: Integer -> a -> [a] -> [a]
pad n xToFill lx = replicate nDiff xToFill ++ lx
    where
        nLen = niLen lx
        nDiff = n - nLen

ifJust :: Maybe a -> (a -> b) -> b -> b
ifJust (Just x) then' _ = then' x
ifJust Nothing _ bDflt = bDflt

ifJust2 :: Maybe a -> Maybe b -> (a -> b -> c) -> c -> c
ifJust2 (Just x1) (Just x2) then' _ = then' x1 x2
ifJust2 Nothing _ _ bDflt = bDflt
ifJust2 _ Nothing _ bDflt = bDflt

ifJust3 :: Maybe a -> Maybe b -> Maybe c -> (a -> b -> c -> d) -> d -> d
ifJust3 (Just x1) (Just x2) (Just x3) then' _ = then' x1 x2 x3
ifJust3 Nothing _ _ _ bDflt = bDflt
ifJust3 _ Nothing _ _ bDflt = bDflt
ifJust3 _ _ Nothing _ bDflt = bDflt

infixl 7 .>>, <<.

(.>>) :: Bts.Bits a => a -> Integer -> a
x .>> ni
    | ni >= 0 && ni <= fromIntegral (maxBound :: Int) = x `Bts.shiftR` fromIntegral ni
    | ni < 0 = x
    | otherwise {- ni > (maxBound :: Int) -} = Bts.zeroBits

(<<.) :: Bts.Bits a => a -> Integer -> a
x <<. ni
    | ni >= 0 && ni <= fromIntegral (maxBound :: Int) = x `Bts.shiftL` fromIntegral ni
    | ni < 0 = x
    | otherwise {- ni > (maxBound :: Int) -} = Bts.zeroBits

niCount :: Eq a => a -> [a] -> Integer
niCount x = niLen . filter (==x)
