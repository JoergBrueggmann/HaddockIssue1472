{-|
Description : provides a class to convert lists of data to a list of octets and vice versa.
Copyright   : (c) Jörg K.-H. W. Brüggmann, 2021-2022
License     : proprietary, to be dual licensed
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX

The module provides the class 'OctetCodec' to convert lists of data to a list of octets and vice versa.

* simmilar to 'Octetable'

    * The difference between 'OctetCodec' and 'Octetable' is that 'OctetCodec' works also on lists of data items 
like streams of characters (e.g. [Octet]/Octets, [Char]/String, [CU8Char], and so on), whereas 'Octetable' only works on single data items 
but on a larger variety of types (e.g. Word32, Char, CU8Char, Int, Interger)

* supported codes, all that are implemented in module 'CharCode' and the standard character code, which are:

    * Octet
    * Char
    * CharUtf8
    * CharIso1
    * CharWin1
-}

{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TypeSynonymInstances #-} -- to allow instance of OctetCodec with type synonym Oct.Octet

module OctetCodec
    (
        OctetCodec(..),
        takeMaybeS,
        takeOctListS,
        Oct.Octetable(..),
        Oct.Octet,
        Oct.Octets,
        Oct.getOctetsDecoded,
        Oct.putOctetsEncoded,
        Oct.keepOctetsPositive,
        Oct.keepOctetsNegative
    ) where

import qualified Control.Monad.State as S
import qualified Data.Bits as Bts hiding ((.|.), (.&.))
import           Data.Bits ((.|.), (.&.))
import qualified System.IO as SysIo
import qualified Data.ByteString as BS

import qualified Code as Cd
import qualified Octetable as Oct
import qualified Safer as Sfr
--import qualified TestCaseWrap as TCW
--import qualified TypedCodeOctets as TCO


--------------------------------------------------------------------------------
--  OctetCodec

class OctetCodec tValue where
    decode :: Oct.Octets -> Maybe tValue
    decode lw8 = S.evalState decodeMaybeS lw8
    decodeList :: Oct.Octets -> [tValue]
    decodeList lw8 = S.evalState decodeListS lw8
    decodeMaybeS :: S.State Oct.Octets (Maybe tValue)
    decodeListS :: S.State Oct.Octets [tValue]
    decodeListS =
        do
            mx <- decodeMaybeS
            Sfr.ifJust mx
                {- then -} (\x ->
                    do
                        liValues <- decodeListS
                        return (x : liValues))
                {- else -} (return [])
    decodeNListS :: Integer -> S.State Oct.Octets [tValue]
    decodeNListS lCount
        | lCount > 0 =
            do
                mx <- decodeMaybeS
                Sfr.ifJust mx
                    {- then -} (\x ->
                        do
                            liValues <- decodeNListS (lCount - 1)
                            return (x : liValues))
                    {- else -} (return [])
        | otherwise = return []
    encode :: tValue -> Oct.Octets
    encodeList :: [tValue] -> Oct.Octets
    encodeList [] = []
    encodeList (x:lrx) = ((encode x) :: Oct.Octets) ++ (encodeList lrx)

takeMaybeS :: S.State Oct.Octets (Maybe Oct.Octet)
takeMaybeS =
    do
        moct <- (S.gets moctGet)
        if moct == Nothing
            then return Nothing
            else
                do
                    S.modify (eatOct 1)
                    return moct

moctGet :: Oct.Octets -> (Maybe Oct.Octet)
moctGet [] = Nothing
moctGet (w8:_) = Just w8

eatOct :: Integer -> Oct.Octets -> Oct.Octets
eatOct _ [] = []
eatOct nCount lch@(_:lrch)
    | nCount > 0 = eatOct (nCount - 1) lrch
    | otherwise  = lch

takeOctListS :: Integer -> S.State Oct.Octets Oct.Octets
takeOctListS nCount =
    do
        lw8 <- S.gets (loctGet nCount)
        S.modify (eatOct nCount)
        return lw8

loctGet :: Integer -> Oct.Octets -> Oct.Octets
loctGet _ [] = []
loctGet nCount (w8:lrw8)
    | nCount > 0 = w8 : (loctGet (nCount - 1) lrw8)
    | otherwise  = []

instance OctetCodec Oct.Octet where
    --decodeMaybeS :: S.State Oct.Octets (Maybe tValue)
    decodeMaybeS =
        do
            moct <- takeMaybeS
            Sfr.ifJust moct
                {- then -} (\oct -> return (Just oct))
                {- else -} (return Nothing)
    -- encode :: tValue -> Oct.Octets
    encode oct = [oct]

instance OctetCodec Char where
    --decodeMaybeS :: S.State Oct.Octets (Maybe tValue)
    decodeMaybeS =
        do
            loct <- takeOctListS 4
            if (Sfr.niLen loct) == 4
                then return (Just (Oct.fromOctets loct))
                else return Nothing
    -- encode :: tValue -> Oct.Octets
    encode ch = Oct.toOctets ch

instance OctetCodec Cd.CharUtf8 where
    --decodeMaybeS :: S.State Oct.Octets (Maybe tValue)
    decodeMaybeS =
        do
            mw81 <- takeMaybeS
            Sfr.ifJust mw81
                {- then -} (\w81 ->
                    do
                        decodeMaybeS' w81)
                {- else -} (return Nothing)
        where
            decodeMaybeS' :: Oct.Octet -> S.State Oct.Octets (Maybe Cd.CharUtf8)
            decodeMaybeS' w81
                | (w81 .&. 0b10000000) == 0b00000000 = return (Just (Cd.CU8OneByte w81))
                | (w81 .&. 0b11100000) == 0b11000000 =
                    do
                        mw82 <- takeMaybeS
                        Sfr.ifJust mw82
                            {- then -} (\w82 -> return (Just (Cd.CU8TwoBytes w81 w82)))
                            {- else -} (return Nothing)
                | (w81 .&. 0b11110000) == 0b11100000 =
                    do
                        mw82 <- takeMaybeS
                        mw83 <- takeMaybeS
                        Sfr.ifJust2 mw82 mw83
                            {- then -} (\w82 w83 -> return (Just (Cd.CU8ThreeBytes w81 w82 w83)))
                            {- else -} (return Nothing)
                | otherwise =
                    do
                        mw82 <- takeMaybeS
                        mw83 <- takeMaybeS
                        mw84 <- takeMaybeS
                        Sfr.ifJust3 mw82 mw83 mw84
                            {- then -} (\w82 w83 w84 -> return (Just (Cd.CU8FourBytes w81 w82 w83 w84)))
                            {- else -} (return Nothing)
    -- encode :: tValue -> Oct.Octets
    encode cu8 = Oct.toOctets cu8

instance OctetCodec Cd.CharIso1 where
    --decodeMaybeS :: S.State Oct.Octets (Maybe tValue)
    decodeMaybeS =
        do
            mw8 <- takeMaybeS
            Sfr.ifJust mw8
                {- then -} (\w8 -> return (Just (Cd.CharIso1 w8)))
                {- else -} (return Nothing)
    -- encode :: tValue -> Oct.Octets
    encode ci1 = Oct.toOctets ci1

instance OctetCodec Cd.CharWin1 where
    --decodeMaybeS :: S.State Oct.Octets (Maybe tValue)
    decodeMaybeS =
        do
            mw8 <- takeMaybeS
            Sfr.ifJust mw8
                {- then -} (\w8 -> return (Just (Cd.CharWin1 w8)))
                {- else -} (return Nothing)
    -- encode :: tValue -> Oct.Octets
    encode cw1 = Oct.toOctets cw1
