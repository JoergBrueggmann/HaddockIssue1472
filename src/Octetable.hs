{-|
Description : provides a class to convert single data items to a list of octets and vice versa.
Copyright   : (c) Jörg K.-H. W. Brüggmann, 2021-2022
License     : proprietary, to be dual licensed
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX

The module provides the class 'Octetable' to convert single data items to a list of octets ('Octets') and vice versa.

* characteristics:

    * 

* supported codes, all that are implemented in module 'CharCode' and the standard character code, which are:

    * Word32
    * Word64
    * Char
    * CharUtf8
    * CharIso1
    * CharWin1
    * Integer
    * Int
    * Double
    * Float

-}

{-# LANGUAGE BinaryLiterals #-}

module Octetable
    (
        Octetable(..), 
        Oct.Octet, 
        Oct.Octets, 
        Oct.getOctetsDecoded, 
        Oct.putOctetsEncoded, 
        Oct.loctFromIntegral, 
        Oct.integralFromOctets, 
        Oct.keepOctetsPositive, 
        Oct.keepOctetsNegative, 
        W.Word32, 
        W.Word64
    ) where

import qualified Data.Word as W
import qualified Data.Bits as Bts hiding ((.|.), (.&.))
import           Data.Bits ((.|.), (.&.))
import qualified Data.Char as Chr

import qualified DoubleNFloat as DnF
import qualified Safer as Sfr
import qualified Code as Cd
import qualified Octets as Oct


--------------------------------------------------------------------------------
--  class Octetable

class (Eq a, Show a) => Octetable a where
    toOctets :: a -> Oct.Octets
    fromOctets :: Oct.Octets -> a

instance Octetable W.Word32 where
    toOctets w32 = fmap (\nShift -> fromIntegral (w32 `Bts.shiftR` nShift)) [32-8, 32-(2*8)..0]
    fromOctets lw = foldl (\w32 w8 -> (w32 `Bts.shiftL` 8) + (fromIntegral w8))  0 lw

instance Octetable W.Word64 where
    toOctets w64 = fmap (\nShift -> fromIntegral (w64 `Bts.shiftR` nShift)) [64-8, 64-(2*8)..0] {-
        [ fromIntegral (w64 `Bts.shiftR` 56)
        , fromIntegral (w64 `Bts.shiftR` 48)
        , fromIntegral (w64 `Bts.shiftR` 40)
        , fromIntegral (w64 `Bts.shiftR` 32)
        , fromIntegral (w64 `Bts.shiftR` 24)
        , fromIntegral (w64 `Bts.shiftR` 16)
        , fromIntegral (w64 `Bts.shiftR` 8)
        , fromIntegral w64
        ]-}
    fromOctets lw = foldl (\w64 w8 -> (w64 `Bts.shiftL` 8) + (fromIntegral w8))  0 lw

instance Octetable Char where
    toOctets ch = Sfr.pad 4 0x00 ((Oct.loctFromIntegral . Chr.ord) ch)
    fromOctets [] = '\0'
    fromOctets lw 
        | niOrd >= 0    = Chr.chr niOrd
        | otherwise     = '\0'
        where
            niOrd = (Oct.integralFromOctets lw)

instance Octetable Cd.CharUtf8 where
    toOctets (Cd.CU8OneByte w81) = [w81]
    toOctets (Cd.CU8TwoBytes w81 w82) = [w81,w82]
    toOctets (Cd.CU8ThreeBytes w81 w82 w83) = [w81,w82,w83]
    toOctets (Cd.CU8FourBytes w81 w82 w83 w84) = [w81,w82,w83,w84]
    fromOctets (w81:w82:w83:w84:_)
        | (w81 .&. 0b10000000) == 0b00000000 = Cd.CU8OneByte w81
        | (w81 .&. 0b11100000) == 0b11000000 = Cd.CU8TwoBytes w81 w82
        | (w81 .&. 0b11110000) == 0b11100000 = Cd.CU8ThreeBytes w81 w82 w83
        | otherwise                          = Cd.CU8FourBytes w81 w82 w83 w84
    fromOctets (w81:w82:w83:[])
        | (w81 .&. 0b10000000) == 0b00000000 = Cd.CU8OneByte w81
        | (w81 .&. 0b11100000) == 0b11000000 = Cd.CU8TwoBytes w81 w82
        | otherwise                          = Cd.CU8ThreeBytes w81 w82 w83
    fromOctets (w81:w82:[])
        | (w81 .&. 0b10000000) == 0b00000000 = Cd.CU8OneByte w81
        | otherwise                          = Cd.CU8TwoBytes w81 w82
    fromOctets (w81:[]) = Cd.CU8OneByte w81
    fromOctets [] = (Cd.CU8OneByte 0)

instance Octetable Cd.CharIso1 where
    toOctets (Cd.CharIso1 w81) = [w81]
    fromOctets (w81:_) = Cd.CharIso1 w81
    fromOctets [] = (Cd.CharIso1 0)

instance Octetable Cd.CharWin1 where
    toOctets (Cd.CharWin1 w81) = [w81]
    fromOctets (w81:_) = Cd.CharWin1 w81
    fromOctets [] = (Cd.CharWin1 0)

instance Octetable Integer where
    toOctets n = Oct.loctFromIntegral n
    fromOctets lw = Oct.integralFromOctets lw

instance Octetable Int where
    toOctets n = Oct.loctFromIntegral n
    fromOctets lw = Oct.integralFromOctets lw

instance Octetable Double where
    toOctets n = toOctets (DnF.doubleToWord n)
    fromOctets lw = DnF.wordToDouble (fromOctets lw)

instance Octetable Float where
    toOctets n = toOctets (DnF.floatToWord n)
    fromOctets lw = DnF.wordToFloat (fromOctets lw)
