{-|
Description : test.
Copyright   : (c) Jörg K.-H. W. Brüggmann, 2021-2022
License     : proprietary, to be dual licensed
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE FlexibleInstances #-}

module SpecCodeTestHelper
    (
        QC.Arbitrary(..),
        Stub(..),
        T',
        fromT',
        Int256Plus,
        njFromInt256Plus,
        niFromInt256Plus,
        IntCharPlus,
        njFromIntCharPlus,
        niFromIntCharPlus
    ) where

import qualified Test.Tasty.QuickCheck as QC

import qualified Octets as Oct
import qualified Safer as Sfr

import qualified Code as Cd

instance QC.Arbitrary Stub where
    arbitrary =
        do
            nj <- QC.frequency
                [
                    (2, QC.elements [25]),
                    (2, QC.choose (-5, 5)),    -- 0
                    (1, QC.choose (6, 19)),
                    (2, QC.choose (20, 30)),   -- 25
                    (1, QC.choose (31, 69)),
                    (2, QC.choose (70, 80)),   -- 75
                    (1, QC.choose (81, 93)),
                    (2, QC.choose (94, 104))   -- 99
                ]
            return (Stub nj)

newtype T' a = T' a
    deriving Show

fromT' :: T' a -> a
fromT' (T' x) = x

instance QC.Arbitrary (T' Char) where
    arbitrary =
        do
            genChar <- QC.frequency
                [
                    (1, QC.elements [Cd.cdNul :: Char]),
                    (5, QC.chooseAny )
                ]
            return (T' genChar)

instance QC.Arbitrary Cd.CharUtf8 where
    arbitrary =
        QC.frequency
        [
            -- 1 case -> shall have 16% contribution
            (16, QC.elements [Cd.cdNul :: Cd.CharUtf8]),
            -- 4 cases -> shall have 84% contribution
            (21, Cd.CU8OneByte    <$> fOctet11),
            (21, Cd.CU8TwoBytes   <$> fOctet12 <*> fOctet2 ),
            (21, Cd.CU8ThreeBytes <$> fOctet13 <*> fOctet2 <*> fOctet2),
            (21, Cd.CU8FourBytes  <$> fOctet14 <*> fOctet2 <*> fOctet2 <*> fOctet2)
        ]

fOctet11 :: QC.Gen Oct.Octet
fOctet11 = QC.frequency
    [
        -- 1 valid code, NUL -> shall have 16% contribution
        (49, QC.elements [0b00000000]),
        -- 127 valid codes -> shall have 42% contribution
        (128, QC.choose (0b00000001, 0b01111111)),
        -- 128 invalid codes -> shall have 42% contribution
        (128, QC.choose (0b10000000, 0b11111111))
    ]

fOctet12 :: QC.Gen Oct.Octet
fOctet12 = QC.frequency
    [
        -- 32 valid codes -> shall have 50% contribution
        (96, QC.choose (0b11000000, 0b11011111)),
        -- 96 invalid codes -> shall have 50% contribution
        (64, QC.choose (0b00000000, 0b10111111)),
        (32, QC.choose (0b11100000, 0b11111111))
    ]

fOctet13 :: QC.Gen Oct.Octet
fOctet13 = QC.frequency
    [
        -- 16 valid codes -> shall have 50% contribution
        (48, QC.choose (0b11100000, 0b11101111)),
        -- 48 invalid codes -> shall have 50% contribution
        (32, QC.choose (0b00000000, 0b11011111)),
        (16, QC.choose (0b11110000, 0b11111111))
    ]

fOctet14 :: QC.Gen Oct.Octet
fOctet14 = QC.frequency
    [
        -- 8 valid codes -> shall have 50% contribution
        (24, QC.choose (0b11110000, 0b11110111)),
        -- 24 invalid codes -> shall have 50% contribution
        (16, QC.choose (0b00000000, 0b11101111)),
        (8, QC.choose (0b11111000, 0b11111111))
    ]

fOctet2 :: QC.Gen Oct.Octet
fOctet2 = QC.frequency
    [
        -- 64 valid codes -> shall have 50% contribution
        (192, QC.choose (0b10000000, 0b10111111)),
        -- 192 invalid codes -> shall have 50% contribution
        (128, QC.choose (0b00000000, 0b01111111)),
        (64, QC.choose (0b11000000, 0b11111111))
    ]

instance QC.Arbitrary Cd.CharIso1 where
    arbitrary =
        Cd.CharIso1 <$>
            QC.frequency
                [
                    -- 1 valid, NUL -> shall have 16% contribution
                    (61, QC.elements [0x00]),
                    -- 224 valid, non-NUL -> shall have 59% contribution
                    (128, QC.choose (0x01, 0x7F)),
                    (96, QC.choose (0xA0, maxBound :: Oct.Octet)),
                    -- 7 invalid -> shall have 25% contribution
                    (95, QC.choose (0x80, 0x9F))
                ]

instance QC.Arbitrary Cd.CharWin1 where
    arbitrary =
        Cd.CharWin1 <$>
            QC.frequency
                [
                    -- 1 valid, NUL -> shall have 16% contribution
                    (80, QC.elements [0x00]), -- 1 valid, NUL
                    -- 250 valid, non-NUL -> shall have 59% contribution
                    (13, QC.choose (0x82, 0x8C)),  -- 11 valid
                    (1, QC.elements [0x8E]),  -- 1 valid
                    (15, QC.choose (0x91, 0x9C)), -- 12 valid
                    (116, QC.choose (0x9E, maxBound :: Oct.Octet)), -- 98 valid
                    -- 5 invalid -> shall have 25% contribution
                    (125, QC.elements [0x81, 0x8D, 0x8F, 0x90, 0x9D]) -- 5 invalid
                ]

instance QC.Arbitrary (T' Oct.Octet) where
    arbitrary =
        do
            genOctet <- QC.frequency
                [
                    -- 1 valid, NUL -> shall have 16% contribution
                    (49, QC.elements [Cd.cdNul :: Oct.Octet]),
                    -- 255 valid, non-NUL -> shall have 84% contribution
                    (255, QC.chooseAny )
                ]
            return (T' genOctet)

newtype Int256Plus = Int256Plus Int
    deriving Show

instance QC.Arbitrary Int256Plus where
    arbitrary =
        do
            nj <- QC.frequency
                [
                    -- 
                    (1, QC.choose (-100, -1)),
                    -- 
                    (1, QC.choose (0, 255)),
                    -- 
                    (1, QC.choose (256, 355))
                ]
            return (Int256Plus nj)

njFromInt256Plus :: Int256Plus -> Int
njFromInt256Plus (Int256Plus nj) = nj

niFromInt256Plus :: Int256Plus -> Integer
niFromInt256Plus (Int256Plus nj) = fromIntegral nj

newtype IntCharPlus = IntCharPlus Int
    deriving Show

instance QC.Arbitrary IntCharPlus where
    arbitrary =
        do
            nj <- QC.frequency
                [
                    -- 
                    (5, QC.choose (-100, -1)),
                    -- 
                    (90, QC.choose (0, 0x10FFFF)),
                    -- 
                    (5, QC.choose (0x110000, 0x110000 + 99))
                ]
            return (IntCharPlus nj)

njFromIntCharPlus :: IntCharPlus -> Int
njFromIntCharPlus (IntCharPlus nj) = nj

niFromIntCharPlus :: IntCharPlus -> Integer
niFromIntCharPlus (IntCharPlus nj) = fromIntegral nj

--------------------------------------------------------------------------------
-- Stub code for class 'Code' 
-- to test default functions (niMinOrd, cdNormalise, isNormalised, isOrdInRange)

newtype Stub = Stub { rniStb :: Int }
    deriving (Show, Eq, Ord)

{-
    (Stub  -2) -> not valid  -> -2 -> (Stub 13) -> '?'
    (Stub  -1) -> not valid  -> -1 -> (Stub 13) -> '?'
    (Stub   0) -> valid      ->  0 -> (Stub  0) -> '0'
    (Stub   1) -> valid      ->  1 -> (Stub  1) -> '1'
    (Stub   2) -> valid      ->  2 -> (Stub  2) -> '2'
    (Stub  23) -> valid      -> 23 -> (Stub 23) -> 'G'
    (Stub  24) -> valid      -> 24 -> (Stub 24) -> 'H'
    (Stub  25) -> valid, NUL -> 25 -> (Stub 25) -> '\x0'
    (Stub  26) -> valid      -> 26 -> (Stub 26) -> 'a'
    (Stub  27) -> valid      -> 27 -> (Stub 27) -> 'b'
    (Stub  48) -> valid      -> 48 -> (Stub 48) -> 'w'
    (Stub  49) -> valid      -> 49 -> (Stub 49) -> 'x'
    (Stub  50) -> valid      ->  0 -> (Stub  0) -> '0'
    (Stub  51) -> valid      ->  1 -> (Stub  1) -> '1'
    (Stub  52) -> valid      ->  2 -> (Stub  2) -> '2'
    (Stub  73) -> valid      -> 23 -> (Stub 23) -> 'G'
    (Stub  74) -> valid      -> 24 -> (Stub 24) -> 'H'
    (Stub  75) -> valid, NUL -> 25 -> (Stub 25) -> '\x0'
    (Stub  76) -> valid      -> 26 -> (Stub 26) -> 'a'
    (Stub  77) -> valid      -> 27 -> (Stub 27) -> 'b'
    (Stub  98) -> valid      -> 48 -> (Stub 48) -> 'w'
    (Stub  99) -> valid      -> 49 -> (Stub 49) -> 'x'
    (Stub 100) -> not valid  -> -1 -> (Stub 63) -> '?'
    (Stub 101) -> not valid  -> -1 -> (Stub 63) -> '?'
-}
instance Cd.Code Stub where
    -- cdNul :: cd
    cdNul = Stub 25
    -- isNul :: cd -> Bool
    isNul (Stub ni) = (ni == 25) || (ni == 75)
    -- isValid :: cd -> Bool
    isValid (Stub ni) = (ni >= 0) && (ni <= 99)
    -- cdReplacement :: cd
    cdReplacement = Stub 13
    -- niOrd :: cd -> Integer
    niOrd stub@(Stub ni)
        | Cd.isValid stub && (ni <= 49) = fromIntegral ni
        | Cd.isValid stub               = fromIntegral (ni - 50)
        | otherwise                     = -1
    -- cdFromInteger :: Integer -> cd
    cdFromInteger ni
        | (ni >= 0) && (ni <= Cd.niMaxOrd (Cd.Px :: Cd.Px Stub)) = Stub (fromIntegral ni)
        | otherwise                                              = Cd.cdReplacement
    -- cdNormalise :: cd -> cd
    -- cdNormalise = cdFromInteger . niOrd                  -- default implementation
    -- isNormalised :: cd -> Bool
    -- isNormalised cd = cd == (cdNormalise cd)             -- default implementation
    -- niMinOrd :: (Px cd) -> Integer
    -- niMinOrd _ = 0                                       -- default implementation
    -- niMaxOrd :: (T.Px cd) -> Integer
    niMaxOrd _ = 49
    -- chFromCode :: cd -> Char
    chFromCode cd
        | niOrd' == 25
            = '\0'
        | niOrd' >= 0 && niOrd' < 25 =
            Cd.cdFromInteger (Cd.niOrd cd + 0x30) -- implies Cd.cdFromInteger :: Integer -> Char !!!
        | niOrd' > 25 && niOrd' <= Cd.niMaxOrd (Cd.Px :: Cd.Px Stub) =
            Cd.cdFromInteger (Cd.niOrd cd + 0x47) -- implies Cd.cdFromInteger :: Integer -> Char !!!
        | otherwise =
            '?'
        where
            niOrd' = Cd.niOrd cd
    -- cdFromChar :: Char -> cd
    cdFromChar ch 
        | ch >= '0' && ch <= 'H' = Cd.cdFromInteger (niOrd' - 0x30)
        | ch >= 'a' && ch <= 'x' = Cd.cdFromInteger (niOrd' - 0x47)
        | otherwise = Cd.cdReplacement
        where
            niOrd' = Cd.niOrd ch -- implies Cd.niOrd :: Char!

instance Sfr.Enum Stub where
    -- toEnum              :: Integer -> a
    toEnum = Cd.cdFromInteger
    -- fromEnum            :: a -> Integer
    fromEnum = Cd.niOrd
