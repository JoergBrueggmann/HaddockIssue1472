{-|
Description : provides the types 'Octet', 'Octets' as lists of 'Word8' and its functions.
Copyright   : (c) Jörg K.-H. W. Brüggmann, 2021-2022
License     : proprietary, to be dual licensed
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX

...provides the types 'Octet' ('Word8'), and 'Octets' as lists of 'Octet' and its functions.

* characteristics:

    * compatible with module 'FileSystem'

* example

    * the code below

        * takes the file "c.txt" as 'Octets'
        * puts the converted 'CharUtf8' characters to file "d.txt", which have been converted from 'Octets'
        * puts the converted 'Char' characters to stdout, which have been converted from 'CharUtf8'

    @
import qualified Octets as Oct
import qualified FileSystem as FS

main :: IO ()
main = 
    do
        FS.processData
            (FS.DataReader (FS.FileInp ".\\test\\c.txt") (Oct.getOctetsDecoded id) [])
            [
                FS.DataWriter (FS.FileOut ".\\test\\d.txt") (Oct.putOctetsEncoded octLatin1ToUtf8), 
                FS.DataWriter FS.StdOut putStringFromLatin1
            ]
        putStrLn ""

octLatin1ToUtf8 :: Oct.Octets -> Oct.Octets
octLatin1ToUtf8 loct = Oct.encodeList ((Cd.convertList ((Oct.decodeList loct) :: [Cd.CharIso1])) :: [Cd.CharUtf8])

putStringFromLatin1 :: FS.Handle -> Oct.Octets -> IO ()
putStringFromLatin1 h loct = FS.hPutStr h ((Cd.convertList ((Oct.decodeList loct) :: [Cd.CharIso1])) :: [Char])
    @

    * output:

    @
Øresund
    @
-}

{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Octets
    (
        -- * types
        -- ** 'Octet'
        Octet,
        -- ** 'Octets'
        Octets,
        -- * functions
        -- ** stream IO via Handle
        getOctetsDecoded,
        putOctetsEncoded,
        -- ** regarding 'Integral'
        loctFromIntegral,
        integralFromOctets,
        -- ** algebraic sign
        keepOctetsPositive,
        keepOctetsNegative,
        -- ** manipulation of 'Octets'
        loctFix,
        loctShiftOctR,
        -- ** bit manipulation
        loctBitField,
        loctBitFieldTakeEnd,
        octClearMsbs,
        loctBitShiftR,
        octBitMaskL,
        -- ** to be ignored - for test purposed only
        lTestCasesTuple2
    ) where

import qualified Data.Word as W
import qualified System.IO as SysIo
import qualified Data.ByteString as BS
import qualified Data.Bits as Bts hiding ((.|.), (.&.))
import           Data.Bits ((.|.), (.&.))

import qualified Safer as Sfr hiding ((.>>), (<<.))
import           Safer ((.>>), (<<.))
import qualified List as Lst


import qualified TestCaseWrap as TCW


--------------------------------------------------------------------------------
--  'Octet' and 'Octets'

type Octet = W.Word8

type Octets = [Octet]

--------------------------------------------------------------------------------
--  stream IO via Handle

-- getOctetsDecoded...
-- | ...loads a list of objects from file or stream via handle.
{-|
* uses the function (first parameter) to decode octets to list of anything
* if 'id' is applied as first parameter then the function loads octets
-}
getOctetsDecoded
    :: (Octets -> [a])  -- ^ function that decodes the octets to a list of anything
    -> SysIo.Handle     -- ^ input handle
    -> IO [a]           -- ^ 'IO' result
getOctetsDecoded fDecode hInp =
    do
        loct <- fmap BS.unpack (BS.hGetContents hInp)
        return (fDecode loct)

-- putOctetsEncoded...
-- | ...saves a list of objects to file or stream via handle.
{-|
* uses the function (first parameter) to encode list of anything to octets
* if 'id' is applied as first parameter then the function saves octets
-}
putOctetsEncoded
    :: ([a] -> Octets)  -- ^ function that encodes the list of anything to octets
    -> SysIo.Handle     -- ^  ouput handle
    -> [a]              -- ^ list of anything
    -> IO ()            -- ^ 'IO'
putOctetsEncoded fEncode hOut lx = BS.hPut hOut (BS.pack (fEncode lx))


--------------------------------------------------------------------------------
--  regarding 'Integral'

-- loctFromIntegral...
-- | ...converts an integral into 'octets'.
{-|
* preserves the most significant bit as algebraic sign
-}
loctFromIntegral :: (Integral a) => a -> Octets
loctFromIntegral n
    | n > 0 =
        let (_, lw8) = loctFromIntegral' (n, [])
        in keepOctetsPositive lw8
    | n < 0 =
        let (_, lw8) = loctFromIntegral' (negate n - 1, [])
        in (keepOctetsNegative . fmap Bts.complement) lw8
    | otherwise = []

loctFromIntegral' :: (Integral a) => (a,Octets) -> (a,Octets)
loctFromIntegral' tpl@(0,_) =
    tpl
loctFromIntegral' (n,lw) =
    loctFromIntegral' (n `div` 256, fromIntegral (n `mod` 256) : lw)

integralFromOctets :: (Integral a) => Octets -> a
integralFromOctets lw8
    | Bts.testBit (head lw8) 7 = (negate . (+1) . integralFromOctets . fmap Bts.complement) lw8
    | otherwise                = foldl (\n w8 -> (n * 256) + fromIntegral w8)  0 lw8

--------------------------------------------------------------------------------
-- algebraic sign

keepOctetsPositive
    :: Octets
    -> Octets
keepOctetsPositive [] = []
keepOctetsPositive lw8
    | Bts.testBit (head lw8) 7 = 0 : lw8 -- highest bit is set, hence would be interpreted as negative by integralFromOctets -> append 0x00 to keep positive
    | otherwise                =     lw8 -- highest bit is NOT set, hence positive -> keep as is

keepOctetsNegative
    :: Octets
    -> Octets
keepOctetsNegative [] = []
keepOctetsNegative lw8
    | Bts.testBit (head lw8) 7 =        lw8 -- highest bit is set, hence would be interpreted as negative by integralFromOctets -> append 0x00 to keep positive
    | otherwise                = 0xff : lw8 -- highest bit is NOT set, hence positive -> keep as is


--------------------------------------------------------------------------------
--  manipulation of 'Octets'

-- loctFix...
-- | ...assures that a list of octets has a defined length.
{- |
* if length of given octets (loct) is equal to first parameter (niLen) then octets are just passed as is
* if length of given octets (loct) is less than niLen then octets will be padded with 0 as head just to get the length equal to niLen
* if length of given octets (loct) is smaller than niLen then octets will be take at the end just to get the length equal to niLen

    @
print $ loctFix 0 [1,2,3] -- prints: []
print $ loctFix 1 [1,2,3] -- prints: [3]
print $ loctFix 2 [1,2,3] -- prints: [2,3]
print $ loctFix 3 [1,2,3] -- prints: [1,2,3]
print $ loctFix 4 [1,2,3] -- prints: [0,1,2,3]
print $ loctFix 5 [1,2,3] -- prints: [0,0,1,2,3]
print $ loctFix 3 [] -- prints: [0,0,0]
    @
-}
loctFix
    :: Integer
    -> Octets
    -> Octets
loctFix niLen loct = Sfr.pad niLen 0 (Lst.takeEnd niLen loct)


--------------------------------------------------------------------------------
-- bit manipulation

-- loctBitField...
-- | ...takes a field of bits, provided in form of a list of octets.
{- |
* takes the bits position range
    * see first parameter
* head of octet list is most significant
* tail of octet list is least significant
-}
loctBitField
    :: (Integer, Integer)   -- ^ (from, to) where "from" is the index of the most significant bit and where "to" is the index of the least significant bit, 
                            -- ^ related to the complete octet list
    -> Octets               -- ^ octet list as source of bits
    -> Octets               -- ^ resulting bits, right aligned
loctBitField _ [] = []
loctBitField (niFrom, niTo) loct
    | niFrom' >= niTo' = loctBitFieldTakeEnd niFromShifted (loctBitShiftR niShift loct)
    | otherwise         = []
        where
            niIndMax = (Sfr.niLen loct * 8) - 1
            niFrom' = min niIndMax niFrom
            niTo' = max 0 niTo
            niShift = niTo'
            niFromShifted = niFrom' - niShift + 1

-- loctBitFieldTakeEnd...
-- | ...takes a field of bits, provided in form of a list of octets.
{- |
* takes the least significant bits
* takes the given amount bits
* head of octet list is most significant
* tail of octet list is least significant
-}
loctBitFieldTakeEnd
    :: Integer  -- ^ amount of bits to get from the complete octet list, right aligned
    -> Octets   -- ^ octet list as source of bits
    -> Octets   -- ^ resulting bits in octets
loctBitFieldTakeEnd ni loct
    | ni > 0                    = loctBitFieldTakeEnd' (_niOctBitReciprocal (ni - ((niAmountOctets - 1) * 8))) (Lst.takeEnd niAmountOctets loct)
    | otherwise {- ni <= 0 -}   = []
    where
        niAmountOctets = _niOctetsPerBit ni
        loctBitFieldTakeEnd' :: Integer -> Octets -> Octets
        loctBitFieldTakeEnd' _ [] = []
        loctBitFieldTakeEnd' niAmountOfBitsToClear loct'@(oct:lroct)
            | Sfr.niLen loct < niAmountOctets = loct'
            | otherwise                       = octClearMsbs niAmountOfBitsToClear oct : lroct

octClearMsbs :: Integer -> Octet -> Octet
octClearMsbs niBitsToClear oct' = (0b11111111 .>> niBitsToClear) .&. oct'

-- loctBitShiftR...
-- | ...shifts all bits within a list of octets.
{- |
* head of octet list is most significant
* tail of octet list is least significant
-}
loctBitShiftR
    :: Integer
    -> Octets
    -> Octets
loctBitShiftR _ [] = []
loctBitShiftR ni loct
    | ni >= 8                   = loctBitShiftR (ni `mod` 8) (loctShiftOctR (ni `div` 8) loct)
    | {- (ni < 8) && -} ni > 0  = _loctBitShiftR' ni 0x00 loct
    | otherwise {- ni > 0 -}    = loct

-- loctShiftOctR...
-- | ...shifts all octets to the right.
{- |
* keeps the amount of octets in the list
* to the right means to the least significant
* head of octet list is most significant
* tail of octet list is least significant
-}
loctShiftOctR
    :: Integer
    -> Octets
    -> Octets
loctShiftOctR = Lst.shiftR 0

octBitMaskL :: Integer -> Octet
octBitMaskL ni
    | (ni > 0) && (ni < 8)  = 0b11111111 `Bts.shiftL` fromIntegral (_niOctBitReciprocal ni)
    | ni <= 0               = 0b00000000
    | otherwise             = 0b11111111


--------------------------------------------------------------------------------
-- module internal functions

-- _loctBitShiftR'...
-- | ...helper function for loctBitShiftR.
{- |
* works only for shifts of n bits, where n is between 0 and 8
* shifts all bits in the sequence of the second and third parameter to the right
    * as if the second and third parameter were one field of bits
* to the right means towards least significant bit
* least significant is the last element of the third parameter
* most significant is the second parameter
-}
_loctBitShiftR' :: Integer -> Octet -> Octets -> Octets
_loctBitShiftR' _ _ [] = []
_loctBitShiftR' ni oct0 (oct:lroct) = (((oct0 <<. _niOctBitReciprocal ni) .&. octBitMaskL ni) .|. (oct .>> ni)) : _loctBitShiftR' ni oct lroct

-- _niOctetsPerBit...
-- | ...computes the amount of octets need to contain the given amount of bits.
_niOctetsPerBit :: Integer -> Integer
_niOctetsPerBit niBits
    | niBits >= 0   = ((niBits - 1) `div` 8) + 1
    | otherwise     = 0

-- _niOctBitReciprocal...
-- | ...computes the remaining of bits of an octet when the given amount is subtracted.
_niOctBitReciprocal
    :: Integer  -- ^ amount of bits to be subtracted
    -> Integer  -- ^ amount of remaining bits
_niOctBitReciprocal ni
    | ni <=  0                  = 0
    | {- (ni > 0) && -} ni <= 8 = 8 - ni
    | otherwise {- ni > 8 -}    = _niOctBitReciprocal (ni `mod` 8)


--------------------------------------------------------------------------------
--  unit tests of non exported functions

lTestCasesTuple2 :: ([TCW.TestCaseWrap Integer], [TCW.TestCaseWrap Octets])
lTestCasesTuple2 = (lTestCaseTpIntegerWrap, lTestCaseTpOctetsWrap)

lTestCaseTpOctetsWrap :: [TCW.TestCaseWrap Octets]
lTestCaseTpOctetsWrap =
    -- _niOctetsPerBit
    [TCW.TestAssertion "_loctBitShiftR' #1"   (_loctBitShiftR'    0 0 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) [0b10010110, 0b01101001, 0b01101001, 0b10010110],
    TCW.TestAssertion "_loctBitShiftR' #2"   (_loctBitShiftR'    1 0 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) [0b01001011, 0b00110100, 0b10110100, 0b11001011],
    TCW.TestAssertion "_loctBitShiftR' #3"   (_loctBitShiftR'    2 0 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) [0b00100101, 0b10011010, 0b01011010, 0b01100101],
    TCW.TestAssertion "_loctBitShiftR' #4"   (_loctBitShiftR'    3 0 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) [0b00010010, 0b11001101, 0b00101101, 0b00110010],
    TCW.TestAssertion "_loctBitShiftR' #5"   (_loctBitShiftR'    6 0 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) [0b00000010, 0b01011001, 0b10100101, 0b10100110],
    TCW.TestAssertion "_loctBitShiftR' #6"   (_loctBitShiftR'    7 0 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) [0b00000001, 0b00101100, 0b11010010, 0b11010011],
    TCW.TestAssertion "_loctBitShiftR' #7"   (_loctBitShiftR'    8 0 [0b10010110, 0b01101001, 0b01101001, 0b10010110]) [0b00000000, 0b10010110, 0b01101001, 0b01101001]]

lTestCaseTpIntegerWrap :: [TCW.TestCaseWrap Integer]
lTestCaseTpIntegerWrap =
    -- _niOctetsPerBit
    [TCW.TestAssertion "_niOctetsPerBit #1"   (_niOctetsPerBit (-2)) 0,
    TCW.TestAssertion "_niOctetsPerBit #2"   (_niOctetsPerBit (-1)) 0,
    TCW.TestAssertion "_niOctetsPerBit #3"   (_niOctetsPerBit    0) 0,
    TCW.TestAssertion "_niOctetsPerBit #4"   (_niOctetsPerBit    1) 1,
    TCW.TestAssertion "_niOctetsPerBit #5"   (_niOctetsPerBit    2) 1,
    TCW.TestAssertion "_niOctetsPerBit #6"   (_niOctetsPerBit    3) 1,
    TCW.TestAssertion "_niOctetsPerBit #7"   (_niOctetsPerBit    4) 1,
    TCW.TestAssertion "_niOctetsPerBit #8"   (_niOctetsPerBit    5) 1,
    TCW.TestAssertion "_niOctetsPerBit #9"   (_niOctetsPerBit    6) 1,
    TCW.TestAssertion "_niOctetsPerBit #10"  (_niOctetsPerBit    7) 1,
    TCW.TestAssertion "_niOctetsPerBit #11"  (_niOctetsPerBit    8) 1,
    TCW.TestAssertion "_niOctetsPerBit #12"  (_niOctetsPerBit    9) 2,
    TCW.TestAssertion "_niOctetsPerBit #13"  (_niOctetsPerBit   10) 2,
    TCW.TestAssertion "_niOctetsPerBit #14"  (_niOctetsPerBit   11) 2,
    TCW.TestAssertion "_niOctetsPerBit #15"  (_niOctetsPerBit   12) 2,
    TCW.TestAssertion "_niOctetsPerBit #16"  (_niOctetsPerBit   13) 2,
    TCW.TestAssertion "_niOctetsPerBit #17"  (_niOctetsPerBit   14) 2,
    TCW.TestAssertion "_niOctetsPerBit #18"  (_niOctetsPerBit   15) 2,
    TCW.TestAssertion "_niOctetsPerBit #19"  (_niOctetsPerBit   16) 2,
    TCW.TestAssertion "_niOctetsPerBit #20"  (_niOctetsPerBit   17) 3,
    TCW.TestAssertion "_niOctetsPerBit #21"  (_niOctetsPerBit   24) 3,
    TCW.TestAssertion "_niOctetsPerBit #22"  (_niOctetsPerBit   25) 4,
    TCW.TestAssertion "_niOctetsPerBit #23"  (_niOctetsPerBit   32) 4,
    TCW.TestAssertion "_niOctetsPerBit #24"  (_niOctetsPerBit   33) 5,
    -- _niOctBitReciprocal
    TCW.TestAssertion "_niOctBitReciprocal #1"  (_niOctBitReciprocal (-2)) 0,
    TCW.TestAssertion "_niOctBitReciprocal #2"  (_niOctBitReciprocal (-1)) 0,
    TCW.TestAssertion "_niOctBitReciprocal #3"  (_niOctBitReciprocal    0) 0,
    TCW.TestAssertion "_niOctBitReciprocal #4"  (_niOctBitReciprocal    1) 7,
    TCW.TestAssertion "_niOctBitReciprocal #5"  (_niOctBitReciprocal    2) 6,
    TCW.TestAssertion "_niOctBitReciprocal #6"  (_niOctBitReciprocal    3) 5,
    TCW.TestAssertion "_niOctBitReciprocal #7"  (_niOctBitReciprocal    4) 4,
    TCW.TestAssertion "_niOctBitReciprocal #8"  (_niOctBitReciprocal    5) 3,
    TCW.TestAssertion "_niOctBitReciprocal #9"  (_niOctBitReciprocal    6) 2,
    TCW.TestAssertion "_niOctBitReciprocal #10" (_niOctBitReciprocal    7) 1,
    TCW.TestAssertion "_niOctBitReciprocal #11" (_niOctBitReciprocal    8) 0,
    TCW.TestAssertion "_niOctBitReciprocal #12" (_niOctBitReciprocal    9) 7,
    TCW.TestAssertion "_niOctBitReciprocal #13" (_niOctBitReciprocal   10) 6,
    TCW.TestAssertion "_niOctBitReciprocal #14" (_niOctBitReciprocal   11) 5,
    TCW.TestAssertion "_niOctBitReciprocal #15" (_niOctBitReciprocal   15) 1,
    TCW.TestAssertion "_niOctBitReciprocal #16" (_niOctBitReciprocal   16) 0,
    TCW.TestAssertion "_niOctBitReciprocal #17" (_niOctBitReciprocal   17) 7,
    TCW.TestAssertion "_niOctBitReciprocal #18" (_niOctBitReciprocal   23) 1,
    TCW.TestAssertion "_niOctBitReciprocal #19" (_niOctBitReciprocal   24) 0,
    TCW.TestAssertion "_niOctBitReciprocal #20" (_niOctBitReciprocal   25) 7]
