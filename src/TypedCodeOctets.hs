{-|
Description : octets with code format/type information, in particular character set formats, and its functions.
Copyright   : (c) Jörg K.-H. W. Brüggmann, 2021
License     : proprietary, to be dual licensed
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX

Types and functions for octets with code format/type information ('TypedCodeOctets'), in particular character set formats, and its functions.

* supports:

    * octet lists ('Octets')
    * UTF-8
    * ISO 8859-1 (also called Latin-1)
    * Windows-1252

* example

    * takes the file ("Utf8Example4.txt") as 'Octets', detects the format/type and shows both the detected type and some octets, 
        limited to 77 character plus "...", written to stdout

    @
import qualified TypedCodeOctets as TCO
import qualified FileSystem as FS

main :: IO ()
main = 
    do
        FS.processData
            (FS.DataReader (FS.FileInp (".\\test\\Utf8Example4.txt")) (TCO.getTypedCodeOctets (TCO.CodeTypeSelfDetecting TCO.All)) TCO.NoOctets)
            [FS.DataWriter FS.StdOut putShowTypeOfCodeOctets]
        putStrLn ""

putShowTypeOfCodeOctets :: FS.Handle -> TCO.TypedCodeOctets -> IO ()
putShowTypeOfCodeOctets h tco = FS.hPutStr h ((take 77 (show tco)) ++ "...")
    @


    * output:

    @
Utf8CharOctets (Just False) [13,10,13,10,32,32,32,32,124,32,72,101,108,112,32...
    @
-}

{-# LANGUAGE BinaryLiterals #-}

module TypedCodeOctets
    (
        -- * Types
        -- ** 'TypedCodeOctets'
        TypedCodeOctets(..),
        -- ** 'CodeType'
        CodeType(..),
        -- ** 'MaybeLimitedAmount'
        MaybeLimitedAmount(..),
        -- * Functions
        -- ** releated to 'CodeType' and 'TypedCodeOctets'
        getTypedCodeOctets,
        putTypedCodeOctets,
        toTypedCodeOctets,
        detectBOM,
        detectEncoding,
        addUtf8BOM,
        -- ** retated to 'MaybeLimitedAmount'
        mlaSubtract,
        takeMLA,
        isMlaPositive,
        -- ** to be ignored - for test purposed only
        lTestCasesTuple2 -- to export test cases of internal functions, can be unwraped in Spec.hs by using TestCaseUnwrap.hs
    ) where

--import qualified Data.Word as W
import qualified System.IO as SysIo
import qualified Data.ByteString as BS
import qualified Data.Bits as Bts hiding ((.|.), (.&.))
import           Data.Bits ((.|.), (.&.))

--import qualified Safer as Sfr
import qualified Octets as Oct
import qualified ConvertibleCode as Cd
import qualified TestCaseWrap as TCW

--import Debug.Trace


--------------------------------------------------------------------------------
--  main types

-- CodeType...
-- | ...indicator for codes like ISO 8859-1 (also called Latin-1) or UTF-8 characters
{-|
* prefix: cdt
* sum type for all codes that are supported by this library, supports:

    * plain 'Octets' ('Word8')
    * UTF-8
    * ISO 8859-1
    * Windows-1252
-}
data CodeType =
    -- | type is not predetermined, and shall be detected; 
    --   samples to detect are according to parameter :: 'MaybeLimitedAmount', either ('Limited' n ) or all octets ('All' :: 'MaybeLimitedAmount')
      CodeTypeSelfDetecting MaybeLimitedAmount
    -- | type is just octets
    | CodeTypeOctet
    -- | type is UTF-8; 
    --   is with BOM according to parameter :: 'IsWithBOM', True (with BOM) or False (without BOM)
    | CodeTypeUtf8Char IsWithBOM
    -- | type is ISO 8859-1
    | CodeTypeCI1Char
    -- | type is Windows-1252
    | CodeTypeCW1Char
    deriving (Show, Eq)

-- TypedCodeOctets...
-- | ...octets with coding type information
{-| 
* prefix: tco
* sum type for all codes that are supported by this library
-}
data TypedCodeOctets =
    -- | no octets at all, or not of the expected format/type
      NoOctets
    -- | just octets; 
    --   includes the payload of octets according to parameter :: 'Octets'
    | JustOctets Oct.Octets
    -- | octets of format/type UTF-8; 
    --   includes whether it is with BOM according to parameter :: 'IsWithBOM', True (with BOM) or False (without BOM)
    --   includes the payload of octets according to parameter :: 'Octets'
    | Utf8CharOctets IsWithBOM Oct.Octets
    -- | octets of format/type ISO 8859-1; 
    --   includes the payload of octets according to parameter :: 'Octets'
    | CI1CharOctets Oct.Octets
    -- | octets of format/type Windows-1252; 
    --   includes the payload of octets according to parameter :: 'Octets'
    | CW1CharOctets Oct.Octets
    deriving (Show, Eq)

-- getTypedCodeOctets...
-- | ...read 'TypedCodeOctets' from file, and 'SysIo.Handle' respectively
{-| 
* dependent on the first parameter, it reads the 'TypedCodeOctets' differently
    * uses 'toTypedCodeOctets' to convert the octet stream
    * see 'toTypedCodeOctets' to understand how the first parameter influences the conversion
-}
getTypedCodeOctets
    :: CodeType             -- ^ expected CodeType
    -> SysIo.Handle         -- ^ file to read from, and 'SysIo.Handle' respectively
    -> IO TypedCodeOctets   -- ^ IO monad of read 'TypedCodeOctets'
getTypedCodeOctets cdtDflt hInp = toTypedCodeOctets cdtDflt <$> fmap BS.unpack (BS.hGetContents hInp)
    -- equals:
    --  do
    --      loct <- (fmap BS.unpack (BS.hGetContents hInp))
    --      return (toTypedCodeOctets cdtDflt loct)

-- toTypedCodeOctets...
-- | ...takes octets and converts them to TypedCodeOctets
{-| 
* dependent on the first parameter it converts the octets differently
* cases:

    * CodeTypeSelfDetecting then the encoding will be detected by 'detectEncoding'

        * see how 'detectEncoding' operates
    * CodeTypeOctet then the encoding will be set to JustOctets
    * CodeTypeUtf8Char Nothing then the encoding will be set to Utf8CharOctets, and 
        the BOM will be detected automatically by detectBOM and removed from the octets
    * CodeTypeUtf8Char (Just True) then the encoding will be detected by detectBOM, and 
        only if the BOM is detetcted then the encoding will be set to Utf8CharOctets (Just True)
    * CodeTypeUtf8Char (Just False) then the encoding will be set to Utf8CharOctets (Just False), and 
        the first characters will be interpretetd as UTF-8, even if the first bytes is the BOM (EF, BB, BF; U+FEFF)
    * CodeTypeCI1Char then the encoding will be set to CI1CharOctets
    * CodeTypeCW1Char then the encoding will be set to CW1CharOctets
-}
toTypedCodeOctets
    :: CodeType         -- ^ expected CodeType
    -> Oct.Octets       -- ^ octets to be converted
    -> TypedCodeOctets  -- ^ to 'TypedCodeOctets' converted octets, as described
toTypedCodeOctets (CodeTypeSelfDetecting mniSamples) loct = detectEncoding mniSamples loct
toTypedCodeOctets CodeTypeOctet loct = JustOctets loct
toTypedCodeOctets (CodeTypeUtf8Char Nothing) loct =
    case cdtBOM of
        CodeTypeUtf8Char isWithBOM  ->  Utf8CharOctets isWithBOM loctWitoutBOM
        _                           ->  NoOctets
    where
        (loctWitoutBOM, cdtBOM) = detectBOM loct
toTypedCodeOctets cdtDflt'@(CodeTypeUtf8Char (Just True)) loct =
    if cdtDflt' == cdtBOM
        then Utf8CharOctets (Just True) loctWitoutBOM
        else NoOctets
    where
        (loctWitoutBOM, cdtBOM) = detectBOM loct
toTypedCodeOctets (CodeTypeUtf8Char (Just False)) loct =
    Utf8CharOctets (Just False) loct
toTypedCodeOctets CodeTypeCI1Char loct = CI1CharOctets loct
toTypedCodeOctets CodeTypeCW1Char loct = CW1CharOctets loct

-- putTypedCodeOctets...
-- | ...puts 'TypedCodeOctets' to file, and 'SysIo.Handle' respectively
{- | 
* dependent on the constructor of the second parameter it saves differently

    * cases:

        * NoOctets                      -> it saves nothing
        * Utf8CharOctets (Just True)    -> it saves the contained octets with Byte Order Mark (BOM)
        * otherwise                     -> it just saves the contained octets

* further characteristics

    * will create an identical file copy if 'TypedCodeOctets' has been read with 'getTypedCodeOctets', and  
        if the expected 'CodeType' (that has been used in 'getTypedCodeOctets') meets the expectation if any
-}
putTypedCodeOctets
    :: SysIo.Handle     -- ^ file to save in, and 'SysIo.Handle' respectively
    -> TypedCodeOctets  -- ^ 'TypedCodeOctets' to save
    -> IO ()            -- ^ IO monad
putTypedCodeOctets _ NoOctets = return ()
putTypedCodeOctets hOut (JustOctets loct) = BS.hPut hOut (BS.pack loct)
putTypedCodeOctets hOut (Utf8CharOctets (Just True) loct) = BS.hPut hOut (BS.pack (addUtf8BOM loct))
putTypedCodeOctets hOut (Utf8CharOctets _ loct) = BS.hPut hOut (BS.pack loct)
putTypedCodeOctets hOut (CI1CharOctets loct) = BS.hPut hOut (BS.pack loct)
putTypedCodeOctets hOut (CW1CharOctets loct) = BS.hPut hOut (BS.pack loct)


--------------------------------------------------------------------------------
--  helper types

type IsWithBOM = Maybe Bool

-- MaybeLimitedAmount...
-- | ...amount of things that may be all or limited (0 or more)
{-| 
* prefix: mla
-}
data MaybeLimitedAmount = Limited Integer | All
    deriving (Show, Eq)

mlaSubtract :: Integer -> MaybeLimitedAmount -> MaybeLimitedAmount
mlaSubtract _ All = All
mlaSubtract niToSubtract (Limited niSize) = Limited (niSize - niToSubtract)

takeMLA :: MaybeLimitedAmount -> [a] -> [a]
takeMLA _ [] = []
takeMLA mla (x:rlx)
    | isMlaPositive mla = x : takeMLA (mlaSubtract 1 mla) rlx
    | otherwise = []

isMlaPositive :: MaybeLimitedAmount -> Bool
isMlaPositive All = True
isMlaPositive (Limited niSize) = niSize > 0


--------------------------------------------------------------------------------
--  helper functions related to CodeType

-- detectBOM...
-- | ...detects the code type from BOM, if any
{-| 
* consideres 'CodeTypeUtf8Char' if the octets begin with [0xEF, 0xBB, 0xBF] and U+FEFF, respectively
    * then it removes the the BOM, and provides the remaining octets in tuple
* otherwise it returns the default code type 'CodeTypeSelfDetecting All'
    * passes all octets in tuple
-}
detectBOM
    :: Oct.Octets               -- ^ octets to check
    -> (Oct.Octets, CodeType)   -- ^ tuple with: 1) remaining octets, and 2) detected code type, showing whether BOM has been detected
detectBOM (0xEF:0xBB:0xBF:lroct) = (lroct, CodeTypeUtf8Char (Just True))
detectBOM loct = (loct, CodeTypeSelfDetecting All)

-- addUtf8BOM...
-- | ...adds the UTF-8 BOM to octets
{-| 
* the UTF-8 byte order mark (UTF-8 BOM) is [0xEF, 0xBB, 0xBF] and U+FEFF, respectively
-}
addUtf8BOM
    :: Oct.Octets   -- ^ octets of input
    -> Oct.Octets   -- ^ octets with added BOB at the head of the octets
addUtf8BOM loct = 0xEF:0xBB:0xBF:loct

-- detectEncoding...
-- | ...tries to detect encoding
{-| 
* useful when encoding is undefined and if there is a need to assume one
* takes the sample size of octets to detect
* uses the following criteria

    * consideres UTF-8 if the sequence starts with a UTF-8 BOM according to 'detectBOM'
    * consideres UTF-8 if the code sequences are according to UTF-8
    * consideres ISO 8859-1 when there is no use of printable Windows-1252 codes between 0x80 and 0x9F

* if BOM is detected then it removes the BOM

    * and provides the remaining octets
-}
detectEncoding
    :: MaybeLimitedAmount  -- ^ either 'Limited' sample size in octets, or all ('All')
    -> Oct.Octets       -- ^ octets of input
    -> TypedCodeOctets  -- ^ to 'TypedCodeOctets' converted octets, as described
detectEncoding mniSamples loct =
    if cdtBOM == CodeTypeUtf8Char (Just True)
        then Utf8CharOctets (Just True) loctBOM
        else detectEncodingAssumingNoBOM mniSamples loct
    where
        (loctBOM, cdtBOM) = detectBOM (takeMLA mniSamples loct)

-- detectEncodingAssumingNoBOM...
-- | ...tries to detect encoding, assuming there is no BOM
{-|
* consideres UTF if the code sequences are according to UTF-8
* consideres ISO 8859-1 when there is no use of printable Windows-1252 codes between 0x80 and 0x9F
-}
detectEncodingAssumingNoBOM
    :: MaybeLimitedAmount  -- ^ either 'Limited' sample size in octets, or all ('All')
    -> Oct.Octets       -- ^ octets of input
    -> TypedCodeOctets  -- ^ to 'TypedCodeOctets' converted octets, as described
detectEncodingAssumingNoBOM mniSamples loct =
    if isSequenceAccordingToUtf8 mniSamples loct
        then Utf8CharOctets (Just False) loct
        else detectEncodingAssumingNotUtf8 loct

-- detectEncodingAssumingNotUtf8...
-- | ...tries to detect encoding, assuming there is no UTF-8
{-|
* consideres ISO 8859-1 when there is no use of printable Windows-1252 codes between 0x80 and 0x9F
-}
detectEncodingAssumingNotUtf8
    :: Oct.Octets       -- ^ octets of input
    -> TypedCodeOctets  -- ^ to 'TypedCodeOctets' converted octets, as described
detectEncodingAssumingNotUtf8 loct =
    if isPrintableWindows1252 loct
        then CW1CharOctets loct
        else CI1CharOctets loct

-- isPrintableWindows1252...
-- | ...checks whether there is any printable character
-- | according to Windows-1252 encoding in the range between 0x80 and ox 9F
isPrintableWindows1252
    :: Oct.Octets       -- ^ octets of input
    -> Bool             -- ^ whether there is any of such characters
isPrintableWindows1252 = any (\oct -> oct >= 0x80 && oct <= 0x9F && (Cd.cdConvert (Cd.CharWin1 oct) :: Char) /= Cd.cdReplacement)

-- isSequenceAccordingToUtf8...
-- | ...checks whether the octet sequences are according to UTF-8
{-|
* complete characters follow complete characters
* consideres 4-byte character if octet starts with pattern 0b11110xxx, and the remaining 3 bytes are according pattern 0b10xxxxxx
* consideres 3-byte character if octet starts with 0b1110xxxx, and the remaining 2 bytes are according pattern 0b10xxxxxx
* consideres 2-byte character if octet starts with 0b110xxxxx, and the remaining byte is according pattern 0b10xxxxxx
* consideres 1-byte character if octet is between 0x00 and 0x7F
* it's unicode value is between U+0000 and U+10FFFF
* incomplete sequences and other pattern lead to False as result
-}
isSequenceAccordingToUtf8
    :: MaybeLimitedAmount  -- ^ either 'Limited' maximal sample size in octets, or all if the parameter is 'All'
    -> Oct.Octets       -- ^ input as octets 
    -> Bool             -- ^ True if the input is completely according to UTF-8 sequences (1-byte, 2-byte, 3-byte, and 4-byte sequences)
isSequenceAccordingToUtf8 _ [] = True
isSequenceAccordingToUtf8 mla (oct:lroct)
    | isMlaPositive mla && oct .&. 0b11111000 == 0b11110000   = isSequenceRemaining 3 lroct && isSequenceAccordingToUtf8 (mlaSubtract 4 mla) (drop 3 lroct)   -- 4-byte sequence
    | isMlaPositive mla && oct .&. 0b11110000 == 0b11100000   = isSequenceRemaining 2 lroct && isSequenceAccordingToUtf8 (mlaSubtract 3 mla) (drop 2 lroct)   -- 3-byte sequence
    | isMlaPositive mla && oct .&. 0b11100000 == 0b11000000   = isSequenceRemaining 1 lroct && isSequenceAccordingToUtf8 (mlaSubtract 2 mla) (drop 1 lroct)   -- 2-byte sequence
    | isMlaPositive mla && oct .&. 0b10000000 == 0b00000000   = isSequenceAccordingToUtf8 (mlaSubtract 1 mla) lroct                                               -- ASCII

    | otherwise                                                     = False                                                                                             -- invalid
{-
 new approach is needed, may be making use of type class Code
isSequenceAccordingToUtf8
    :: MaybeLimitedAmount  -- ^ either 'Limited' maximal sample size in octets, or all if the parameter is 'All'
    -> Oct.Octets       -- ^ input as octets 
    -> Bool             -- ^ True if the input is completely according to UTF-8 sequences (1-byte, 2-byte, 3-byte, and 4-byte sequences)
isSequenceAccordingToUtf8 _ [] = True
isSequenceAccordingToUtf8 mla loct@(oct:lroct)
    | oct .&. 0b11111000 == 0b11110000 && isMlaGreaterEqual mla 4 && is= 
    | otherwise = 
-}

isSequenceRemaining
    :: Integer
    -> Oct.Octets
    -> Bool
isSequenceRemaining 0 _ = True
isSequenceRemaining n [] = n == 0
isSequenceRemaining n (oct:lroct) = oct .&. 0b11000000 == 0b10000000 && isSequenceRemaining (n - 1) lroct


--------------------------------------------------------------------------------
--  unit tests of non exported functions

lTestCasesTuple2 :: ([TCW.TestCaseWrap Oct.Octets], [TCW.TestCaseWrap Bool])
lTestCasesTuple2 = (lTestCaseTplOctetsWrap, lTestCaseBoolWrap)

lTestCaseTplOctetsWrap :: [TCW.TestCaseWrap Oct.Octets]
lTestCaseTplOctetsWrap =
    [TCW.TestAssertion  "takeMLA #1" (takeMLA All [0x1A, 0x1B, 0x1C]) [0x1A, 0x1B, 0x1C],
    TCW.TestAssertion  "takeMLA #2" (takeMLA All [0x2A, 0x2B]) [0x2A, 0x2B],
    TCW.TestAssertion  "takeMLA #3" (takeMLA All [0x3A]) [0x3A],
    TCW.TestAssertion  "takeMLA #4" (takeMLA All []) [],
    TCW.TestAssertion  "takeMLA #5" (takeMLA (Limited 0) [0x1A, 0x1B, 0x1C]) [],
    TCW.TestAssertion  "takeMLA #6" (takeMLA (Limited 0) [0x2A, 0x2B]) [],
    TCW.TestAssertion  "takeMLA #7" (takeMLA (Limited 0) [0x3A]) [],
    TCW.TestAssertion  "takeMLA #8" (takeMLA (Limited 0) []) [],
    TCW.TestAssertion  "takeMLA #9" (takeMLA (Limited 1) [0x1A, 0x1B, 0x1C]) [0x1A],
    TCW.TestAssertion "takeMLA #10" (takeMLA (Limited 1) [0x2A, 0x2B]) [0x2A],
    TCW.TestAssertion "takeMLA #11" (takeMLA (Limited 1) [0x3A]) [0x3A],
    TCW.TestAssertion "takeMLA #12" (takeMLA (Limited 1) []) [],
    TCW.TestAssertion "takeMLA #13" (takeMLA (Limited 2) [0x1A, 0x1B, 0x1C]) [0x1A, 0x1B],
    TCW.TestAssertion "takeMLA #14" (takeMLA (Limited 2) [0x2A, 0x2B]) [0x2A, 0x2B],
    TCW.TestAssertion "takeMLA #15" (takeMLA (Limited 2) [0x3A]) [0x3A],
    TCW.TestAssertion "takeMLA #16" (takeMLA (Limited 2) []) [],
    TCW.TestAssertion "takeMLA #17" (takeMLA (Limited 3) [0x1A, 0x1B, 0x1C]) [0x1A, 0x1B, 0x1C],
    TCW.TestAssertion "takeMLA #18" (takeMLA (Limited 3) [0x2A, 0x2B]) [0x2A, 0x2B],
    TCW.TestAssertion "takeMLA #19" (takeMLA (Limited 3) [0x3A]) [0x3A],
    TCW.TestAssertion "takeMLA #20" (takeMLA (Limited 3) []) [],
    TCW.TestAssertion "takeMLA #21" (takeMLA (Limited 4) [0x1A, 0x1B, 0x1C]) [0x1A, 0x1B, 0x1C],
    TCW.TestAssertion "takeMLA #22" (takeMLA (Limited 4) [0x2A, 0x2B]) [0x2A, 0x2B],
    TCW.TestAssertion "takeMLA #23" (takeMLA (Limited 4) [0x3A]) [0x3A],
    TCW.TestAssertion "takeMLA #24" (takeMLA (Limited 4) []) [],
    TCW.TestAssertion "takeMLA #25" (takeMLA (Limited (-1)) [0x1A, 0x1B, 0x1C]) [],
    TCW.TestAssertion "takeMLA #26" (takeMLA (Limited (-1)) [0x2A, 0x2B]) [],
    TCW.TestAssertion "takeMLA #27" (takeMLA (Limited (-1)) [0x3A]) [],
    TCW.TestAssertion "takeMLA #28" (takeMLA (Limited (-1)) []) []]

lTestCaseBoolWrap :: [TCW.TestCaseWrap Bool]
lTestCaseBoolWrap =
    [TCW.TestAssertion "isSequenceAccordingToUtf8 #1" (isSequenceAccordingToUtf8 All [0xEF, 0xBB, 0xBF]) True,
    TCW.TestAssertion "isSequenceAccordingToUtf8 #2" (isSequenceAccordingToUtf8 All [0xEF, 0xBB]) False,
    TCW.TestAssertion "isSequenceAccordingToUtf8 #3" (isSequenceAccordingToUtf8 All [0xEF, 0xBB, 0xBF, 0x00]) True,
    TCW.TestAssertion "isSequenceAccordingToUtf8 #4" (isSequenceAccordingToUtf8 All [0xEF, 0xBB, 0xBF, 0x01]) True,
    TCW.TestAssertion "isSequenceAccordingToUtf8 #5" (isSequenceAccordingToUtf8 All [0xEF, 0xBB, 0xBF, 0x7E]) True,
    TCW.TestAssertion "isSequenceAccordingToUtf8 #6" (isSequenceAccordingToUtf8 All [0xEF, 0xBB, 0xBF, 0x7F]) True,
    TCW.TestAssertion "isSequenceAccordingToUtf8 #7" (isSequenceAccordingToUtf8 All [0xEF, 0xBB, 0xBF, 0x80]) False,
    TCW.TestAssertion "isSequenceAccordingToUtf8 #8" (isSequenceAccordingToUtf8 All [0xEF, 0xBB, 0xBF, 0x81]) False,
    TCW.TestAssertion "isSequenceAccordingToUtf8 #9" (isSequenceAccordingToUtf8 All [0xEF, 0xBB, 0xBF, 0xC0]) False,
    TCW.TestAssertion "isSequenceAccordingToUtf8 #10" (isSequenceAccordingToUtf8 All [0xEF, 0xBB, 0xBF, 0xC1]) False,
    TCW.TestAssertion "isSequenceAccordingToUtf8 #11" (isSequenceAccordingToUtf8 All [0xEF, 0xBB, 0xBF, 0xDE]) False,
    TCW.TestAssertion "isSequenceAccordingToUtf8 #12" (isSequenceAccordingToUtf8 All [0xEF, 0xBB, 0xBF, 0xDF]) False,
    TCW.TestAssertion "isSequenceAccordingToUtf8 #13" (isSequenceAccordingToUtf8 All [0xEF, 0xBB, 0xBF, 0xE0]) False,
    TCW.TestAssertion "isSequenceAccordingToUtf8 #14" (isSequenceAccordingToUtf8 All [0xEF, 0xBB, 0xBF, 0xE1]) False,
    TCW.TestAssertion "isSequenceAccordingToUtf8 #15" (isSequenceAccordingToUtf8 All [0xEF, 0xBB, 0xBF, 0xEE]) False,
    TCW.TestAssertion "isSequenceAccordingToUtf8 #16" (isSequenceAccordingToUtf8 All [0xEF, 0xBB, 0xBF, 0xEF]) False,
    TCW.TestAssertion "isSequenceAccordingToUtf8 #17" (isSequenceAccordingToUtf8 All [0xEF, 0xBB, 0xBF, 0xF0]) False,
    TCW.TestAssertion "isSequenceAccordingToUtf8 #18" (isSequenceAccordingToUtf8 All [0xEF, 0xBB, 0xBF, 0xF1]) False,
    TCW.TestAssertion "isSequenceAccordingToUtf8 #19" (isSequenceAccordingToUtf8 All [0xEF, 0xBB, 0xBF, 0xF6]) False,
    TCW.TestAssertion "isSequenceAccordingToUtf8 #20" (isSequenceAccordingToUtf8 All [0xEF, 0xBB, 0xBF, 0xF7]) False,
    TCW.TestAssertion "isSequenceAccordingToUtf8 #21" (isSequenceAccordingToUtf8 All [0xEF, 0xBB, 0xBF, 0xF8]) False,
    TCW.TestAssertion "isSequenceAccordingToUtf8 #22" (isSequenceAccordingToUtf8 All [0xEF, 0xBB, 0xBF, 0xFF]) False,
    TCW.TestAssertion "isSequenceAccordingToUtf8 #23" (isSequenceAccordingToUtf8 All [0x00]) True, -- on byte
    TCW.TestAssertion "isSequenceAccordingToUtf8 #24" (isSequenceAccordingToUtf8 All [0x01]) True, -- on byte
    TCW.TestAssertion "isSequenceAccordingToUtf8 #25" (isSequenceAccordingToUtf8 All [0x7E]) True, -- on byte
    TCW.TestAssertion "isSequenceAccordingToUtf8 #26" (isSequenceAccordingToUtf8 All [0x7F]) True, -- on byte
    TCW.TestAssertion "isSequenceAccordingToUtf8 #27" (isSequenceAccordingToUtf8 All [0x80]) False, -- just invalid
    TCW.TestAssertion "isSequenceAccordingToUtf8 #28" (isSequenceAccordingToUtf8 All [0x81]) False, -- just invalid
    TCW.TestAssertion "isSequenceAccordingToUtf8 #29" (isSequenceAccordingToUtf8 All [0x90]) False, -- just invalid
    TCW.TestAssertion "isSequenceAccordingToUtf8 #30" (isSequenceAccordingToUtf8 All [0x91]) False, -- just invalid
    TCW.TestAssertion "isSequenceAccordingToUtf8 #31" (isSequenceAccordingToUtf8 All [0x9E]) False, -- just invalid
    TCW.TestAssertion "isSequenceAccordingToUtf8 #32" (isSequenceAccordingToUtf8 All [0x9F]) False, -- just invalid
    TCW.TestAssertion "isSequenceAccordingToUtf8 #33" (isSequenceAccordingToUtf8 All [0xA0]) False, -- just invalid
    TCW.TestAssertion "isSequenceAccordingToUtf8 #34" (isSequenceAccordingToUtf8 All [0xA1]) False, -- just invalid
    TCW.TestAssertion "isSequenceAccordingToUtf8 #35" (isSequenceAccordingToUtf8 All [0xAE]) False, -- just invalid
    TCW.TestAssertion "isSequenceAccordingToUtf8 #36" (isSequenceAccordingToUtf8 All [0xAF]) False, -- just invalid
    TCW.TestAssertion "isSequenceAccordingToUtf8 #37" (isSequenceAccordingToUtf8 All [0xB0]) False, -- just invalid
    TCW.TestAssertion "isSequenceAccordingToUtf8 #38" (isSequenceAccordingToUtf8 All [0xB1]) False, -- just invalid
    TCW.TestAssertion "isSequenceAccordingToUtf8 #39" (isSequenceAccordingToUtf8 All [0xBE]) False, -- just invalid
    TCW.TestAssertion "isSequenceAccordingToUtf8 #40" (isSequenceAccordingToUtf8 All [0xBF]) False, -- just invalid
    TCW.TestAssertion "isSequenceAccordingToUtf8 #41" (isSequenceAccordingToUtf8 All [0xC0]) False, -- two bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #42" (isSequenceAccordingToUtf8 All [0xC1]) False, -- two bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #43" (isSequenceAccordingToUtf8 All [0xDE]) False, -- two bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #44" (isSequenceAccordingToUtf8 All [0xDF]) False, -- two bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #45" (isSequenceAccordingToUtf8 All [0xC0, 0x80]) True, -- two bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #46" (isSequenceAccordingToUtf8 All [0xC1, 0x80]) True, -- two bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #47" (isSequenceAccordingToUtf8 All [0xDE, 0x80]) True, -- two bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #48" (isSequenceAccordingToUtf8 All [0xDF, 0x80]) True, -- two bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #49" (isSequenceAccordingToUtf8 All [0xC0, 0x81]) True, -- two bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #50" (isSequenceAccordingToUtf8 All [0xC1, 0x81]) True, -- two bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #51" (isSequenceAccordingToUtf8 All [0xDE, 0x81]) True, -- two bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #52" (isSequenceAccordingToUtf8 All [0xDF, 0x81]) True, -- two bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #53" (isSequenceAccordingToUtf8 All [0xC0, 0xBE]) True, -- two bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #54" (isSequenceAccordingToUtf8 All [0xC1, 0xBE]) True, -- two bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #55" (isSequenceAccordingToUtf8 All [0xDE, 0xBE]) True, -- two bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #56" (isSequenceAccordingToUtf8 All [0xDF, 0xBE]) True, -- two bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #57" (isSequenceAccordingToUtf8 All [0xC0, 0xBF]) True, -- two bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #58" (isSequenceAccordingToUtf8 All [0xC1, 0xBF]) True, -- two bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #59" (isSequenceAccordingToUtf8 All [0xDE, 0xBF]) True, -- two bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #60" (isSequenceAccordingToUtf8 All [0xDF, 0xBF]) True, -- two bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #61" (isSequenceAccordingToUtf8 All [0xC0, 0xC0]) False, -- two bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #62" (isSequenceAccordingToUtf8 All [0xC1, 0xC0]) False, -- two bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #63" (isSequenceAccordingToUtf8 All [0xDE, 0xC0]) False, -- two bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #64" (isSequenceAccordingToUtf8 All [0xDF, 0xC0]) False, -- two bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #65" (isSequenceAccordingToUtf8 All [0xE0]) False, -- three bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #66" (isSequenceAccordingToUtf8 All [0xE1]) False, -- three bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #67" (isSequenceAccordingToUtf8 All [0xEE]) False, -- three bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #68" (isSequenceAccordingToUtf8 All [0xEF]) False, -- three bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #69" (isSequenceAccordingToUtf8 All [0xE0, 0x80]) False, -- three bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #70" (isSequenceAccordingToUtf8 All [0xE1, 0x80]) False, -- three bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #71" (isSequenceAccordingToUtf8 All [0xEE, 0x80]) False, -- three bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #72" (isSequenceAccordingToUtf8 All [0xEF, 0x80]) False, -- three bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #73" (isSequenceAccordingToUtf8 All [0xE0, 0x80, 0x80]) True, -- three bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #74" (isSequenceAccordingToUtf8 All [0xE1, 0x80, 0x80]) True, -- three bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #75" (isSequenceAccordingToUtf8 All [0xEE, 0x80, 0x80]) True, -- three bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #76" (isSequenceAccordingToUtf8 All [0xEF, 0x80, 0x80]) True, -- three bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #77" (isSequenceAccordingToUtf8 All [0xE0, 0x80, 0xBE]) True, -- three bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #78" (isSequenceAccordingToUtf8 All [0xE1, 0x80, 0xBE]) True, -- three bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #79" (isSequenceAccordingToUtf8 All [0xEE, 0x80, 0xBE]) True, -- three bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #80" (isSequenceAccordingToUtf8 All [0xEF, 0x80, 0xBE]) True, -- three bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #81" (isSequenceAccordingToUtf8 All [0xE0, 0x80, 0xBF]) True, -- three bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #82" (isSequenceAccordingToUtf8 All [0xE1, 0x80, 0xBF]) True, -- three bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #83" (isSequenceAccordingToUtf8 All [0xEE, 0x80, 0xBF]) True, -- three bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #84" (isSequenceAccordingToUtf8 All [0xEF, 0x80, 0xBF]) True, -- three bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #85" (isSequenceAccordingToUtf8 All [0xE0, 0x80, 0xC0]) False, -- three bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #86" (isSequenceAccordingToUtf8 All [0xE1, 0x80, 0xC0]) False, -- three bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #87" (isSequenceAccordingToUtf8 All [0xEE, 0x80, 0xC0]) False, -- three bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #88" (isSequenceAccordingToUtf8 All [0xEF, 0x80, 0xC0]) False, -- three bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #89" (isSequenceAccordingToUtf8 All [0xF0]) False, -- four bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #90" (isSequenceAccordingToUtf8 All [0xF1]) False, -- four bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #91" (isSequenceAccordingToUtf8 All [0xF6]) False, -- four bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #92" (isSequenceAccordingToUtf8 All [0xF7]) False, -- four bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #93" (isSequenceAccordingToUtf8 All [0xF0, 0x80]) False, -- four bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #94" (isSequenceAccordingToUtf8 All [0xF1, 0x80]) False, -- four bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #95" (isSequenceAccordingToUtf8 All [0xF6, 0x80]) False, -- four bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #96" (isSequenceAccordingToUtf8 All [0xF7, 0x80]) False, -- four bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #97" (isSequenceAccordingToUtf8 All [0xF0, 0x80, 0x80]) False, -- four bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #98" (isSequenceAccordingToUtf8 All [0xF1, 0x80, 0x80]) False, -- four bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #99" (isSequenceAccordingToUtf8 All [0xF6, 0x80, 0x80]) False, -- four bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #100" (isSequenceAccordingToUtf8 All [0xF7, 0x80, 0x80]) False, -- four bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #101" (isSequenceAccordingToUtf8 All [0xF0, 0x80, 0x80, 0x80]) True, -- four bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #102" (isSequenceAccordingToUtf8 All [0xF1, 0x80, 0x80, 0x80]) True, -- four bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #103" (isSequenceAccordingToUtf8 All [0xF6, 0x80, 0x80, 0x80]) True, -- four bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #104" (isSequenceAccordingToUtf8 All [0xF7, 0x80, 0x80, 0x80]) True, -- four bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #105" (isSequenceAccordingToUtf8 All [0xF0, 0x80, 0x80, 0xBE]) True, -- four bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #106" (isSequenceAccordingToUtf8 All [0xF1, 0x80, 0x80, 0xBE]) True, -- four bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #107" (isSequenceAccordingToUtf8 All [0xF6, 0x80, 0x80, 0xBE]) True, -- four bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #108" (isSequenceAccordingToUtf8 All [0xF7, 0x80, 0x80, 0xBE]) True, -- four bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #109" (isSequenceAccordingToUtf8 All [0xF0, 0x80, 0x80, 0xBF]) True, -- four bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #110" (isSequenceAccordingToUtf8 All [0xF1, 0x80, 0x80, 0xBF]) True, -- four bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #111" (isSequenceAccordingToUtf8 All [0xF6, 0x80, 0x80, 0xBF]) True, -- four bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #112" (isSequenceAccordingToUtf8 All [0xF7, 0x80, 0x80, 0xBF]) True, -- four bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #113" (isSequenceAccordingToUtf8 All [0xF0, 0x80, 0x80, 0xC0]) False, -- four bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #114" (isSequenceAccordingToUtf8 All [0xF1, 0x80, 0x80, 0xC0]) False, -- four bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #115" (isSequenceAccordingToUtf8 All [0xF6, 0x80, 0x80, 0xC0]) False, -- four bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #116" (isSequenceAccordingToUtf8 All [0xF7, 0x80, 0x80, 0xC0]) False, -- four bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #117" (isSequenceAccordingToUtf8 All [0xF0, 0x80, 0x80, 0x80, 0xE0, 0x80, 0x80, 0xC0, 0x81, 0x00]) True, -- four, three, two, one bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #118" (isSequenceAccordingToUtf8 All [0xF1, 0x80, 0x80, 0x80, 0xE1, 0x80, 0x80, 0xC1, 0x81, 0x01]) True, -- four, three, two, one bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #119" (isSequenceAccordingToUtf8 All [0xF6, 0x80, 0x80, 0x80, 0xEE, 0x80, 0x80, 0xDE, 0x81, 0x7E]) True, -- four, three, two, one bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #120" (isSequenceAccordingToUtf8 All [0xF7, 0x80, 0x80, 0x80, 0xEF, 0x80, 0x80, 0xDF, 0x81, 0x7F]) True, -- four, three, two, one bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #121" (isSequenceAccordingToUtf8 All [0xF0, 0x80, 0x80, 0xBE, 0xE0, 0x80, 0xBE, 0xC0, 0xBE, 0x00]) True, -- four, three, two, one bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #122" (isSequenceAccordingToUtf8 All [0xF1, 0x80, 0x80, 0xBE, 0xE1, 0x80, 0xBE, 0xC1, 0xBE, 0x01]) True, -- four, three, two, one bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #123" (isSequenceAccordingToUtf8 All [0xF6, 0x80, 0x80, 0xBE, 0xEE, 0x80, 0xBE, 0xDE, 0xBE, 0x7E]) True, -- four, three, two, one bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #124" (isSequenceAccordingToUtf8 All [0xF7, 0x80, 0x80, 0xBE, 0xEF, 0x80, 0xBE, 0xDF, 0xBE, 0x7F]) True, -- four, three, two, one bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #125" (isSequenceAccordingToUtf8 All [0xF0, 0x80, 0x80, 0xBF, 0xE0, 0x80, 0xBF, 0xC0, 0xBF, 0x00]) True, -- four, three, two, one bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #126" (isSequenceAccordingToUtf8 All [0xF1, 0x80, 0x80, 0xBF, 0xE1, 0x80, 0xBF, 0xC1, 0xBF, 0x01]) True, -- four, three, two, one bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #127" (isSequenceAccordingToUtf8 All [0xF6, 0x80, 0x80, 0xBF, 0xEE, 0x80, 0xBF, 0xDE, 0xBF, 0x7E]) True, -- four, three, two, one bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #128" (isSequenceAccordingToUtf8 All [0xF7, 0x80, 0x80, 0xBF, 0xEF, 0x80, 0xBF, 0xDF, 0xBF, 0x7F]) True, -- four, three, two, one bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #129" (isSequenceAccordingToUtf8 All [0xF0, 0x80, 0x80, 0xFF, 0xE0, 0x80, 0x80, 0xC0, 0x81, 0x00]) False, -- four, three, two, one bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #130" (isSequenceAccordingToUtf8 All [0xF1, 0x80, 0x80, 0x80, 0xE1, 0x80, 0xFF, 0xC1, 0x81, 0x01]) False, -- four, three, two, one bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #131" (isSequenceAccordingToUtf8 All [0xF6, 0x80, 0x80, 0x80, 0xEE, 0x80, 0x80, 0xDE, 0xFF, 0x7E]) False, -- four, three, two, one bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #132" (isSequenceAccordingToUtf8 All [0xF7, 0x80, 0x80, 0x80, 0xEF, 0x80, 0x80, 0xDF, 0x81, 0xFF]) False, -- four, three, two, one bytes
    TCW.TestAssertion "isSequenceAccordingToUtf8 #133" (isSequenceAccordingToUtf8 All [0xF8]) False, -- just invalid
    TCW.TestAssertion "isSequenceAccordingToUtf8 #134" (isSequenceAccordingToUtf8 All [0xFF]) False, -- just invalid
    TCW.TestAssertion "isPrintableWindows1252 #1" (isPrintableWindows1252 []) False,
    TCW.TestAssertion "isPrintableWindows1252 #2" (isPrintableWindows1252 [0x00]) False,
    TCW.TestAssertion "isPrintableWindows1252 #3" (isPrintableWindows1252 [0x01]) False,
    TCW.TestAssertion "isPrintableWindows1252 #4" (isPrintableWindows1252 [0x7E]) False,
    TCW.TestAssertion "isPrintableWindows1252 #5" (isPrintableWindows1252 [0x7F]) False,
    TCW.TestAssertion "isPrintableWindows1252 #6" (isPrintableWindows1252 [0x80]) True,
    TCW.TestAssertion "isPrintableWindows1252 #7" (isPrintableWindows1252 [0x81]) False,
    TCW.TestAssertion "isPrintableWindows1252 #8" (isPrintableWindows1252 [0x82]) True,
    TCW.TestAssertion "isPrintableWindows1252 #9" (isPrintableWindows1252 [0x8D]) False,
    TCW.TestAssertion "isPrintableWindows1252 #10" (isPrintableWindows1252 [0x8E]) True,
    TCW.TestAssertion "isPrintableWindows1252 #11" (isPrintableWindows1252 [0x8F]) False,
    TCW.TestAssertion "isPrintableWindows1252 #12" (isPrintableWindows1252 [0x90]) False,
    TCW.TestAssertion "isPrintableWindows1252 #13" (isPrintableWindows1252 [0x91]) True,
    TCW.TestAssertion "isPrintableWindows1252 #14" (isPrintableWindows1252 [0x9C]) True,
    TCW.TestAssertion "isPrintableWindows1252 #15" (isPrintableWindows1252 [0x9D]) False,
    TCW.TestAssertion "isPrintableWindows1252 #16" (isPrintableWindows1252 [0x9E]) True,
    TCW.TestAssertion "isPrintableWindows1252 #17" (isPrintableWindows1252 [0x9F]) True,
    TCW.TestAssertion "isPrintableWindows1252 #18" (isPrintableWindows1252 [0xA0]) False,
    TCW.TestAssertion "isPrintableWindows1252 #19" (isPrintableWindows1252 [0xA1]) False,
    TCW.TestAssertion "isPrintableWindows1252 #20" (isPrintableWindows1252 [0xFE]) False,
    TCW.TestAssertion "isPrintableWindows1252 #21" (isPrintableWindows1252 [0xFF]) False,
    TCW.TestAssertion "isPrintableWindows1252 #22" (isPrintableWindows1252 [0x00, 0x00]) False,
    TCW.TestAssertion "isPrintableWindows1252 #23" (isPrintableWindows1252 [0x00, 0x80]) True,
    TCW.TestAssertion "isPrintableWindows1252 #24" (isPrintableWindows1252 [0x00, 0x00, 0x00]) False,
    TCW.TestAssertion "isPrintableWindows1252 #25" (isPrintableWindows1252 [0x00, 0x00, 0x80]) True,
    TCW.TestAssertion "isPrintableWindows1252 #26" (isPrintableWindows1252 [0x00, 0x00, 0x00, 0x00]) False,
    TCW.TestAssertion "isPrintableWindows1252 #27" (isPrintableWindows1252 [0x00, 0x00, 0x00, 0x80]) True,
    TCW.TestAssertion "isPrintableWindows1252 #28" (isPrintableWindows1252 [0x00, 0x00, 0x00, 0x00, 0x00]) False,
    TCW.TestAssertion "isPrintableWindows1252 #29" (isPrintableWindows1252 [0x00, 0x00, 0x00, 0x00, 0x80]) True,
    TCW.TestAssertion "isPrintableWindows1252 #30" (isPrintableWindows1252 [0x00, 0x00, 0x00, 0x80, 0x00]) True,
    TCW.TestAssertion "isPrintableWindows1252 #31" (isPrintableWindows1252 [0x00, 0x00, 0x80, 0x00, 0x00]) True,
    TCW.TestAssertion "isPrintableWindows1252 #32" (isPrintableWindows1252 [0x00, 0x80, 0x00, 0x00, 0x00]) True,
    TCW.TestAssertion "isPrintableWindows1252 #33" (isPrintableWindows1252 [0x80, 0x00, 0x00, 0x00, 0x00]) True,
    TCW.TestAssertion "isPrintableWindows1252 #34" (isPrintableWindows1252 [0x00, 0x80, 0x80, 0x00, 0x00]) True,
    TCW.TestAssertion "isPrintableWindows1252 #35" (isPrintableWindows1252 [0x00, 0x80, 0x00, 0x80, 0x00]) True,
    TCW.TestAssertion "isPrintableWindows1252 #36" (isPrintableWindows1252 [0x00, 0x00, 0x80, 0x80, 0x00]) True,
    TCW.TestAssertion "isPrintableWindows1252 #37" (isPrintableWindows1252 [0x00, 0x80, 0x80, 0x80, 0x00]) True,
    TCW.TestAssertion "isPrintableWindows1252 #38" (isPrintableWindows1252 [0x80, 0x80, 0x80, 0x80, 0x00]) True,
    TCW.TestAssertion "isPrintableWindows1252 #39" (isPrintableWindows1252 [0x00, 0x80, 0x80, 0x80, 0x80]) True,
    TCW.TestAssertion "isPrintableWindows1252 #40" (isPrintableWindows1252 [0x80, 0x80, 0x80, 0x80, 0x80]) True]
