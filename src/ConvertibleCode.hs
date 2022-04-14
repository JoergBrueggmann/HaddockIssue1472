{-|
Description : provides a class to convert one code to another, in particular character codes.
Copyright   : (c) Jörg K.-H. W. Brüggmann, 2021-2022
License     : proprietary, to be dual licensed
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX

The module provides the class 'ConvertibleCode' to convert one code to another.

* tries to keep its symbolic value e.g. the symbol € is kept when converting from 'CharWin1' to 'CharUtf8'

* supported codes, all that are implemented in module 'Code' and the standard character code, which are:

    * Char
    * CharUtf8
    * CharIso1
    * CharWin1
    * Octets.Octet
    * Bool

* example

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

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BinaryLiterals #-}

module ConvertibleCode
    (
        -- * Classes
        -- ** 'ConvertibleCode'
        ConvertibleCode(..),
        -- * reimported from 'Code'
        -- ** Classes
        Cd.Code(..),
        -- ** Types
        -- Char implied
        Cd.CharUtf8(..),
        Cd.CharIso1(..),
        Cd.CharWin1(..),
        Oct.Octet(..)
        -- Bool implied
    ) where

import qualified Data.Char as Chr
import qualified Data.Bits as Bts hiding ((.|.), (.&.))
import           Data.Bits ((.|.), (.&.))
--import qualified System.IO as SysIo

import qualified Code as Cd
import qualified Octetable as Oct

import qualified TestCaseWrap as TCW

--------------------------------------------------------------------------------
--  class ConvertibleCode

class ConvertibleCode t1 t2 where
    -- cdConvert
    -- | ...converts code of type t1 to code of type t2.
    {-|
    * in case of invalid code ('Code.isValid') to be converted the replacement character ('Code.cdReplacement' :: t2) of the type to be converted is used
    * in case of code that can not be converted the replacement character is used as well, e.g. ('Code.cdConvert' '為') :: Code.CharCW1 translates to '?'
    -}
    cdConvert :: t1 -> t2
    -- convertList
    -- | ...converts lists of code of type t1 to lists of code of type t2.
    {-|
    * uses cdConvert to convert ('fmap' 'cdConvert')
    -}
    lcdConvert :: [t1] -> [t2]
    lcdConvert = fmap cdConvert

{-
    Matrix of 'ConvertibleCode'

      from
       |
       v
 to ->         Char  CharUtf8 CharIso1 CharWin1 Octet  Bool
    Char         1       2       3       4       5       6
    CharUtf8     7       8       9      10      11      12
    CharIso1    13      14      15      16      17      18
    CharWin1    19      20      21      22      23      24
    Octet       25      26      27      28      29      30
    Bool        31      32      33      34      35      36
-}

instance ConvertibleCode Char Char where                    -- 1
    cdConvert ch = ch

instance ConvertibleCode Char Cd.CharUtf8 where             -- 2
    cdConvert ch = Cd.cdFromChar ch

instance ConvertibleCode Char Cd.CharIso1 where             -- 3
    cdConvert ch = Cd.cdFromChar ch

instance ConvertibleCode Char Cd.CharWin1 where             -- 4
    cdConvert ch = Cd.cdFromChar ch

instance ConvertibleCode Char Oct.Octet where               -- 5
    cdConvert ch = Cd.cdFromChar ch

instance ConvertibleCode Char Bool where                    -- 6
    cdConvert ch = Cd.cdFromChar ch


instance ConvertibleCode Cd.CharUtf8 Char where             -- 7
    cdConvert cu8 = Cd.chFromCode cu8

instance ConvertibleCode Cd.CharUtf8 Cd.CharUtf8 where      -- 8
    cdConvert cu8 
        | Cd.isValid cu8 = cu8
        | otherwise      = Cd.cdReplacement

instance ConvertibleCode Cd.CharUtf8 Cd.CharIso1 where      -- 9
    cdConvert cu8 
        | Cd.isValid cu8 = (Cd.cdFromChar . Cd.chFromCode) cu8
        | otherwise      = Cd.cdReplacement

instance ConvertibleCode Cd.CharUtf8 Cd.CharWin1 where      -- 10
    cdConvert cu8 
        | Cd.isValid cu8 = (Cd.cdFromChar . Cd.chFromCode) cu8
        | otherwise      = Cd.cdReplacement

instance ConvertibleCode Cd.CharUtf8 Oct.Octet where        -- 11
    cdConvert cu8
        | Cd.isValid cu8 = (Cd.cdFromChar . Cd.chFromCode) cu8
        | otherwise      = Cd.cdReplacement

instance ConvertibleCode Cd.CharUtf8 Bool where             -- 12
    cdConvert cu8 
        | Cd.isValid cu8 = (Cd.cdFromChar . Cd.chFromCode) cu8
        | otherwise      = Cd.cdReplacement


instance ConvertibleCode Cd.CharIso1 Char where             -- 13
    cdConvert ci1
        | Cd.isValid ci1 = Cd.chFromCode ci1
        | otherwise      = Cd.cdReplacement

instance ConvertibleCode Cd.CharIso1 Cd.CharUtf8 where      -- 14
    cdConvert ci1
        | Cd.isValid ci1 = (Cd.cdFromChar . Cd.chFromCode) ci1
        | otherwise      = Cd.cdReplacement

instance ConvertibleCode Cd.CharIso1 Cd.CharIso1 where      -- 15
    cdConvert ci1
        | Cd.isValid ci1 = ci1
        | otherwise      = Cd.cdReplacement

instance ConvertibleCode Cd.CharIso1 Cd.CharWin1 where      -- 16
    cdConvert ci1
        | Cd.isValid ci1 = (Cd.cdFromChar . Cd.chFromCode) ci1
        | otherwise      = Cd.cdReplacement

instance ConvertibleCode Cd.CharIso1 Oct.Octet where        -- 17
    cdConvert ci1
        | Cd.isValid ci1 = (Cd.cdFromChar . Cd.chFromCode) ci1
        | otherwise      = Cd.cdReplacement

instance ConvertibleCode Cd.CharIso1 Bool where             -- 18
    cdConvert ci1
        | Cd.isValid ci1 = (Cd.cdFromChar . Cd.chFromCode) ci1
        | otherwise      = Cd.cdReplacement


instance ConvertibleCode Cd.CharWin1 Char where             -- 19
    cdConvert cw1
        | Cd.isValid cw1 = Cd.chFromCode cw1
        | otherwise      = Cd.cdReplacement

instance ConvertibleCode Cd.CharWin1 Cd.CharUtf8 where      -- 20
    cdConvert cw1
        | Cd.isValid cw1 = (Cd.cdFromChar . Cd.chFromCode) cw1
        | otherwise      = Cd.cdReplacement

instance ConvertibleCode Cd.CharWin1 Cd.CharIso1 where      -- 21
    cdConvert cw1
        | Cd.isValid cw1 = (Cd.cdFromChar . Cd.chFromCode) cw1
        | otherwise      = Cd.cdReplacement

instance ConvertibleCode Cd.CharWin1 Cd.CharWin1 where      -- 22
    cdConvert cw1
        | Cd.isValid cw1 = cw1
        | otherwise      = Cd.cdReplacement

instance ConvertibleCode Cd.CharWin1 Oct.Octet where        -- 23
    cdConvert cw1
        | Cd.isValid cw1 = (Cd.cdFromChar . Cd.chFromCode) cw1
        | otherwise      = Cd.cdReplacement

instance ConvertibleCode Cd.CharWin1 Bool where             -- 24
    cdConvert cw1
        | Cd.isValid cw1 = (Cd.cdFromChar . Cd.chFromCode) cw1
        | otherwise      = Cd.cdReplacement


instance ConvertibleCode Oct.Octet Char where               -- 25
    cdConvert oct = Cd.chFromCode oct

instance ConvertibleCode Oct.Octet Cd.CharUtf8 where        -- 26
    cdConvert oct = (Cd.cdFromChar . Cd.chFromCode) oct

instance ConvertibleCode Oct.Octet Cd.CharIso1 where        -- 27
    cdConvert oct = (Cd.cdFromChar . Cd.chFromCode) oct

instance ConvertibleCode Oct.Octet Cd.CharWin1 where        -- 28
    cdConvert oct = (Cd.cdFromChar . Cd.chFromCode) oct

instance ConvertibleCode Oct.Octet Oct.Octet where          -- 29
    cdConvert oct = oct

instance ConvertibleCode Oct.Octet Bool where               -- 30
    cdConvert oct = (Cd.cdFromChar . Cd.chFromCode) oct


instance ConvertibleCode Bool Char where                    -- 31
    cdConvert is = Cd.chFromCode is

instance ConvertibleCode Bool Cd.CharUtf8 where             -- 32
    cdConvert is = (Cd.cdFromChar . Cd.chFromCode) is

instance ConvertibleCode Bool Cd.CharIso1 where             -- 33
    cdConvert is = (Cd.cdFromChar . Cd.chFromCode) is

instance ConvertibleCode Bool Cd.CharWin1 where             -- 34
    cdConvert is = (Cd.cdFromChar . Cd.chFromCode) is

instance ConvertibleCode Bool Oct.Octet where               -- 35
    cdConvert is = (Cd.cdFromChar . Cd.chFromCode) is

instance ConvertibleCode Bool Bool where                    -- 36
    cdConvert is = is
