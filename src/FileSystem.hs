{-|
Description : provides a generalised, abstract, and save interface to the files system.
Copyright   : (c) Jörg K.-H. W. Brüggmann, 2021-2022
License     : proprietary, to be dual licensed
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX

Provides a generalised, abstract, and save interface to the files system.

* characteristics:

    * abstraction:

        * uniform types for identification of input and output streams ('Inp', 'Out')

        * higher order functions encapsulated in data types ('DataReader', 'DataWriter') are applied on streams

    * low probability to cause runtime errors

        * can __not__ cause error by "invalid characters" (as long as the stream handler functions do not cause them)

        * works together with 'TypedCodeOctets' (e.g. 'getTypedCodeOctets', and 'putTypedCodeOctets')

        * works together with 'Octets' (e.g. 'getOctetsDecoded', and 'putTypedCodeOctets')

    * supports the following file system features:

        * deletion of files

        * processing of streams

            * reading ('Inp') pre-processed

            * writing pre-processed stream to many streams ('Out')

NOTE: There is no isolated read function like the write function, because this would lead to errors like "illegal operation (delayed read on closed handle)".
Background: The function hGetContents needs an open file handle, that may be closed before the lazy String has been evaluated.

* example

    @
import qualified System.Info as SI
import qualified FileSystem as FS

sFileA :: String
sFileB :: String
(sFileA, sFileB) 
    | SI.os == "mingw32" = (".\\test\\a.txt", ".\\test\\b.txt")
    | SI.os == "linux" = ("./test/a.txt", "./test/b.txt")
    | otherwise = ("./test/a.txt", "./test/b.txt")

main :: IO ()
main = 
        FS.processData
            (FS.DataReader (FS.FileInp sFileA) FS.hGetContents [])
            [
                FS.DataWriter (FS.FileOut sFileB) FS.hPutStr, 
                FS.DataWriter FS.StdOut FS.hPutStr, 
                FS.DataWriter FS.StdErr FS.hPutStr
            ]
    @

    * copies the file "a.txt" in ".\\test" to "b.txt"
    * uses different file pathes that fit to different operating systems
    * sends the file content of "a.txt" also to 'stdout', and 'stderr'
    * will __not__ cause uncaught exception caused by "invalid characters", regardles of the files content
    * see also
        * 'getTypedCodeOctets', and 'putTypedCodeOctets' in 'TypedCodeOctets'
        * 'getOctetsDecoded', and 'putOctetsEncoded' in 'Octets'
-}

module FileSystem (
        -- * Types
        -- ** 'Inp'
        Inp(..),
        -- ** 'Out'
        Out(..),
        -- ** 'DataReader'
        DataReader(..),
        -- ** 'DataWriter'
        DataWriter(..),
        -- ** 'Handle'
        SysIo.Handle,
        -- * Functions
        -- ** 'removeFileIfExists'
        removeFileIfExists,
        -- ** 'moveFileIfExists'
        moveFileIfExists,
        -- ** 'processData'
        processData,
        -- ** 'writeData'
        writeData,
        -- ** 'hGetContents'
        SysIo.hGetContents,
        -- ** 'hPutStr'
        SysIo.hPutStr
    ) where

import qualified System.IO as SysIo
import qualified System.IO.Error as Err
import qualified Control.Exception as Ex
import qualified System.Directory as Dir
--import qualified Control.Parallel as Par

-- input streams
data Inp =
   StdInp
 | FileInp String
 deriving (Eq, Show)

-- output streams
data Out =
   NullOut
 | StdOut
 | StdErr
 | FileOut String
 deriving (Eq, Show)

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists fileName = Dir.removeFile fileName `Ex.catch` handleExists
    where
        handleExists :: Err.IOError -> IO ()
        handleExists e | Err.isDoesNotExistError e = return ()
                       | otherwise = Ex.throwIO e

moveFileIfExists :: FilePath -> FilePath -> IO ()
moveFileIfExists fileName0 fileName1 = Dir.renameFile fileName0 fileName1 `Ex.catch` handleExists
    where
        handleExists :: Err.IOError -> IO ()
        handleExists e | Err.isDoesNotExistError e = return ()
                       | otherwise = Ex.throwIO e

data DataReader s =
    DataReader {
      -- | input stream
      rinp :: Inp,
      -- | function that transforms the input stream and write to 
      rfRead :: SysIo.Handle -> IO s,
      -- | default result stream if the input stream can be read
      rfDefault :: s }

data DataWriter s =
    DataWriter {
      -- | output stream
      rout :: Out,
      -- | function that takes data (second parameter) to the given the output stream (first parameter)
      rfWrite :: SysIo.Handle -> s -> IO () }

processData :: DataReader a -> [DataWriter a] -> IO ()
processData (DataReader StdInp f _) lwrt =
    do
        dt <- f SysIo.stdin
        processData' dt lwrt
processData (DataReader (FileInp sFileName) f dflt) lwrt =
    (
        do
            hFile <- SysIo.openFile sFileName SysIo.ReadMode
            dt <- f hFile
            processData' dt lwrt
            SysIo.hClose hFile
    )
    `Ex.catch` handleIOErr
        where
            handleIOErr :: Ex.SomeException -> IO ()
            handleIOErr _ = processData' dflt lwrt

processData' :: a -> [DataWriter a] -> IO ()
processData' _ [] = return ()
processData' x (wrt:lrwrt) =
    do
        writeData wrt x
        processData' x lrwrt

writeData :: DataWriter a -> a -> IO ()
writeData (DataWriter NullOut _) _ = return ()
writeData (DataWriter StdOut f) x = f SysIo.stdout x
writeData (DataWriter StdErr f) x = f SysIo.stderr x
writeData (DataWriter (FileOut sFileName) f) x =
    do
        hFile <- SysIo.openFile sFileName SysIo.WriteMode
        f hFile x
        SysIo.hClose hFile
