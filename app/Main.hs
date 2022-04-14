{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE BlockArguments #-}

import qualified ConvertibleCode as Cd

import qualified Control.Monad.State as S
import qualified System.Info as SI

import Debug.Trace

import qualified TypedCodeOctets as TCO
import qualified OctetCodec as Oct
import qualified FileSystem as FS
import qualified Safer as Sfr

--import qualified Data.Bits as Bts hiding ((.|.), (.&.))
--import           Data.Bits ((.|.), (.&.))

sFileA :: String
sFileB :: String
sFileC :: String
sFileD :: String
sFileE :: String
sFileF :: String
sFileTestFolder :: String
(sFileA, sFileB, sFileC, sFileD, sFileE, sFileF, sFileTestFolder)
    | SI.os == "mingw32" = (".\\test\\a.txt", ".\\test\\b.txt", ".\\test\\c.txt", ".\\test\\d.txt", ".\\test\\e.txt", ".\\test\\f.txt", ".\\test\\")
    | SI.os == "linux" = ("./test/a.txt", "./test/b.txt", "./test/c.txt", "./test/d.txt", "./test/e.txt", "./test/f.txt", "./test/")
    | otherwise = ("./test/a.txt", "./test/b.txt", "./test/c.txt", "./test/d.txt", "./test/e.txt", "./test/f.txt", "./test/")

processTestFiles :: [String] -> IO ()
processTestFiles [] = return ()
processTestFiles (sTestFile:lrsTestFile) =
    do
        FS.processData
            (FS.DataReader (FS.FileInp (sFileTestFolder ++ sTestFile)) (TCO.getTypedCodeOctets (TCO.CodeTypeSelfDetecting TCO.All)) TCO.NoOctets)
            [FS.DataWriter FS.StdOut putShowTypeOfCodeOctets]
        putStrLn ""
        processTestFiles lrsTestFile

main :: IO ()
main =
    do
        FS.processData
            (FS.DataReader (FS.FileInp (sFileTestFolder ++ "Utf8Example4.txt")) (TCO.getTypedCodeOctets (TCO.CodeTypeSelfDetecting TCO.All)) TCO.NoOctets)
            [FS.DataWriter FS.StdOut putShowTypeOfCodeOctets]
        putStrLn ""
        processTestFiles (fmap (\n -> "Lat1Example" ++ show n ++ ".txt") [(0::Integer)..9])
        processTestFiles (fmap (\n -> "Utf8Example" ++ show n ++ ".txt") [(0::Integer)..9])
        processTestFiles (fmap (\n -> "Utf8BomExample" ++ show n ++ ".txt") [(0::Integer)..9])
        processTestFiles (fmap (\n -> "Win1252Example" ++ show n ++ ".txt") [(0::Integer)..9])
        putStrLn ""
        FS.processData
            (FS.DataReader (FS.FileInp sFileA) (Oct.getOctetsDecoded id) [])
            [
                FS.DataWriter (FS.FileOut sFileB) (Oct.putOctetsEncoded id),
                FS.DataWriter FS.StdOut hPutParseResult,
                FS.DataWriter FS.StdOut putShowStringFromUtf8
            ]
        putStrLn ""
        FS.processData
            (FS.DataReader (FS.FileInp sFileC) (Oct.getOctetsDecoded id) [])
            [
                FS.DataWriter (FS.FileOut sFileD) (Oct.putOctetsEncoded octLatin1ToUtf8),
                FS.DataWriter FS.StdOut putStringFromLatin1
            ]
        putStrLn ""
        FS.processData
            (FS.DataReader (FS.FileInp sFileD) (Oct.getOctetsDecoded id) [])
            [
                FS.DataWriter FS.StdOut putShowStringFromUtf8
            ]
        putStrLn ""
        FS.processData
            (FS.DataReader (FS.FileInp sFileE)
                (TCO.getTypedCodeOctets (TCO.CodeTypeSelfDetecting TCO.All))  -- reads code self detecting, take the whole file as samples
                (TCO.CI1CharOctets []))
            [
                FS.DataWriter (FS.FileOut sFileF) TCO.putTypedCodeOctets,
                FS.DataWriter FS.StdOut putShowCodeList
            ]
        putStrLn ""

octLatin1ToUtf8 :: Oct.Octets -> Oct.Octets
octLatin1ToUtf8 loct = Oct.encodeList (Cd.lcdConvert (Oct.decodeList loct :: [Cd.CharIso1]) :: [Cd.CharUtf8])

putStringFromLatin1 :: FS.Handle -> Oct.Octets -> IO ()
putStringFromLatin1 h loct = FS.hPutStr h (Cd.lcdConvert (Oct.decodeList loct :: [Cd.CharIso1]) :: [Char])

putShowStringFromUtf8 :: FS.Handle -> Oct.Octets -> IO ()
putShowStringFromUtf8 h loct = trace ("Oct.decodeList loct = " ++ show (Oct.decodeList loct :: [Cd.CharUtf8])) $ FS.hPutStr h (show (Cd.lcdConvert (Oct.decodeList loct :: [Cd.CharUtf8]) :: [Char]))

hPutParseResult :: FS.Handle -> Oct.Octets -> IO ()
hPutParseResult h oct = FS.hPutStr h (show (parse synHallo oct))

putShowCodeList :: FS.Handle -> TCO.TypedCodeOctets -> IO ()
putShowCodeList h lcd = FS.hPutStr h (show lcd)

putShowTypeOfCodeOctets :: FS.Handle -> TCO.TypedCodeOctets -> IO ()
putShowTypeOfCodeOctets h tco = FS.hPutStr h (take 77 (show tco) ++ "...")

data Syntax =
      SynTerminal Oct.Octet
    | SynSequence [Syntax]
    deriving Show

synCharTerminal :: Char -> Syntax
synCharTerminal ch = SynSequence (fmap SynTerminal ((Oct.toOctets . (Cd.cdConvert  :: Char -> Cd.CharUtf8)) ch))

synStringTerminal :: String -> Syntax
synStringTerminal s = SynSequence (fmap synCharTerminal s)

synHallo :: Syntax
synHallo = synStringTerminal "H⼈llöle!"


data Symbol =
      SymTerminal Oct.Octet
    | SymSequence
    deriving Show

type SymbolTree = SymbolNode

data SymbolNode = SymbolNode Symbol [SymbolNode]
    deriving Show

newtype ParseState = ParseState SymbolTree
    deriving Show

parse :: Syntax -> Oct.Octets -> Maybe SymbolTree
parse syn = S.evalState (parseS syn)

parseS :: Syntax -> S.State Oct.Octets (Maybe SymbolTree)
parseS (SynTerminal octSyn) =
    do
        moct <- Oct.takeMaybeS
        Sfr.ifJust moct
            {- then -} (\oct -> -- trace ((show oct) ++ " " ++ (show octSyn)) $ 
                do
                    if oct == octSyn
                        then return (Just (SymbolNode (SymTerminal oct) []))
                        else return Nothing)
            {- else -} (return Nothing)
parseS (SynSequence lsyn) =
    do
        mlsym <- parseS' lsyn
        Sfr.ifJust mlsym
            {- then -} (return . Just . SymbolNode SymSequence)
            {- else -} (return Nothing)

parseS' :: [Syntax] -> S.State Oct.Octets (Maybe [SymbolNode])
parseS' [] = return (Just [])
parseS' (syn:lrsyn) =
    do
        msym <- parseS syn
        Sfr.ifJust msym
            {- then -} (\sym ->
                do
                    mlsym <- parseS' lrsyn
                    Sfr.ifJust mlsym
                        {- then -} (\lsym -> return (Just (sym : lsym)))
                        {- else -} (return Nothing)
                    )
            {- else -} (return Nothing)
