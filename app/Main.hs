module Main where

import qualified Data.Pak as P
import           Data.Pak.Parse (parsePak)
import           Data.N64Font (decodeBS)
import           Data.Pak.Note.Utils (dumpNote, mioOffsets)

import qualified Data.ByteString.Lazy as BS

import qualified System.IO            as IO
import           System.IO (stdin, stdout, stderr)

import Main.Options

main :: IO ()
main = do
  opt    <- getOpts
  case opt of
    Opt mode af (FPath f) Stdout -> do
      let handle = stdout
      processed <- fmap (>>= P.processPak) (parsePak f)
      case processed of
        Left err -> IO.hPutStrLn stderr err
        Right pp -> doDump af handle mode pp

doDump _  _ DumpAll        _  = IO.hPutStr stderr "--dump-all not yet implemented"
doDump _  h Meta           pp = IO.hPutStr h (P.displayPak pp)
doDump af h (DumpPage i)   pp = dumpPage af h i pp
doDump _  h (DumpNote i)   pp =
  case dumpNote i pp of
    Left err -> IO.hPutStrLn stderr err
    Right bs -> BS.hPutStr h bs
doDump _  h (MIOOffsets i) pp =
  case mioOffsets i pp of
    Left err -> IO.hPutStrLn stderr err
    Right os -> IO.hPutStrLn h (makeMsg os i)
  where
    makeMsg [] i = "No MIO0 signatures were found in Note #" ++ show i
    makeMsg os i = "Note #" ++ show i ++ " had MIO0 signatures at bytes: " ++ show os

dumpPage :: Bool -> IO.Handle -> Int -> P.Pak P.Processed -> IO ()
dumpPage af h i (P.Pak _ _ _ pgs) =
  case P.getPage i pgs of
    Nothing -> IO.hPutStr stderr ("Could not find page # " ++ show i)
    Just bs -> BS.hPutStr h (font bs)
 where
  font bs
    | af        = decodeBS bs
    | otherwise = id bs
