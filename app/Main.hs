module Main where

import qualified Data.Pak as P
import           Data.Pak.Parse (parsePak)
import           Data.Pak.DumpNote (dumpNote)

import qualified Data.ByteString.Lazy as BS

import qualified System.IO            as IO
import           System.IO (stdin, stdout, stderr)

import Main.Options

main :: IO ()
main = do
  opt    <- getOpts
  case opt of
    Opt mode (FPath f) Stdout -> do
      let handle = stdout
      processed <- fmap (>>= P.processPak) (parsePak f)
      case processed of
        Left err -> IO.hPutStrLn stderr err
        Right pp -> doDump handle mode pp

doDump h Meta         pp = IO.hPutStr h (P.displayPak pp)
doDump h (DumpPage i) pp = dumpPage h i pp
doDump h (DumpNote i) pp =
  case dumpNote i pp of
    Left err -> IO.hPutStrLn stderr err
    Right bs -> BS.hPutStr h bs

dumpPage :: IO.Handle -> Int -> P.Pak P.Processed -> IO ()
dumpPage h i (P.Pak _ _ _ pgs) =
  case P.getPage i pgs of
    Nothing -> IO.hPutStr stderr ("Could not find page # " ++ show i)
    Just bs -> BS.hPutStr h bs
