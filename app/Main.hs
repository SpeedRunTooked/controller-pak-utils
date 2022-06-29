module Main where

import qualified Data.Pak as P
import           Data.Pak.Parse (parsePak)
import           Data.Pak.DumpNote (dumpNote)

import qualified Data.ByteString.Lazy as BS

import Main.Options

main :: IO ()
main = do
  opt    <- getOpts
  case opt of
    Opt mode (FPath f) Stdout -> do
      processed <- fmap (>>= P.processPak) (parsePak f)
      case processed of
        Left err -> putStrLn err
        Right pp -> doDump mode pp

doDump Meta         pp = putStr (P.displayPak pp)
doDump (DumpNote i) pp =
  case dumpNote i pp of
    Left err -> putStr err
    Right bs -> BS.putStr bs
