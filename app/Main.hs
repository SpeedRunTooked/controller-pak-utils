module Main where

import qualified Data.Pak as P
import           Data.Pak.Parse (parsePak)

import           System.Environment (getArgs)

main :: IO ()
main = do
  [f]    <- getArgs
  processed <- fmap (>>= P.processPak) (parsePak f)
  case processed of
    Left err -> putStrLn err
    Right pp -> putStr (P.displayPak pp)
