module Main where

import qualified Data.Pak as P
import           Data.Pak.Parse (parsePak)

import           Data.Maybe (fromMaybe)
import           Options.Applicative

main :: IO ()
main = do
  opt    <- execParser optP
  case opt of
    Opt mode (FPath f) -> do
      processed <- fmap (>>= P.processPak) (parsePak f)
      case processed of
        Left err -> putStrLn err
        Right pp -> doDump mode pp
 where
  optP = info (helper <*> opts)
    (  fullDesc
    <> progDesc
        "Utilities for reading and manipulating N64 Controller Pak data"
    <> header "cpak: giving you control(ler pak)" )

doDump Meta         pp = putStr (P.displayPak pp)
doDump (DumpNote i) pp = error "TODO: Implement dumpNote"

data Options = Opt
  { mode  :: Dump
  , input :: InputFile
  }

opts :: Parser Options
opts = Opt <$> pDump <*> pInput

data Dump = Meta | DumpNote Int | DumpAll

pDumpNote :: Parser Dump
pDumpNote =
  DumpNote <$> option auto
                ( long "dump-note" 
                <> short 'd'
                <> metavar "FILE INDEX" <> help "dump bytes of pak file" )

pDumpAll :: Parser Dump
pDumpAll = flag' DumpAll
  ( long "dump-all"
  <> help "dump bytes of all pak files" )

pDump :: Parser Dump
pDump = fromMaybe Meta <$> optional (pDumpNote <|> pDumpAll)
                                  

data InputFile = Stdin | FPath FilePath

fileInput :: Parser InputFile
fileInput = FPath <$> strOption
  (  long "file"
  <> short 'f'
  <> metavar "FILENAME"
  <> help "path to pak file" )

stdInput :: Parser InputFile
stdInput = flag' Stdin
  (  long "stdin"
  <> help "Read from stdin" )

pInput :: Parser InputFile
pInput = fileInput <|> stdInput
