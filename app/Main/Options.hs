module Main.Options
  ( Options(..)
  , Dump(..)
  , InputFile(..)
  , OutputFile(..)
  , getOpts
  ) where

import           Data.Maybe (fromMaybe)
import           Options.Applicative

getOpts :: IO Options
getOpts = execParser optP
 where
  optP = info (helper <*> opts)
    (  fullDesc
    <> progDesc
        "Utilities for reading and manipulating N64 Controller Pak data"
    <> header "cpak: giving you control(ler pak)" )

data Options = Opt
  { mode   :: Dump
  , apFont :: Bool
  , input  :: InputFile
  , output :: OutputFile
  }

opts :: Parser Options
opts = Opt <$> pDump <*> pApFont <*> pInput <*> pOutput

data Dump = Meta | DumpNote Int | DumpPage Int | DumpAll

pDumpNote :: Parser Dump
pDumpNote =
  DumpNote <$> option auto
                ( long "dump-note" 
                <> short 'd'
                <> metavar "FILE-INDEX" <> help "dump bytes of pak file" )

pDumpPage :: Parser Dump
pDumpPage =
  DumpPage <$> option auto
                ( long "dump-page" 
                <> short 'p'
                <> metavar "PAGE-INDEX" <> help "dump bytes of single page" )

pDumpAll :: Parser Dump
pDumpAll = flag' DumpAll
  ( long "dump-all"
  <> help "dump bytes of all pak files" )

pDump :: Parser Dump
pDump = fromMaybe Meta <$> optional (pDumpNote <|> pDumpPage <|> pDumpAll)

pApFont :: Parser Bool
pApFont = switch
  (  long "apply-n64-font"
  <> short 'a'
  <> help "Wether to apply the N64 font mapping. Only affects dumping pages" )

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
pInput = fromMaybe Stdin <$> optional (fileInput <|> stdInput)

data OutputFile = Stdout | OFPath FilePath

fileOutput :: Parser OutputFile
fileOutput = OFPath <$> strOption
  (  long "out"
  <> short 'o'
  <> metavar "FILENAME"
  <> help "path to dump bytes file" )

stdOutput :: Parser OutputFile
stdOutput = flag' Stdout
  (  long "stdout"
  <> help "dump to stdout" )

pOutput :: Parser OutputFile
pOutput = fromMaybe Stdout <$> optional (fileOutput <|> stdOutput)
