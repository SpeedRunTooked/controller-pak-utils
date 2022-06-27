module Data.Pak.Parse where

import           Control.Monad (unless)
import           Data.Word

import           Data.Binary.Get

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import           Data.N64Font
import           Data.Pak
import           Data.Pak.Types
import           Data.Pak.INodeTable
import           Data.Pak.NoteTable

parsePak :: FilePath -> IO (Either String (Pak Parsed))
parsePak path = 
 do conts <- BS.readFile path
    case runGetOrFail (isolate 32768 getPak) conts of
      Left (_, _, str)       -> pure (Left str)
      Right (_, offset, val) -> pure (Right val)

getPak :: Get (Pak Parsed)
getPak =
  do
    idSec  <- getIDSector
    iNodeT <- getINodeTable
    skip 256 -- skip second INodeTable
    noteT  <- getNoteTable
    rest   <- getRawPages 3
    pure (Pak idSec iNodeT noteT rest)

getIDSector :: Get ByteString
getIDSector = getLazyByteString 256


getRawPages :: Int -> Get RawPages
getRawPages sp =
  do
    rPages <- getRawPages'
    empty  <- isEmpty
    unless empty (fail nbytesError)
    pure (RP (IM.fromList (zip [sp..127] rPages)))

  where
    nbytesError = "getRawPages' did not consume the number of bytes we expected"

    getRawPages' =
      do
        empty <- isEmpty
        if empty
        then pure []
        else do page  <- getLazyByteString 256 -- Each page is 256 bytes
                pages <- getRawPages'
                pure (page:pages)

--getRawPagesFile :: FilePath -> IO (Either String RawPages)
getRawPagesFile file = BS.readFile file >>= pure . runGetOrFail (getRawPages 0)

getINodeTable :: Get (INodeTable Parsed)
getINodeTable =
  do
    fbyte <- getWord8

    -- The first byte is always meant to be 0
    unless (fbyte == 0)
           (fail "First byte of INode Table not null")

    check <- getWord8        -- The checksum byte
    skip 8                   -- next 8 bytes should be 0, but we don't care

    -- isolate the next decoder so that we only go until the end of the
    -- current page (256 - the 10 we've already decoded)
    inodes <- isolate 246 getINodes -- each INode is two bytes, and we should have
                                    -- 123 of them

    let calculated = toInteger (sum inodes `mod` 256)
    unless (calculated == toInteger check) (fail (checksumErrorMsg check calculated))

    pure (ITable check inodes [] [])

  where
    getINodes =
      do empty  <- isEmpty
         if empty
         then pure []
         else do
             inode  <- getWord16be
             inodes <- getINodes
             pure (inode:inodes)

    checksumErrorMsg c i = "INode table checksum failed: checksum = "
                        ++ show c
                        ++ " INode sum = "
                        ++ show i

getNoteTable :: Get (NoteTable Parsed)
getNoteTable = NT <$> sequence (replicate 16 getNoteTableEntry)

getNoteTableEntry :: Get NoteTableEntry
getNoteTableEntry = NTE <$> getLazyByteString 4
                        <*> getLazyByteString 2
                        <*> getWord16be
                        <*> getWord8
                        <*> getWord8
                        <*> getWord16be
                        <*> getLazyByteString 4
                        <*> getLazyByteString 16
