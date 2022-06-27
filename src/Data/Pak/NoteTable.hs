module Data.Pak.NoteTable where

import           Control.Monad (when)

import           Data.Word
import           Data.Pak.Types
import           Data.N64Font

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS

data NoteTable a = NT { tableEntries :: [NoteTableEntry] }
  deriving Show

processNT :: NoteTable Parsed -> Either String (NoteTable Processed)
processNT (NT es) =
 do let numEntries = length es
    when (numEntries /= 16)
      (Left ("Expected 16 NoteTableEntries, but only found " ++ show numEntries))
    pure (NT processed)
 where
  processed = map decodeEntries onlyValid
  onlyValid = filter validStartPage es
  validStartPage nte = startPage nte >= 5

decodeEntries nte = nte { fileExt = decodeBS (fileExt nte)
                        , fileName = decodeBS (fileName nte)
                        }


-- | Each entry in the NoteTable is 32 bytes long and is meant to conform to
--   the following format
data NoteTableEntry = NTE { gameCode      :: ByteString
                            -- ^ The code for the game, 4 bytes ASCII

                          , publisherCode :: ByteString
                            -- ^ The code for the publisher, 2 bytes ASCII

                          , startPage     :: Word16
                            -- ^ The starting page for the note on the Pak

                          , statusByte    :: Word8
                            -- ^ No idea, 1 byte

                          , reservedByte  :: Word8
                            -- ^ No idea, 1 byte

                          , dataSum       :: Word16
                            -- ^ No idea, 2 bytes

                          , fileExt       :: ByteString
                            -- ^ Extension, 4 bytes custom encoded

                          , fileName      :: ByteString
                            -- ^ Extension, 16 bytes custom encoded
                          }
  deriving Show
