module Data.Pak.Internal where

import           Data.Word

import           Data.ByteString.Lazy (ByteString)

import           Data.IntMap (IntMap)

import           Data.Pak.Types
import           Data.Pak.INodeTable
import           Data.Pak.NoteTable


-- | The `Pak` in its structured form
data Pak a = Pak { idSector   :: ByteString
                 , indexTable :: INodeTable a
                 , noteTabe   :: NoteTable a
                 , rawData    :: RawPages
                 }

-- | The Raw pages, one ByteString per page. This should probably never be used.
data RawPages = RP { rawPages :: IntMap ByteString }

