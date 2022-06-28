module Data.Pak.Internal where

import           Data.Word

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import qualified Data.Maybe as M

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

getPage :: Word16 -> RawPages -> Maybe ByteString
getPage i (RP im) = IM.lookup (fromIntegral i) im

getPages :: [Word16] -> RawPages -> Maybe ByteString
getPages is rp = if length ps /= length is
                 then Nothing
                 else Just (BS.concat (M.catMaybes ps))
 where
  ps = map (flip getPage rp) is
