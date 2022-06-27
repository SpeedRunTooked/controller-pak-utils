module Data.Pak.Internal where

import           Data.Word

import           Data.ByteString.Lazy (ByteString)

import           Data.IntMap (IntMap)

import           Data.Pak.Types
import           Data.Pak.INodeTable


-- | The `Pak` in its structured form
data Pak a = Pak { idSector   :: ByteString
                 , indexTable :: INodeTable a
                 , rawData    :: RawPages
                 }

-- | The Raw pages, one ByteString per page. This should probably never be used.
data RawPages = RP { rawPages :: IntMap ByteString }

