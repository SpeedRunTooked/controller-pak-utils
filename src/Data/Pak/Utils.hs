module Data.Pak.Utils where

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import qualified Data.Maybe as M
import qualified Data.List  as L

import           GHC.Word (Word8)

import           Data.Pak
import           Data.Pak.Types
import           Data.Pak.INodeTable
import           Data.Pak.NoteTable


sumBS :: ByteString -> Word8
sumBS = BS.foldl' (+) 0

-- | Calculate the checksum from a byte, not including that byte
--   Then return that byte and the checksum of the rest of the ByteString
checksumFrom :: Int -> ByteString -> Maybe (Word8, Word8)
checksumFrom 0 _  = Nothing
checksumFrom i bs = (fmap sumBS) <$> BS.uncons (BS.drop (fromIntegral (i - 1)) bs)

-- | Indices of page + byte that may be checksums.
--   I hope they don't use 16 bit checksums...
--
--   Example: checksumCandidates $ BS.pack [5,4,3,2,1,0] == [(0,2)]
checksumCandidates :: Bool -> Int -> ByteString -> [(Int, Int)]
checksumCandidates zeros count bs =
  noFullPages $ map pagify $ L.findIndices pred rawCandidates

 where
  pagify x = (x `div` 256, x `mod` 256)

  noFullPages xs = L.concat (L.filter (\xs -> length xs < count)
                                      (L.groupBy (\(x,_) (y,_) -> x == y)
                                                 xs))

  pred (Just (x,y)) = (zeros || x /= 0) && x == y

  rawCandidates = [Just (b, check)
                  | Just (b, check) <- map (flip checksumFrom bs)
                                           [1..(fromIntegral (BS.length bs))]]
