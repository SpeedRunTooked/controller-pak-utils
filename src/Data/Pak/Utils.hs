module Data.Pak.Utils where

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import           Data.Set (Set)
import qualified Data.Set as S

import qualified Data.Maybe as M
import qualified Data.List  as L

import           GHC.Word (Word8)
import           Data.Int (Int64)

import           Data.Pak
import           Data.Pak.Types
import           Data.Pak.INodeTable
import           Data.Pak.NoteTable

type Count = Int
type Start = Int64
type End = Int64

sumBS :: ByteString -> Word8
sumBS = BS.foldl' (+) 0

checksumFromTo :: ByteString -> Start -> End -> Word8
checksumFromTo bs i j = sumBS end
 where
  begining = BS.drop i bs

  end      = BS.take (j - i + 1) begining


-- | Calculate the checksum from a byte, not including that byte
--   Then return that byte and the checksum for the next `j - i + 1` bytes
checksumFromTo1 :: ByteString -> Start -> End -> Maybe (Word8, Word8)
checksumFromTo1 _  0 _ = Nothing
checksumFromTo1 bs i j = (fmap sumBS) <$> BS.uncons end
 where
  begining = BS.drop (i - 1) bs

  end      = BS.take (j - i + 1) begining

checksumFrom :: ByteString -> Int64 -> Maybe (Word8, Word8)
checksumFrom bs i = checksumFromTo1 bs i (BS.length bs)

-- | Treat the first byte of a ByteString as a checksum, and return
--   all of lengths that actually sum up to the proposed checksum.
checksumCandidatesFrom :: ByteString -> Set Int64
checksumCandidatesFrom bs = S.fromList cands
 where
  cands = [i - 1
          | (i, Just (b, check)) <- map f [2..(BS.length bs)]
          , b == check
          ]

  f i = (i , checksumFromTo1 bs 1 i)

-- | Indices of page + byte that may be checksums.
--   I hope they don't use 16 bit checksums...
--
--   This function is probably not useful, it's unlikely that
--   Checksums wouldn't be for some _portion_ of a file/page/etc.
--
--   Example: checksumCandidates $ BS.pack [5,4,3,2,1,0] == [(0,2)]
checksumCandidatesAll :: Bool -> Count -> ByteString -> [(Int, Int)]
checksumCandidatesAll zeros count bs =
  noFullPages $ map pagify $ L.findIndices pred rawCandidates

 where
  pagify x = (x `div` 256, x `mod` 256)

  noFullPages xs = L.concat (L.filter (\xs -> length xs < count)
                                      (L.groupBy (\(x,_) (y,_) -> x == y)
                                                 xs))

  pred (Just (x,y)) = (zeros || x /= 0) && x == y

  rawCandidates = [Just (b, check)
                  | Just (b, check) <- map (checksumFrom bs)
                                           [1..(BS.length bs)]]
