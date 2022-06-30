module Data.MIO0 where

import           Data.Word
import           GHC.Int
import           Data.Bits (shiftR)

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Writer

import           Data.Binary.Get

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS

import           Control.Arrow (first, second)

import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed (Vector)


data MIORawHeader = MIORH
  { mioSig     :: ByteString
  , fullLen    :: Word32
  , cOffset    :: Word32
  , uOffset    :: Word32
  , layoutBits :: [Bool]
  }
 deriving Show

getMIOHeader :: ByteString -> Either String (ByteString, MIORawHeader)
getMIOHeader bs = 
    case runGetOrFail getHeader bs of
      Left (_, _, str)       -> Left str
      Right (rest, _, val) -> Right (rest, val)


getHeader :: Get MIORawHeader
getHeader =
  MIORH <$> getSig
        <*> getWord32be
        <*> getWord32be
        <*> getWord32be
        <*> getLayout

getSig :: Get ByteString
getSig = getLazyByteString 4


-- This is WRONG! We don't know how many layout bits there are!
-- Technically not part of the header
getLayout :: Get [Bool]
getLayout = sequence (replicate 10 getWord8) >>= pure . concatMap word8ToBools

word8ToBools :: Word8 -> [Bool]
word8ToBools w = go 8 []
 where
  go 0 acc = acc
  go i acc = go (i - 1) (odd (w `shiftR` (8 - i)):acc)

data MIOState = MIOState { layout_   :: [Bool]
                         , outputB_  :: Vector Word8
                         , index_    :: Int
                         , comped_   :: ByteString
                         , uncomped_ :: ByteString
                         }
  deriving Show

type MIO = ExceptT String (State MIOState)

mkInitMIOState :: ByteString -> MIORawHeader -> MIOState
mkInitMIOState dat (MIORH _ l c u bs) = MIOState bs vec 0
 where
  vec  = V.replicate (fromIntegral l) 0
  cOff = BS.drop ((fromIntegral c) - 16) dat
  uOff = BS.drop ((fromIntegral u) - 16) dat


outputB :: MIO (Vector Word8)
outputB = gets outputB_

layout :: MIO [Bool]
layout = gets layout_

-- getByteU :: MIO Word8
-- getByteU =
--  do
  


runMIO :: ByteString -> Either String ByteString
runMIO bs =
 do
  (rest, header) <- getMIOHeader bs
  flip evalState (mkInitMIOState header) $ (runExceptT (uncompress rest))

uncompress :: ByteString -> MIO ByteString
uncompress = undefined
