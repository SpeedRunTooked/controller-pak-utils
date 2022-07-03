{-# LANGUAGE TemplateHaskell #-}
module Codec.MIO0 ( decodeMIO ) where

import           Data.Word
import           GHC.Int
import           Data.Bits (shiftR, shiftL, (.|.), (.&.))

import           Control.Lens

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

-- | Header is 16 bytes, the first 4 of which _must_ be the "MIO0" sig (ASCII).
--
-- The locations defined by the offsets are relative to the begnning of the
-- header, as opposed to the end. What this means is that if an offset say 32,
-- for instance, that would be 32 bytes from the "MIO0" signature.
data MIORawHeader = MIORH
  { mioSig     :: ByteString
  , fullLen    :: Word32      -- Length of the final, uncompressed, data
  , cOffset    :: Word32      -- location of the compressed data
  , uOffset    :: Word32      -- location of the uncompressed data
  }
 deriving Show

getMIOHeader :: ByteString -> Either String MIORawHeader
getMIOHeader bs = 
    case runGetOrFail getHeader bs of
      Left (_, _, str)       -> Left str
      Right (rest, _, val) -> Right val

getHeader :: Get MIORawHeader
getHeader =
  MIORH <$> getSig
        <*> getWord32be
        <*> getWord32be
        <*> getWord32be

getSig :: Get ByteString
getSig = getLazyByteString 4

-- | Lazily convert a ByteSting to a list of booleans representing its bits
getLayout :: ByteString -> [Bool]
getLayout = concatMap word8ToBools . BS.unpack

word8ToBools :: Word8 -> [Bool]
word8ToBools w = go 8 []
 where
  go 0 acc = acc
  go i acc = go (i - 1) (odd (w `shiftR` (8 - i)):acc)

data MIOState = MIOState { _layout   :: [Bool] -- lazy stream of layout bits
                         , _outputB  :: Vector Word8 -- output vector
                         , _idx    :: Int          -- Current output index
                         , _comped   :: ByteString   -- compressed data
                         , _uncomped :: ByteString   -- uncompressed data
                         }
  deriving Show

makeLenses ''MIOState

-- | The only function in this module that matters: Decode an MIO-encoded
--   ByteString into a Vector of Word8 bytes.
decodeMIO :: ByteString -> Either String (Vector Word8)
decodeMIO bs = fmap _outputB (runMIO bs)

type MIO = ExceptT String (State MIOState)

runMIO :: ByteString -> Either String MIOState
runMIO bs =
 do
  (_header, initSt) <- mkInitMIOState bs
  flip evalState initSt $ runExceptT (uncompress >> get)

mkInitMIOState :: ByteString -> Either String (MIORawHeader, MIOState)
mkInitMIOState dat = do
  header <- getMIOHeader dat
  let cOff = BS.drop (fromIntegral (cOffset header)) dat
      uOff = BS.drop (fromIntegral (uOffset header)) dat
      vec  = V.replicate (fromIntegral (fullLen header)) maxBound
      lbs  = getLayout (BS.drop 16 dat)
  pure (header, MIOState lbs vec 0 cOff uOff)

putByte :: Word8 -> MIO ()
putByte b =
 do
  buf <- use outputB
  i   <- use idx
  outputB .= (buf V.// [(i,b)])
  idx += 1

nextLBit :: MIO Bool
nextLBit =
 do
  lbss <- use layout
  let lb:lbs = lbss
  layout .= lbs
  return lb

compByte :: MIO Word8
compByte =
 do
  res <- BS.uncons <$> use comped
  case res of
    Nothing     -> throwError "Couldn't uncons in compByte"
    Just (b,bs) ->
     do
      comped .= bs
      return b

unCompByte :: MIO Word8
unCompByte =
 do
  res <- BS.uncons <$> use uncomped
  case res of
    Nothing     -> throwError "Couldn't uncons in unCompByte"
    Just (b,bs) ->
     do
      uncomped .= bs
      return b

isEnd :: MIO Bool
isEnd = (>=) <$> use idx <*> (fmap V.length) (use outputB)

calcLenOff :: Word8 -> Word8 -> (Word8, Word16)
calcLenOff b1 b2 = (l, combined + 1)
 where
  l = (b1 `shiftR` 4) + 3

  upperOff = (fromIntegral (b1 .&. 0x0F)) `shiftL` 8
  lowerOff = fromIntegral b2
  combined = upperOff .|. lowerOff

readCompressed :: MIO ()
readCompressed =
 do (l,o) <- calcLenOff <$> compByte <*> compByte
    curr  <- use idx
    outB  <- use outputB
    when ((fromIntegral o) > curr)
      (throwError ("len: " ++ show l ++ 
                   "\nidx: " ++ show curr ++
                   "\noffset: " ++ show o ++
                   "\noutputB_: " ++ show (V.take 32 outB)))
    let l'    = fromIntegral l
        vbs   = V.slice (curr - (fromIntegral o)) l' outB
        idxV  = V.enumFromN (fromIntegral curr) l'
        outB' = V.update_ outB idxV vbs
    idx += l'
    outputB .= outB'

uncompress :: MIO ()
uncompress =
 do b <- nextLBit
    if b
     then unCompByte >>= putByte
     else readCompressed
    end <- isEnd
    unless end
      uncompress
