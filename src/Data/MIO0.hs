module Data.MIO0 where

import           Data.Word
import           GHC.Int
import           Data.Bits (shiftR, shiftL, (.|.), (.&.))

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

import Debug.Trace (trace)


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

data MIOState = MIOState { layout_   :: [Bool]
                         , outputB_  :: Vector Word8
                         , index_    :: Int
                         , len_      :: Int
                         , comped_   :: ByteString
                         , uncomped_ :: ByteString
                         }
  deriving Show

type MIO = ExceptT String (State MIOState)

mkInitMIOState :: ByteString -> Either String (MIORawHeader, MIOState)
mkInitMIOState dat = do
  header <- getMIOHeader dat
  let cOff = BS.drop (fromIntegral (cOffset header)) dat
      uOff = BS.drop (fromIntegral (uOffset header)) dat
      len  = fromIntegral (fullLen header)
      vec  = V.replicate (fromIntegral (fullLen header)) maxBound
      lbs  = getLayout (BS.drop 16 dat)
  pure (header, MIOState lbs vec 0 len cOff uOff)

outputB :: MIO (Vector Word8)
outputB = gets outputB_

putByte :: Word8 -> MIO ()
putByte b =
 do
  buf <- outputB
  i   <- index
  let nvec = (V.//) buf [(i,b)]
  modify (\s -> s { outputB_ = nvec })
  ipp

layout :: MIO [Bool]
layout = gets layout_

nextLBit :: MIO Bool
nextLBit =
 do
  lbss <- layout
  let lb:lbs = lbss
  modify (\s -> s { layout_ = lbs })
  lbss' <- layout
  trace (show (take 10 lbss')) (pure ())
  return lb

index :: MIO Int
index = gets index_

ipp :: MIO ()
ipp = modify (\s -> s { index_ = index_ s + 1 })

ipEq :: Int -> MIO ()
ipEq n = modify (\s -> s { index_ = index_ s + n })

comped :: MIO ByteString
comped = gets comped_

uncomped :: MIO ByteString
uncomped = gets uncomped_

compByte :: MIO Word8
compByte =
 do
  res <- BS.uncons <$> comped
  case res of
    Nothing     -> throwError "Couldn't uncons in compByte"
    Just (b,bs) ->
     do
      modify (\s -> s { uncomped_ = bs })
      return b

unCompByte :: MIO Word8
unCompByte =
 do
  res <- BS.uncons <$> uncomped
  case res of
    Nothing     -> throwError "Couldn't uncons in unCompByte"
    Just (b,bs) ->
     do
      modify (\s -> s { uncomped_ = bs })
      return b

runMIO :: ByteString -> Either String MIOState
runMIO bs =
 do
  (_header, initSt) <- mkInitMIOState bs
  flip evalState initSt $ runExceptT (uncompress >> get)

--runMIO' :: MIO a -> MIOState -> Either String 
runMIO' mio st = flip runState st $ runExceptT mio

isEnd :: MIO Bool
isEnd = (>=) <$> index <*> (fmap V.length) outputB

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
    curr  <- index
    outB  <- outputB
    when ((fromIntegral o) > curr)
      (throwError ("len: " ++ show l ++ 
                   "\nindex: " ++ show curr ++
                   "\noffset: " ++ show o ++
                   "\noutputB_: " ++ show (V.take 32 outB)))
    let l'    = fromIntegral l
        vbs   = V.slice (curr - (fromIntegral o)) l' outB
        idxV  = V.enumFromN (fromIntegral curr) l'
        outB' = V.update_ outB idxV vbs
    ipEq l'
    modify (\s -> s { outputB_ = outB' })

uncompress :: MIO ()
uncompress =
 do b <- nextLBit
    if b
     then unCompByte >>= putByte
     else readCompressed
    end <- isEnd
    unless end
      uncompress
