{-# LANGUAGE OverloadedStrings #-}
module Data.Pak.DumpNote
  ( dumpNote
  , withNote
  , mioOffsets
  ) where

import qualified Data.ByteString.Lazy as BS
import           Data.ByteString.Lazy    (ByteString)
import qualified Data.List            as L

import Data.Pak.Internal
import Data.Pak.Types
import Data.Pak.INodeTable
import Data.Pak.NoteTable

import GHC.Int

dumpNote :: Int -> Pak Processed -> Either String ByteString
dumpNote i pak = withNote i pak pure

withNote :: Int -> Pak Processed -> (ByteString -> Either String a) -> Either String a
withNote i (Pak _ inT (NT es) ps) f
  | i >= length es = Left "Invalid index for Note: withNote"
  | otherwise      =
    case L.find sameStart (iNodeFiles inT) of
      Nothing               -> Left "Couldn't find Note in INodeTable"
      Just (FileLoc _ _ is) ->
        case getPages is ps of
          Nothing -> Left "Unable to get all Pages"
          Just bs -> f bs

 where
  sameStart (FileLoc s _ _) = s == (startPage (es !! i))

mioOffsets :: Int -> Pak Processed -> Either String [Int64]
mioOffsets i pak = withNote i pak search

search :: ByteString -> Either String [Int64]
search bs = pure [i | (True, i) <- zip mios is]
 where
  is   = BS.findIndices (== 77) bs
  mios = map (BS.isPrefixOf "MIO0" . snd . flip BS.splitAt bs) is
