module Data.Pak.DumpNote where

import qualified Data.ByteString.Lazy as BS
import           Data.ByteString.Lazy    (ByteString)
import qualified Data.List            as L

import Data.Pak.Internal
import Data.Pak.Types
import Data.Pak.INodeTable
import Data.Pak.NoteTable

dumpNote :: Int -> Pak Processed -> Either String ByteString
dumpNote i (Pak _ inT (NT es) ps)
  | i >= length es = Left "Invalid index for Note"
  | otherwise      =
    case L.find sameStart (iNodeFiles inT) of
      Nothing               -> Left "Couldn't find Note in INodeTable"
      Just (FileLoc _ _ is) -> case getPages is ps of
                                Nothing -> Left "Unable to get all Pages"
                                Just bs -> Right bs
 where
  sameStart (FileLoc s _ _) = s == (startPage (es !! i))
