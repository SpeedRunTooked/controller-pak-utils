module Data.Pak 
  ( module Data.Pak.Internal
  , module Data.Pak.INodeTable
  , module Data.Pak.Display
  , processPak
  , Parsed
  , Processed
  )
where

import Data.Pak.Display
import Data.Pak.Internal
import Data.Pak.Types
import Data.Pak.INodeTable
import Data.Pak.NoteTable

processPak :: Pak Parsed -> Either String (Pak Processed)
processPak (Pak idS inT nT r) = Pak <$> pure idS
                                    <*> processINodeTable (map startPage (tableEntries nT)) inT
                                    <*> processNT nT
                                    <*> pure r

processIDSector = id
processRest = id
