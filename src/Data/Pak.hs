module Data.Pak 
  ( module Data.Pak.Internal
  , module Data.Pak.INodeTable
  , module Data.Pak.Display
  , processPak
  )
where

import Data.Pak.Display
import Data.Pak.Internal
import Data.Pak.Types
import Data.Pak.INodeTable

processPak :: Pak Parsed -> Either String (Pak Processed)
processPak (Pak idS inT r) =
  case processINodeTable inT of
    Left err -> Left err
    Right v  -> Right (Pak (processIDSector idS) v (processRest r))

processIDSector = id
processRest = id
