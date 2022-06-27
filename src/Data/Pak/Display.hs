module Data.Pak.Display where

import Data.Pak.Types
import Data.Pak.Internal
import Data.Pak.INodeTable

displayPak :: Pak Processed -> String
displayPak (Pak _ itab _) =
  "INodeIndex:\n" ++
  "\tChecksum: " ++ show check ++ " (Passed)\n\n" ++
  "\tFiles (Notes): \n" ++ concat (map displayFile (zip [1..] fs)) ++ "\n" ++
  "\tFree Pages: " ++ show free ++ "\n"
  where
    ITable check tab fs free = itab

displayFile (n, FileLoc i l) = "\t\tFile #" ++ show n ++ " starts on page " ++
                                show i ++ " and is " ++ show l ++ " pages (" ++
                                show ((toInteger l) * 256) ++ " bytes) long\n"
