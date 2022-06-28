module Data.Pak.Display where

import Data.List (intersperse)

import Data.ByteString.Lazy.Char8 (unpack)

import Data.Pak.Types
import Data.Pak.Internal
import Data.Pak.INodeTable
import Data.Pak.NoteTable

displayPak :: Pak Processed -> String
displayPak (Pak _ itab nT _) = concat $ intersperse divider [displayINodeTable itab, displayNoteTable nT]
 where
  divider = "\n\n" ++ replicate 78 '-' ++ "\n\n"


displayINodeTable :: INodeTable Processed -> String
displayINodeTable itab =
  "INodeIndex:\n" ++
  "\tChecksum: " ++ show check ++ " (Passed)\n\n" ++
  "\tFiles (Notes): \n" ++ concat (map displayFile (zip [1..] fs)) ++ "\n" ++
  "\tFree Pages: " ++ show free ++ "\n"
  where
    ITable check tab fs free = itab

displayNoteTable :: NoteTable Processed -> String
displayNoteTable (NT es) =
  "Note Table entries:\n" ++
  concat (map displayNoteTableEntry (zip [1..] es)) ++ "\n"

displayNoteTableEntry :: (Int, NoteTableEntry) -> String
displayNoteTableEntry (i, (NTE a b c _ _ _ g h)) =
  "\tNote #" ++ show i ++ "\n" ++
  "\t\tGame  Code: " ++ unpack a ++ "\n" ++
  "\t\tPub.  Code: " ++ unpack b ++ "\n" ++
  "\t\tStart Page: " ++ show c ++ "\n" ++
  "\t\tFile  Ext.: " ++ unpack g ++ "\n" ++
  "\t\tFile  Name: " ++ unpack h ++ "\n"

displayFile (n, FileLoc i l s) = "\t\tFile #" ++ show n ++ " starts on page " ++
                                  show i ++ " and is " ++ show l ++ " pages (" ++
                                  show ((toInteger l) * 256) ++ " bytes) long\n"
