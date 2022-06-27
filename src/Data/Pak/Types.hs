module Data.Pak.Types where

import Data.Word

-- | Phantom Type for when we've only parsed, but not processed the filesystem
data Parsed

-- | Phantom Type for when we've processed the filesystem
data Processed

-- TODO: RuntimeRep??? or UNPACK or UNBoxedTuples or UnliftedDataTypes
data Three a = T a a a
data Four a = F a a a a

data NoteHuh = Note { companyCode :: Word16
                 , gameCode    :: Word32
                 , noteName    :: Four Word32
                 , noteExt0    :: Word8
                 , noteExt123  :: Three Word8
                 }
