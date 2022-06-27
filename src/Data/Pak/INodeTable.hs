module Data.Pak.INodeTable where

import           Data.Word
import           Data.Pak.Types

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Writer

import           Control.Arrow (first, second)

-- | The I-Node table. When `Parsed` we will not have the list of Files
--   or the Free list.
data INodeTable a =
  ITable { iNodeChecksum :: Word8
           -- ^ The checksum is calculated by summing the
           --   bytes of the I-Node sequence

         , iNodeSequence :: [Word16]
           -- ^ The I-Node entries, two bytes per entry

         , iNodeFiles    :: [FileLoc]
           -- ^ Once `Processed` this will be the list of
           --   files found on this Controller Pak

         , iNodeFree     :: [Word8]
           -- ^ Once `Processed` this will be the list of
           --   free pages on the Controller Pak
         }

-- | A file on the Pak, the starting page and the length is all we need.
data FileLoc =
  FileLoc { fileStart  :: Word8
          , fileLength :: Word8
          }
  deriving Show


processINodeTable :: INodeTable Parsed -> Either String (INodeTable Processed)
processINodeTable (ITable check tab locs free) =
  case (e, s, w) of
    (Left err ,        _, _) -> Left err
    (Right fls, (128, _), w) -> Right (ITable check tab fls w)
    (_        , (off, _), _) -> Left (consumptionError off)
  where
    consumptionError off =
      "Expect to consume all 128 entries of the INode, only got " ++ show off
    ((e, s), w) = runINodeParser (5, tab) parseFileLocs

getFileLocs :: INodeTable Parsed -> [FileLoc]
getFileLocs (ITable _ sq _ _) = undefined

type INodeState = (Word8, [Word16])

type INodeParser = ExceptT String (StateT INodeState (Writer [Word8]))

runINodeParser :: INodeState -> INodeParser a -> ((Either String a, (Word8, [Word16])), [Word8])
runINodeParser st = runWriter . flip runStateT st . runExceptT

parseFileLocs :: INodeParser [FileLoc]
parseFileLocs =
 do fl <- parseFileLoc
    case fl of
      Nothing  -> pure []
      Just fl' ->
       do fls <- parseFileLocs
          pure (fl':fls)
      

parseFileLoc :: INodeParser (Maybe FileLoc)
parseFileLoc =
 do skipFree
    start <- getOffset
    if (start == 128)
    then pure Nothing
    else do consumeFileSeq
            eoc <- endOfChain
            if not eoc
            then throwError "consumed entire file but do not see sentinel"
            else do skipEntry
                    end <- getOffset
                    pure (Just (FileLoc start (end - start)))

consumeFileSeq :: INodeParser ()
consumeFileSeq =
 do ind   <- isIndex
    if not ind
    then pure ()
    else do skipEntry
            consumeFileSeq

isIndex :: INodeParser Bool
isIndex =
 do i <- peek
    if i == 3 || i == 1
    then pure False
    else pure True

endOfChain :: INodeParser Bool
endOfChain =
 do i <- peek
    if i == 1
    then pure True
    else pure False

next :: INodeParser Word16
next =
 do is <- gets snd
    case is of
      []       -> throwError "Out of INodes"
      (i:rest) -> modify (first (+1)) >> pure i

peek :: INodeParser Word16
peek =
 do is <- gets snd
    case is of
      []       -> throwError "Out of INodes in `peek`"
      (i:rest) -> pure i

getOffset :: INodeParser Word8
getOffset = gets fst

atEnd :: INodeParser Bool
atEnd = getOffset >>= pure . (== 128)

skipEntry :: INodeParser ()
skipEntry =
 do (o, s) <- get
    case s of
      []   -> throwError "Out of INodes in `skip`"
      i:is -> put (o+1, is)

tellOffset :: INodeParser ()
tellOffset =
 do getOffset >>= tell . pure
    

skipFree :: INodeParser ()
skipFree =
 do end <- atEnd
    if end
    then pure ()
    else do i <- peek
            case i of
              3 -> skipEntry >> tellOffset >> skipFree
              _ -> pure ()
