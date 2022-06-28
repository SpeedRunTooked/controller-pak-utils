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

import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed (Vector)

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

         , iNodeFree     :: [Word16]
           -- ^ Once `Processed` this will be the list of
           --   free pages on the Controller Pak
         }

-- | A file on the Pak, the starting page and the length is all we need.
data FileLoc =
  FileLoc { fileStart  :: Word16
          , fileLength :: Word16
          , fileSeq    :: [Word16]
          }
  deriving Show

-- | We're going to index _a lot_ into the INodeTable when processing, so
--   creating a vector is important
makeTableVec :: [Word16] -> Vector Word16
makeTableVec is = V.fromList ((replicate 5 0) ++ is)

-- | The state of our monad is the list of inodes visited
--   IMPORTANT: The list will be _in reverse_ during computation
type INodeState = [Word16]

type INodeParser = ExceptT String (State INodeState)

runINodeParser :: INodeParser a -> Either String a
runINodeParser = flip evalState [] . runExceptT

getFreePages :: Vector Word16 -> [Word16]
getFreePages vec = V.toList (V.map fromIntegral (V.elemIndices 3 vec))

followFile :: Vector Word16 -> Word16 -> INodeParser [Word16]
followFile v s =
 do put []
    if (s == 0)
    then pure []
    else followINodes v s

v !? i = v V.!? (fromIntegral i)

followINodes :: Vector Word16 -> Word16 -> INodeParser [Word16]
followINodes vec start
  | Just 1 <- vec !? start = reverse <$> get
  | Just 3 <- vec !? start = throwError "The chain of inodes leads to a free page!"
  | Just i <- vec !? start = modify (i:) >> followINodes vec i
  | otherwise              = throwError "INode Table value out of bounds"


-- | To process the INode Table we need a list of starting pages
--   (one for each file), and the parsed INodeTable
processINodeTable :: [Word16] -> INodeTable Parsed -> Either String (INodeTable Processed)
processINodeTable ss (ITable check tab locs free) =
  case runINodeParser (mapM (followFile vec) ss) of
    Left err -> Left err
    Right is -> validate is freeList >>
                pure (ITable check tab (package ss is) freeList)
  where
    vec = makeTableVec tab

    freeList = getFreePages vec

    validate is fs = unless (totalLen /= 256)
                       (Left $ "Number of used+free INodes: " ++ show totalLen
                              ++ " number expected: 256")

      where totalLen = (length fs) + (sum (map length is))

package :: [Word16] -> [[Word16]] -> [FileLoc]
package ss iss = filter ((>= 5) . fileStart) (map f (zip ss iss))
 where
  f (s, is) = FileLoc s (fromIntegral $ length is) is
