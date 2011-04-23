module FixedQueue where

import qualified Data.Sequence as S
import Data.Foldable (toList)
import Control.Concurrent.STM
import Control.Monad (when)
import FixedQueueTypes
import Config

max_entries = 100

-- | put entry in list
putElem :: FixedQueue t -> t -> IO ()
putElem queue element = atomically $ do
    (curNum, list) <- readTVar queue
    writeTVar queue (curNum+1, element S.<| (S.take max_entries list))

-- | blocks until events with num>last available
takeElem :: FixedQueue t -> Int -> IO (Int, [t])
takeElem queue last = atomically $ do
    (curNum, list) <- readTVar queue
    when (last >= curNum) retry
    return (curNum, reverse $ toList $ S.take (curNum - last) list)

-- vim: sw=4 expandtab
