--- |
--- | This module contains a queue fixed size which drops entry, which are to old
--- |
--- Copyright   : (c) Florian Richter 2011
--- License     : GPL
---

module FixedQueue where

import qualified Data.Sequence as S
import Data.Foldable (toList)
import Control.Concurrent.STM
import Control.Monad (when)
import FixedQueueTypes
import Config

max_entries = 100

-- | put entry in list
putFixedQueue :: FixedQueue t -> t -> IO ()
putFixedQueue queue element = atomically $ do
    (curNum, list) <- readTVar queue
    writeTVar queue (curNum+1, element S.<| (S.take max_entries list))

-- | blocks until events with num>last available
takeFixedQueue :: FixedQueue t -> Int -> IO (Int, [t])
takeFixedQueue queue last = atomically $ do
    (curNum, list) <- readTVar queue
    when (last >= curNum) retry
    return (curNum, reverse $ toList $ S.take (curNum - last) list)

-- | blocks until events with num>last available
takeOneFixedQueue :: FixedQueue t -> Int -> IO (Int, t)
takeOneFixedQueue queue last = atomically $ do
    (curNum, list) <- readTVar queue
    when (last >= curNum) retry
    let returnNum = max (last + 1) (curNum - (S.length list) + 1)
    return (returnNum, S.index list (curNum - returnNum))

-- vim: sw=4 expandtab
