--- |
--- | type definitions for FixedQueue.hs to prevent circular imports
--- |
--- Copyright   : (c) Florian Richter 2011
--- License     : GPL
---

module FixedQueueTypes where

import qualified Data.Sequence as S
import Control.Concurrent.STM.TVar (TVar, newTVarIO)

type FixedQueue t = TVar (Int, S.Seq t)

-- | create event list
newFixedQueue :: IO (FixedQueue t)
newFixedQueue = newTVarIO (0, S.empty)

-- vim: sw=4 expandtab
