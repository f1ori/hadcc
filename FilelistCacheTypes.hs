--- |
--- | This module contains type definitions for FilelistCache.hs
--- |
--- Copyright   : (c) Florian Richter 2011
--- License     : GPL
---


module FilelistCacheTypes where

import Control.Concurrent.STM
import qualified Data.Map as M
import Control.DeepSeq

import FilelistTypes

data FilelistCacheEntry = FlCETreeNode TreeNode
                        | FlCEInProgress
type FilelistCache = TVar (M.Map String FilelistCacheEntry)

newFilelistCache = newTVarIO M.empty

instance NFData FilelistCacheEntry where
    rnf (FlCETreeNode tree) = rnf tree
    rnf (FlCEInProgress)    = rnf ()


-- vim: sw=4 expandtab
