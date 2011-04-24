module FilelistCacheTypes where

import Control.Concurrent.STM
import qualified Data.Map as M

import FilelistTypes

data FilelistCacheEntry = FlCETreeNode TreeNode
                        | FlCEInProgress
type FilelistCache = TVar (M.Map String FilelistCacheEntry)

newFilelistCache = newTVarIO M.empty
