--- |
--- | This module contains a mgmt-functions to maintain a cache of filelists
--- |
--- Copyright   : (c) Florian Richter 2011
--- License     : GPL
---

module FilelistCache where

import Control.Concurrent.STM
import Control.Monad
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as L
import Control.DeepSeq

import FilelistCacheTypes
import Config
import FilelistTypes
import Filelist
import DCToClient


-- | download filelist of some user.
-- | the filelist will be cached, so the next request should be fast
getFilelistCached :: AppState -> Nick -> IO TreeNode
getFilelistCached appState nick = do
    result <- atomically $ do
        cache <- readTVar (appFilelistCache appState)
	case M.lookup nick cache of
	    Just FlCEInProgress      -> retry
	    Just (FlCETreeNode tree) -> return $! Just tree
	    Nothing                  -> do
	               -- not in cache, tell everybody, we're going to download it
                       let newcache = M.insert nick FlCEInProgress cache
		       newcache `seq` writeTVar (appFilelistCache appState) newcache
	               return Nothing
    case result of
        Just tree -> tree `seq` return tree
	Nothing   -> do
		     filelist <- downloadFilelist appState nick
	             let tree = xmlBzToTreeNode $ L.fromChunks [filelist]
		     atomically $ do
                         cache <- readTVar (appFilelistCache appState)
                         let newcache = M.insert nick (FlCETreeNode tree) cache
		         newcache `deepseq` writeTVar (appFilelistCache appState) newcache
		     return tree

-- vim: sw=4 expandtab
