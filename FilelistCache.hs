module FilelistCache where

import Control.Concurrent.STM
import Control.Monad
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as L

import FilelistCacheTypes
import Config
import FilelistTypes
import Filelist
import DCToClient

getFilelistCached :: AppState -> Nick -> IO TreeNode
getFilelistCached appState nick = do
    result <- atomically $ do
        cache <- readTVar (appFilelistCache appState)
	case M.lookup nick cache of
	    Just FlCEInProgress      -> retry
	    Just (FlCETreeNode tree) -> return $ Just tree
	    Nothing                  -> do
	               -- not in cache, tell everybody, we're going to download it
		       writeTVar (appFilelistCache appState) (M.insert nick FlCEInProgress cache)
	               return Nothing
    case result of
        Just tree -> return tree
	Nothing   -> do
		     filelist <- downloadFilelist appState nick
	             let tree = xmlBzToTreeNode $ L.fromChunks [filelist]
		     atomically $ do
                         cache <- readTVar (appFilelistCache appState)
		         writeTVar (appFilelistCache appState) (M.insert nick (FlCETreeNode tree) cache)
		     return tree
