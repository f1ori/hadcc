--- |
--- | tiger hash calcuation and caching stuff
--- |
--- Copyright   : (c) Florian Richter 2011
--- License     : GPL
---

module TTH 
    (
      getHashForFile
    , loadTTHCache
    , getCachedHash
    , hashFileList
    ) where

import Data.Digest.TigerHash
import Data.Digest.TigerHash.ByteString
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Text as T
import System.Posix.Types
import System.Posix.Files
import System.Directory
import Control.Monad
import Control.Concurrent
import Control.Exception as E
import Config
import TTHTypes
import FilelistTypes
import Data.HashTable

loadTTHCache :: AppState -> FilePath -> IO ()
loadTTHCache appState cacheFile = do
    putMVar (appTTHCache appState) =<< Prelude.catch
        ((E.evaluate . read) =<< readFile cacheFile)
        (\e -> return M.empty)

getCachedHash :: AppState -> T.Text -> EpochTime -> IO (Maybe T.Text)
getCachedHash appState path curModTime = withMVar (appTTHCache appState) $ \cache -> do
    case M.lookup path cache of
        Just (hash, modTime) -> if modTime == curModTime
	                        then return $! Just hash
				else return Nothing
	Nothing -> return Nothing

setHashInCache :: AppState -> T.Text -> T.Text -> IO ()
setHashInCache appState path hash = do
    fileStatus <- getFileStatus (T.unpack path)
    let modTime = modificationTime fileStatus
    newCache <- modifyMVar (appTTHCache appState) (\cache -> return $! double $ M.insert path (hash, modTime) cache)
    writeFile "Hadcc.cache" (show newCache)
    where
        double a = (a,a)


-- | calc hash for every file in fileTree, of not present
hashFileList :: AppState -> IO ()
hashFileList appState = do
    IndexedFileTree tree htable <- readMVar (appFileTree appState)
    traverse appState [] tree
    where
        traverse :: AppState -> [T.Text] -> TreeNode -> IO ()
        traverse appState dirs (DirNode name _ children) = mapM_ (traverse appState (name:dirs)) children
        traverse appState dirs (FileNode _ _ _ _ (Just hash)) = return ()
        traverse appState dirs node@(FileNode name path _ _ Nothing) = do
	    hash <- getHashForFile path
	    setHashInCache appState path hash
	    modifyMVar_ (appFileTree appState) (\(IndexedFileTree tree htable) -> do
                insert htable hash node
                return $! IndexedFileTree (setHash hash (reverse (name:dirs)) tree) htable)

        setHash :: T.Text -> [T.Text] -> TreeNode -> TreeNode
        setHash hash [name] file@(FileNode fname _ _ _ _)
	    | name == fname = file {fileNodeHash = Just hash}
	    | otherwise     = file
        setHash hash (name:dirs) dir@(DirNode dname _ children)
	    | name == dname  = dir {dirNodeChildren = map (setHash hash dirs) children}
	    | otherwise      = dir


getHashForFile :: T.Text -> IO T.Text
getHashForFile path = do
    content <- L.readFile (T.unpack path)
    return $! T.filter (/='=') $ T.pack $ b32TigerHash (tigerTreeHash content)

-- test with runhaskell tth.hs tth file.ext
--main = liftM (!!1) getArgs >>= getHashForFile >>= putStrLn

-- vim: sw=4 expandtab
