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
import System.Time
import System.Directory
import Control.Monad
import Control.Concurrent
import Control.Exception as E
import Config
import TTHTypes
import FilelistTypes

loadTTHCache :: AppState -> FilePath -> IO ()
loadTTHCache appState cacheFile = do
    putMVar (appTTHCache appState) =<< Prelude.catch
        ((E.evaluate . read) =<< readFile cacheFile)
        (\e -> return M.empty)

getCachedHash :: AppState -> FilePath -> CalendarTime -> IO (Maybe String)
getCachedHash appState path curModTime = withMVar (appTTHCache appState) $ \cache -> do
    case M.lookup path cache of
        Just (hash, modTime) -> if modTime == curModTime
	                        then return $ Just hash
				else return Nothing
	Nothing -> return Nothing

setHashInCache :: AppState -> FilePath -> String -> IO ()
setHashInCache appState path hash = do
    modTime <- getModificationTime path
    let modTimeUTC = toUTCTime modTime
    newCache <- modifyMVar (appTTHCache appState) (\cache -> return $ double $ M.insert path (hash, modTimeUTC) cache)
    writeFile "Hadcc.cache" (show newCache)
    where
        double a = (a,a)


hashFileList :: AppState -> IO ()
hashFileList appState = do
    tree <- readMVar (appFileTree appState)
    traverse appState [] tree
    where
        traverse :: AppState -> [String] -> Node -> IO ()
        traverse appState dirs (DirNode name _ children) = mapM_ (traverse appState (name:dirs)) children
        --traverse appState dir@(DirNode _ _ children) = liftM (dir {dirNodeChildren=}) =<< return (mapM (traverse appState) children)
        traverse appState dirs (FileNode _ _ _ _ (Just hash)) = return ()
        traverse appState dirs (FileNode name path _ _ Nothing) = do
	    hash <- getHashForFile path
	    setHashInCache appState path hash
	    modifyMVar_ (appFileTree appState) (\n -> return $ setHash hash (reverse (name:dirs)) n)

        setHash :: String -> [String] -> Node -> Node
        setHash hash [name] file@(FileNode fname _ _ _ _)
	    | name == fname = file {fileNodeHash = Just hash}
	    | otherwise     = file
        setHash hash (name:dirs) dir@(DirNode dname _ children)
	    | name == dname  = dir {dirNodeChildren = map (setHash hash dirs) children}
	    | otherwise      = dir


getHashForFile :: FilePath -> IO String
getHashForFile path = do
    content <- L.readFile path
    return $! b32TigerHash (tigerTreeHash content)

-- test with runhaskell tth.hs tth file.ext
--main = liftM (!!1) getArgs >>= getHashForFile >>= putStrLn
