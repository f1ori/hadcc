--- |
--- | tiger hash calcuation and caching stuff
--- |
--- Copyright   : (c) Florian Richter 2011
--- License     : GPL
---

module TTH 
    (
      getHashForFile
    , initTTHCache
    , getCachedHash
    , hashFileList
    ) where

import Data.Digest.TigerHash
import Data.Digest.TigerHash.ByteString
import Data.Maybe
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
import Data.HashTable
import Database.SQLite

import Config
import TTHTypes
import FilelistTypes

-- | sqlite table for tth cache
tthCacheTable = Table "TTHCache" [
       (Column "Path" (SQLVarChar 255) [IsNullable False])
     , (Column "TTH" (SQLVarChar 255) [IsNullable False])
     , (Column "ModTime" (SQLInt BIG True False) [IsNullable False])
     ] [TablePrimaryKey ["Path"], TableUnique ["Path"]]


-- | init sqlite
initTTHCache :: AppState -> IO ()
initTTHCache appState = do
    sqliteHandle <- openConnection (configCacheFile $ appConfig appState)
    result <- defineTableOpt sqliteHandle True tthCacheTable
    case result of
        Just msg -> putStrLn ("create cache table: " ++ msg)
        Nothing -> return ()
    putMVar (appSQLiteHandle appState) sqliteHandle

-- | get hash from sqlite cache
getCachedHash :: AppState -> T.Text -> EpochTime -> IO (Maybe T.Text)
getCachedHash appState path curModTime = do
    sqliteHandle <- readMVar (appSQLiteHandle appState)
    result <- execParamStatement sqliteHandle
              "SELECT Path, TTH, ModTime FROM TTHCache WHERE Path=:Path"
              [(":Path", Text $ T.unpack path)]
    case result of
        Left msg -> do
                    putStrLn ("cache database error: " ++ msg)
                    return Nothing
        Right rows -> if null rows || (null $ head rows)
                      then return Nothing
                      else let row = head $ head rows
                               path = fromJust $ Prelude.lookup "Path" row
                               hash = fromJust $ Prelude.lookup "TTH" row
                               modTime = read $ fromJust $ Prelude.lookup "ModTime" row
                           in if modTime == curModTime
                              then return $! Just $ T.pack hash
                              else return Nothing

-- | set hash in sqlite cache
setHashInCache :: AppState -> T.Text -> T.Text -> IO ()
setHashInCache appState path hash = do
    fileStatus <- getFileStatus (T.unpack path)
    let modTime = modificationTime fileStatus
    sqliteHandle <- readMVar (appSQLiteHandle appState)
    result <- insertRow sqliteHandle "TTHCache"
              [("Path", T.unpack path), ("TTH", T.unpack hash), ("ModTime", show modTime)]
    return ()


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
            putStrLn ("hash: " ++ (show path))
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
        setHash hash rest node = node


getHashForFile :: T.Text -> IO T.Text
getHashForFile path = do
    content <- L.readFile (T.unpack path)
    return $! T.filter (/='=') $ T.pack $ b32TigerHash (tigerTreeHash content)

-- test with runhaskell tth.hs tth file.ext
--main = liftM (!!1) getArgs >>= getHashForFile >>= putStrLn

-- vim: sw=4 expandtab
