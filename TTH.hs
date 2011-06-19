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
import Database.HDBC
import Database.HDBC.Sqlite3

import Config
import TTHTypes
import FilelistTypes


-- | init sqlite
initTTHCache :: AppState -> IO ()
initTTHCache appState = do
    conn <- connectSqlite3 (configCacheFile $ appConfig appState)
    run conn ("CREATE TABLE IF NOT EXISTS tthcache "
            ++ "(path VARCHAR(1024) NOT NULL PRIMARY KEY, tth VARCHAR(80) NOT NULL, "
            ++ "modtime BIGINT NOT NULL);") []
    commit conn
    putStrLn "CREATE TABLE"
    putMVar (appTTHCache appState) conn

-- | get hash from sqlite cache
getCachedHash :: AppState -> T.Text -> EpochTime -> IO (Maybe T.Text)
getCachedHash appState path curModTime = do
    conn <- readMVar (appTTHCache appState)
    rows <- sqlbla conn path
    if null rows || (null $ head rows)
        then return Nothing
        else let row = head rows
                 hash = fromSql $ row !! 0
                 modTime = fromInteger $ fromSql $ row !! 1
             in if modTime == curModTime
                then return $! Just $ T.pack hash
                else return Nothing

sqlbla conn path = quickQuery' conn "SELECT tth, modtime FROM tthcache WHERE path=?;"
                   [SqlString $ T.unpack path]

-- | set hash in sqlite cache
setHashInCache :: AppState -> T.Text -> T.Text -> IO ()
setHashInCache appState path hash = do
    fileStatus <- getFileStatus (T.unpack path)
    let modTime = modificationTime fileStatus
    conn <- readMVar (appTTHCache appState)
    run conn "INSERT INTO tthcache VALUES (?, ?, ?);" [toSql path, toSql hash, SqlString $ show modTime]
    commit conn
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
