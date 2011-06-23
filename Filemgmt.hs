--- |
--- | This module contains functions to handle operations on local filesystem
--- |
--- Copyright   : (c) Florian Richter 2011
--- License     : GPL
---


module Filemgmt (
      getFileSizeAndContent
    , dcFilelist
    , loadOwnShare
    , reloadOwnShare
    ) where

import System.IO
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Control.Monad
import Control.Concurrent
import FilelistTypes
import Filelist
import Config

dcFilelist = "files.xml.bz2"
dcFilelistText = T.pack dcFilelist

-- | get file size and content of file object in tree
-- | (as one call in case size (of filelist) changes )
getFileSizeAndContent :: AppState -> String -> Integer -> IO (Maybe (Integer, L.ByteString))
getFileSizeAndContent appState path offset = do
    IndexedFileTree fileTree htable <- readMVar $ appFileTree appState
    let xmlBzList = treeNodeToXmlBz fileTree
    case T.pack path of
        cpath | dcFilelistText == cpath             -> return $ Just (fromIntegral $ L.length xmlBzList, xmlBzList)
             | (T.pack "TTH/") == (T.take 4 cpath)  -> returnSizeAndStream (searchHash (T.drop 4 cpath) fileTree)
             | otherwise                            -> returnSizeAndStream (searchFile cpath fileTree)
    where
        returnSizeAndStream :: Maybe TreeNode -> IO (Maybe (Integer, L.ByteString))
        returnSizeAndStream node =
            case node of
                Just f -> do
                              stream <- (getSystemFileContentsWithOffset (T.unpack $ fileNodePath f) offset)
                              return $ Just (fileNodeSize f, stream)
                Nothing -> return Nothing


-- | get file content of system file using an offset
getSystemFileContentsWithOffset :: FilePath -> Integer -> IO L.ByteString
getSystemFileContentsWithOffset path offset = do
    h <- openBinaryFile path ReadMode
    hSeek h AbsoluteSeek offset
    L.hGetContents h

loadOwnShare :: AppState -> IO ()
loadOwnShare appState = do
    let config = appConfig appState
    filelist <- getFileList appState (configShareDir config)
    indexedFilelist <- newIndexedFileTree filelist
    putMVar (appFileTree appState) indexedFilelist

reloadOwnShare :: AppState -> IO ()
reloadOwnShare appState = do
    takeMVar (appFileTree appState)
    loadOwnShare appState

-- vim: sw=4 expandtab
