--- |
--- | This module contains functions to handle operations on local filesystem
--- |
--- Copyright   : (c) Florian Richter 2011
--- License     : GPL
---


module Filemgmt (
      getFileSize
    , getFileContent
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

-- | get size value of file object in tree
getFileSize :: AppState -> String -> IO (Maybe Integer)
getFileSize appState path = do
    IndexedFileTree fileTree htable <- readMVar $ appFileTree appState
    case T.pack path of
        cpath | dcFilelistText == cpath             -> return $ Just (fromIntegral $ L.length (treeNodeToXmlBz fileTree))
             | (T.pack "TTH/") == (T.take 4 cpath)  -> returnFileSize (searchHash (T.drop 4 cpath) fileTree)
             | otherwise                            -> returnFileSize (searchFile cpath fileTree)
    where
        returnFileSize node = 
            case node of
                Just f -> return $ Just (fileNodeSize f)
                Nothing -> return Nothing


-- | get file content of file object in tree
getFileContent :: AppState -> String -> Integer -> IO (Maybe L.ByteString)
getFileContent appState path offset = do
    IndexedFileTree fileTree htable <- readMVar $ appFileTree appState
    --putStrLn (show $ treeNodeToXml fileTree)
    case T.pack path of
        path | dcFilelistText == path             -> return $ Just (treeNodeToXmlBz fileTree)
             | (T.pack "TTH/") == (T.take 4 path) -> returnStream (searchHash (T.drop 4 path) fileTree)
             | otherwise                          -> returnStream (searchFile path fileTree)
    where
        returnStream :: Maybe TreeNode -> IO (Maybe L.ByteString)
        --returnStream node = (return node) >>= (\f-> liftIO $ Just $ getSystemFileContentsWithOffset (T.unpack $ fileNodePath f) offset)
        returnStream node =
            case node of
                Just f -> do
                              stream <- (getSystemFileContentsWithOffset (T.unpack $ fileNodePath f) offset)
                              return $ Just stream
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
    putMVar (appFileTree appState) =<< newIndexedFileTree =<< getFileList appState (configShareDir config)

reloadOwnShare :: AppState -> IO ()
reloadOwnShare appState = do
    takeMVar (appFileTree appState)
    loadOwnShare appState

-- vim: sw=4 expandtab
