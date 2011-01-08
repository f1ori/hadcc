
module Filemgmt (
      getFileSize
    , getFileContent
    ) where

import System.IO
import qualified Data.ByteString.Lazy as L
import Control.Monad
import Control.Concurrent
import FilelistTypes
import Filelist
import Config

getFileSize :: AppState -> String -> IO (Maybe Integer)
getFileSize appState path = do
    list <- readMVar $ appFileTree appState
    if path == "files.xml.bz2"
        then
            return $ Just (fromIntegral $ L.length (getXmlBZList list))
        else do
            let file = searchFile path list
            case file of
                Just f -> return $ Just (fileNodeSize f)
                Nothing -> return Nothing


getFileContent :: AppState -> String -> Integer -> IO (Maybe L.ByteString)
getFileContent appState path offset = do
    list <- readMVar $ appFileTree appState
    if path == "files.xml.bz2"
        then
            return $ Just (getXmlBZList list)
        else do
            let file = searchFile path list
            case file of
                Just f -> do
                              stream <- (getSystemFileContentsWithOffset (fileNodePath f) offset)
                              return $ Just stream
                Nothing -> return Nothing


getSystemFileContentsWithOffset :: FilePath -> Integer -> IO L.ByteString
getSystemFileContentsWithOffset path offset = do
    h <- openBinaryFile path ReadMode
    hSeek h AbsoluteSeek offset
    L.hGetContents h

-- vim: sw=4 expandtab
