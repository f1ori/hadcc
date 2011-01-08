
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
    fileTree <- readMVar $ appFileTree appState
    case path of
        path | "files.xml.bz2" == path -> return $ Just (fromIntegral $ L.length (getXmlBZList fileTree))
             | "TTH/" == (take 4 path) -> returnFileSize (searchHash (drop 4 path) fileTree)
             | otherwise               -> returnFileSize (searchFile path fileTree)
    where
        returnFileSize node = 
            case node of
                Just f -> return $ Just (fileNodeSize f)
                Nothing -> return Nothing


getFileContent :: AppState -> String -> Integer -> IO (Maybe L.ByteString)
getFileContent appState path offset = do
    fileTree <- readMVar $ appFileTree appState
    case path of
        path | "files.xml.bz2" == path -> return $ Just (getXmlBZList fileTree)
             | "TTH/" == (take 4 path) -> returnStream (searchHash (drop 4 path) fileTree)
             | otherwise               -> returnStream (searchFile path fileTree)
    where
        returnStream node =
            case node of
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
