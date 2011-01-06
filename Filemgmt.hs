
module Filemgmt (
      getFileSize
    , getFileContent
    ) where

import System.IO
import qualified Data.ByteString.Lazy as L
import Control.Monad
import Filelist

getFileSize :: String -> IO (Maybe Integer)
getFileSize path = do
    list <- getFileList "/mnt/music"
    if path == "files.xml.bz2"
        then
            return $ Just (fromIntegral $ L.length (getXmlBZList list))
        else do
            let file = searchFile path list
            case file of
                Just f -> return $ Just (fileNodeSize f)
                Nothing -> return Nothing


getFileContent :: String -> Integer -> IO (Maybe L.ByteString)
getFileContent path offset = do
    list <- getFileList "/mnt/music"
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
