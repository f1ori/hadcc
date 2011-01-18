module Filelist where

import System.Directory
import System.FilePath
import System.IO
import Time
import Data.Maybe
import Control.Monad
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Codec.Compression.BZip as BZip
import Text.XML.Light


import FilelistTypes
import TTH
import Config


getFileList :: AppState -> FilePath -> IO TreeNode
getFileList appState dir = do
    names <- getUsefulContents dir
    let paths = map (dir </>) names
    nodes <- (forM paths $ \path -> do
	        isDirectory <- doesDirectoryExist path
	        if isDirectory
	          then getFileList appState path
	          else getFile appState path
	     )
    return (DirNode (last (splitDirectories dir)) dir nodes)


getFile :: AppState -> FilePath -> IO TreeNode
getFile appState path = do
    size <- getSystemFileSize path
    modTime <- toUTCTime `liftM` getModificationTime path
    hash <- getCachedHash appState path modTime
    return ( FileNode (takeFileName path) path size modTime hash )


getSystemFileSize :: FilePath -> IO Integer
getSystemFileSize path = do
    h <- openFile path ReadMode
    size <- hFileSize h
    hClose h
    return size


getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
    names <- getDirectoryContents path
    return (filter (`notElem` [".", ".."]) names)


-- | accumlulate filesizes of all files in tree
treeSize :: TreeNode -> Integer
treeSize (DirNode _ _ children)  = sum $ map treeSize children
treeSize (FileNode _ _ size _ _) = size


firstNotNothing :: [Maybe a] -> Maybe a
firstNotNothing [] = Nothing
firstNotNothing ((Just x):xs) = Just x
firstNotNothing (Nothing:xs) = firstNotNothing xs

-- | search FileNode in TreeNode by path
searchFile :: String -> TreeNode -> Maybe TreeNode
searchFile path file@(FileNode name _ _ _ _)
        | path == name             = Just file
	| otherwise                = Nothing
searchFile path (DirNode name _ children)
        | (firstPath path) == name = firstNotNothing $ map (searchFile (restPath path)) children
	| otherwise                = Nothing
    where
	firstPath path = takeWhile (/='/') path
	restPath path = tail $ dropWhile (/='/') path

-- | search hash in TreeNode
searchHash :: String -> TreeNode -> Maybe TreeNode
searchHash hash file@(FileNode _ _ _ _ (Just fhash))
        | fhash == hash                = Just file
	| otherwise                    = Nothing
searchHash hash (FileNode _ _ _ _ _)   = Nothing
searchHash hash (DirNode _ _ children) = firstNotNothing $ map (searchHash hash) children


-- | convert TreeNode to xml
treeNodeToXml :: TreeNode -> String
treeNodeToXml node = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n" ++ 
                  "<FileListing Version=\"1\" Generator=\"hdc V:0.1\">" ++ 
                  (toXml node) ++ "</FileListing>"
    where
        toXml (DirNode name _ children)            = "<Directory Name=\"" ++ (xmlQuote name) ++ "\">" ++
	                                             (concat $ map toXml children) ++ "</Directory>"
        toXml (FileNode name _ size _ (Just hash)) = "<File Name=\"" ++ (xmlQuote name) ++ "\" Size=\"" ++
	                                             (show size) ++ "\" TTH=\"" ++ hash ++ "\"/>"
        toXml (FileNode name _ size _ _)           = "<File Name=\"" ++ (xmlQuote name) ++ "\" Size=\"" ++
	                                             (show size) ++ "\"/>"
	xmlQuote [] = []
	xmlQuote ('"':xs) = "&quot;" ++ (xmlQuote xs)
	xmlQuote (x:xs) = x : (xmlQuote xs)

-- | convert TreeNode to compressed xml
treeNodeToXmlBz :: TreeNode -> L.ByteString
treeNodeToXmlBz node = (BZip.compress . C.pack . treeNodeToXml) node


xmlBzToTreeNode :: L.ByteString -> TreeNode
xmlBzToTreeNode xmlbz = (xmlToTreeNode . BZip.decompress) xmlbz

xmlToTreeNode :: L.ByteString -> TreeNode
xmlToTreeNode xml = toNode (head $ tail $ onlyElems $ parseXML xml)
    where
        toNode (Element (QName "FileListing" _ _) _ content _) = DirNode "base" "" (map toNode (onlyElems content))
        toNode (Element (QName "Directory" _ _) attr content _) = DirNode (getAttr "Name" attr) "" (map toNode (onlyElems content))
        toNode (Element (QName "File" _ _) attr _ _) = FileNode (getAttr "Name" attr) "" (read $ getAttr "Size" attr)
	                                                        someCalendarTime (Just $ getAttr "TTH" attr)
        --toNode (Element (QName node _ _) _ _ _) = FileNode node "" 0 someCalendarTime Nothing
	getAttr name attr = fromJust (lookupAttr (QName name Nothing Nothing) attr)
	someCalendarTime = CalendarTime 1970 January 1 0 0 0 0 Sunday 0 "UTC" 0 False

