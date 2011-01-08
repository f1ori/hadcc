module Filelist where

import System.Directory
import System.FilePath
import System.IO
import Time
import Control.Monad
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Codec.Compression.BZip as BZip


import FilelistTypes
import TTH
import Config


getFileList :: AppState -> FilePath -> IO Node
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


getFile :: AppState -> FilePath -> IO Node
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

traverse :: (Node -> t) -> (Node -> t) -> Node -> [t]
traverse startNode endNode node = case node of
    DirNode _ _ children -> [startNode node] ++
                            (concat (map (traverse startNode endNode) children) ) ++
			    [endNode node]
    FileNode _ _ _ _ _   -> [startNode node]

treeSize :: Node -> Integer
treeSize node = sum (traverse getSize (\n -> 0) node)
    where
        getSize (DirNode _ _ _) = 0
        getSize (FileNode _ _ size _ _) = size

searchFile :: String -> Node -> Maybe Node
searchFile path root = case root of
    FileNode name _ _ _ _ -> if path == name then Just root else Nothing
    DirNode name _ children -> if (firstPath path) == name
                               then firstNotNothing $ map (searchFile (restPath path)) children
			       else Nothing
    where
        firstNotNothing [] = Nothing
        firstNotNothing ((Just x):xs) = Just x
        firstNotNothing (Nothing:xs) = firstNotNothing xs

	firstPath path = takeWhile (/='/') path
	restPath path = tail $ dropWhile (/='/') path


getXmlList :: Node -> String
getXmlList node = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n" ++ 
        "<FileListing Version=\"1\" Generator=\"hdc V:0.1\">" ++ 
        (getXmlListRec node) ++ "</FileListing>"
    where
        getXmlListRec :: Node -> String
        getXmlListRec node = concat (traverse startNode endNode node)
	-- TODO: escape
	startNode (DirNode name _ _) = "<Directory Name=\"" ++ name ++ "\">"
	startNode (FileNode name _ size _ (Just hash)) = "<File Name=\"" ++ name ++ "\" Size=\"" ++ (show size) ++ "\" TTH=\"" ++ hash ++ "\"/>"
	startNode (FileNode name _ size _ _) = "<File Name=\"" ++ name ++ "\" Size=\"" ++ (show size) ++ "\"/>"
	endNode (DirNode name _ _) = "</Directory>"
	endNode _ = ""

getXmlBZList :: Node -> L.ByteString
getXmlBZList node = (BZip.compress . C.pack . getXmlList) node

--main = getFileList "/mnt/music" >>= print
--main = do
--    list <- getFileList "/mnt/music"
--    print (treeSize list)
--    print (getXmlList list)
