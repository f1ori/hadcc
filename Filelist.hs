--- |
--- | This module contains stuff to handle and convert Filelists alias TreeNodes
--- |
--- Copyright   : (c) Florian Richter 2011
--- License     : GPL
---

module Filelist where

import System.Directory
import System.FilePath
import System.Posix.Files
import System.IO
import Data.Maybe
import Control.Monad
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Codec.Compression.BZip as BZip
import Text.XML.HXT.DOM.TypeDefs
import Data.Tree.NTree.TypeDefs
import Text.XML.HXT.Parser.XmlParsec
import Text.XML.HXT.DOM.ShowXml


import FilelistTypes
import TTH
import Config


-- | create TreeNode tree from filesystem directory
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

-- | create TreeNode object for file in filesystem (hash is retreived from cache if available)
getFile :: AppState -> FilePath -> IO TreeNode
getFile appState path = do
    fileStatus <- getFileStatus path
    let size = fromIntegral $ fileSize fileStatus
    let modTime = modificationTime fileStatus
    hash <- getCachedHash appState path modTime
    return ( FileNode (takeFileName path) path size modTime hash )


-- | get size of file from filesystem
getSystemFileSize :: FilePath -> IO Integer
getSystemFileSize path = do
    h <- openFile path ReadMode
    size <- hFileSize h
    hClose h
    return size

-- | get useful contents of a directory (not . or ..)
getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
    names <- getDirectoryContents path
    return (filter (`notElem` [".", ".."]) names)


-- | accumlulate filesizes of all files in tree
treeSize :: TreeNode -> Integer
treeSize (DirNode _ _ children)  = sum $ map treeSize children
treeSize (FileNode _ _ size _ _) = size


-- | get return first value in list, which is not Nothing
firstNotNothing :: [Maybe a] -> Maybe a
firstNotNothing [] = Nothing
firstNotNothing ((Just x):xs) = Just x
firstNotNothing (Nothing:xs) = firstNotNothing xs

-- | search FileNode in TreeNode by path
searchFile :: String -> TreeNode -> Maybe TreeNode
searchFile path tree =
    case searchNode path tree of
        Just (file@(FileNode _ _ _ _ _)) -> Just file
	_                                -> Nothing

-- | search Node in TreeNode by path
searchNode :: String -> TreeNode -> Maybe TreeNode
searchNode path file@(FileNode name _ _ _ _)
        | path == name               = Just file
	| otherwise                  = Nothing
searchNode path dir@(DirNode name _ children)
        | path == name               = Just dir
        | (firstPath path) == name   = firstNotNothing $ map (searchNode (restPath path)) children
	| otherwise                  = Nothing
    where
	firstPath path = takeWhile (/='/') path
	restPath (x:xs) | x=='/'    = xs
	                | otherwise = restPath xs

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


-- | convert compressed xml to TreeNode object (this is, what you normally need)
xmlBzToTreeNode :: L.ByteString -> TreeNode
xmlBzToTreeNode xmlbz = (xmlToTreeNode . BZip.decompress) xmlbz

-- | convert plain xml to TreeNode object
xmlToTreeNode :: L.ByteString -> TreeNode
xmlToTreeNode xml = toNode (head $ onlyTags $ xread $ dropXmlDecl $ C.unpack xml)
    where
        toNode (NTree (XTag tag attr) children)
            | localPart tag == "FileListing" = DirNode "base" "" (map toNode (onlyTags children))
            | localPart tag == "Directory"   = DirNode (getAttr "Name" attr) "" (map toNode (onlyTags children))
            | localPart tag == "File"        = FileNode (getAttr "Name" attr) "" (read $ getAttr "Size" attr)
                                                        0 (Just $ getAttr "TTH" attr)

        getAttr :: String -> XmlTrees -> String
        getAttr name attrs = let Just value = lookup name $ map getAttrNameValue attrs in value

        getAttrNameValue :: XmlTree -> (String, String)
        getAttrNameValue (NTree (XAttr name) value) = (localPart name, xshow value)

        onlyTags :: XmlTrees -> XmlTrees
        onlyTags children = filter isTag children

        isTag :: XmlTree -> Bool
        isTag (NTree (XTag _ _) _) = True
        isTag _          = False

-- | hack to remove <?xml-declaration, xread can't handle this
dropXmlDecl ('<':'?':'x':'m':'l':rest) = tail $ dropWhile (/='>') rest
dropXmlDecl rest = rest

-- | print hxt parsing output for debugging
printFilelist :: L.ByteString -> String
printFilelist filelist = show $ xread $ dropXmlDecl $ C.unpack $ BZip.decompress filelist

-- | get name of TreeNode object (directory name or filename)
nodeToName :: TreeNode -> String
nodeToName (DirNode name _ _) = name
nodeToName (FileNode name _ _ _ _) = name

-- vim: sw=4 expandtab
