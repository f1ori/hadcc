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
import qualified Data.ByteString.Char8 as SC
import qualified Codec.Compression.BZip as BZip
import qualified Data.Text as T
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import Data.Monoid (mappend, mconcat)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Text.XML.Expat.SAX
import Data.List
import Data.Maybe
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
	          then maybeCatch (getFileList appState path)
	          else maybeCatch (getFile appState path)
	     )
    return (DirNode (T.pack $ last (splitDirectories dir)) (T.pack dir) (catMaybes nodes))
    where
        maybeCatch :: IO a -> IO (Maybe a)
        maybeCatch func = catch (Just `liftM` func) (\e-> return Nothing)

-- | create TreeNode object for file in filesystem (hash is retreived from cache if available)
getFile :: AppState -> FilePath -> IO TreeNode
getFile appState path = do
    fileStatus <- getFileStatus path
    let size = fromIntegral $ fileSize fileStatus
    let modTime = modificationTime fileStatus
    hash <- getCachedHash appState (T.pack path) modTime
    return ( hash `seq` FileNode (T.pack $ takeFileName path) (T.pack path) size modTime hash )


-- | get size of file from filesystem
getSystemFileSize :: FilePath -> IO Integer
getSystemFileSize path = do
    h <- openFile path ReadMode
    size <- hFileSize h
    hClose h
    return size

-- | get useful contents of a directory (not . or ..)
getUsefulContents :: String -> IO [String]
getUsefulContents path = do
    names <- getDirectoryContents path
    return (map (T.unpack . (decodeUtf8With lenientDecode) . SC.pack) $ filter (`notElem` [".", ".."]) names)


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
searchFile :: T.Text -> TreeNode -> Maybe TreeNode
searchFile path tree =
    case searchNode path tree of
        Just (file@(FileNode _ _ _ _ _)) -> Just file
	_                                -> Nothing

-- | search Node in TreeNode by path
searchNode :: T.Text -> TreeNode -> Maybe TreeNode
searchNode path file@(FileNode name _ _ _ _)
        | path == name               = Just file
	| otherwise                  = Nothing
searchNode path dir@(DirNode name _ children)
        | path == name               = Just dir
        | (firstPath path) == name   = firstNotNothing $ map (searchNode (restPath path)) children
	| otherwise                  = Nothing
    where
	firstPath path = T.takeWhile (/='/') path
        restPath str = T.tail $ T.dropWhile (/='/') path

-- | search hash in TreeNode
searchHash :: T.Text -> TreeNode -> Maybe TreeNode
searchHash hash file@(FileNode _ _ _ _ (Just fhash))
        | fhash == hash                = Just file
	| otherwise                    = Nothing
searchHash hash (FileNode _ _ _ _ _)   = Nothing
searchHash hash (DirNode _ _ children) = firstNotNothing $ map (searchHash hash) children


-- | convert TreeNode to xml
treeNodeToXml :: TreeNode -> Builder
treeNodeToXml node = (fromString "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n") `mappend`
                  (fromString "<FileListing Version=\"1\" Generator=\"hdc V:0.1\">") `mappend`
                  (toXml node) `mappend` (fromString "</FileListing>")

    where
        toXml (DirNode name _ children)            = mconcat
                                                         [fromString "<Directory Name=\"", xmlQuote name, fromString "\">",
                                                          mconcat $ map toXml children, fromString "</Directory>" ]
        toXml (FileNode name _ size _ (Just hash)) = mconcat [fromString "<File Name=\"", xmlQuote name,
                                                              fromString "\" Size=\"", fromString $ show size,
                                                              fromString "\" TTH=\"", fromText hash, fromString "\"/>" ]
        toXml (FileNode name _ size _ _)           = mconcat [fromString "<File Name=\"", xmlQuote name,
                                                              fromString "\" Size=\"", fromString $ show size, fromString "\"/>" ]
        xmlQuote :: T.Text -> Builder
        xmlQuote = fromText
                 . T.replace (T.singleton '"') (T.pack "&quot;")
                 . T.replace (T.singleton '&') (T.pack "&amp;")
                 . T.replace (T.singleton '<') (T.pack "&lt;")
                 . T.replace (T.singleton '>') (T.pack "&gt;")


-- | convert TreeNode to compressed xml
treeNodeToXmlBz :: TreeNode -> L.ByteString
treeNodeToXmlBz node = (BZip.compress . toLazyByteString . treeNodeToXml) node


-- | convert compressed xml to TreeNode object (this is, what you normally need)
xmlBzToTreeNode :: L.ByteString -> TreeNode
xmlBzToTreeNode xmlbz = (xmlToTreeNode . BZip.decompress) xmlbz

-- | helper function, to extract attribute value from attributelist
getAttr :: [(T.Text, T.Text)] -> String -> T.Text
getAttr attrs name = fromJust $ lookup (T.pack name) attrs

-- | helper function to add a node to a directory
addToDir :: TreeNode -> TreeNode -> TreeNode
addToDir node (DirNode name path children) = DirNode name path (node:children)

-- | parse tagsoup tag on TreeNode stack
processXmlTag :: [TreeNode] -> SAXEvent T.Text T.Text -> [TreeNode]
processXmlTag result (XMLDeclaration _ _ _) = result
processXmlTag result (StartElement tag attrs)
                           | tag == (T.pack "FileListing") = [DirNode (T.pack "base") T.empty []] 
                           | tag == (T.pack "Directory")   = (DirNode (getAttr attrs "Name") T.empty []) : result
                           | tag == (T.pack "File")        = let file = FileNode (getAttr attrs "Name") T.empty 
                                                                           (read $ T.unpack $ getAttr attrs "Size") 0
                                                                           (Just $ getAttr attrs "TTH")
                                                           in (addToDir file (head result)) : (tail result)
                           | otherwise = error ("unknown tag: " ++ (show tag))
processXmlTag result (EndElement tag)
                           | tag == (T.pack "File") = result
                           | tag == (T.pack "Directory") = addToDir (result !! 0) (result !! 1) : (tail $ tail result)
                           | tag == (T.pack "FileListing") = result
                           | otherwise = error ("unknown close tag: " ++ (show tag))
processXmlTag result (CharacterData _)           = result
processXmlTag result (StartCData)                = result
processXmlTag result (EndCData)                  = result
processXmlTag result (ProcessingInstruction _ _) = result
processXmlTag result (Comment _)                 = result
processXmlTag result (FailDocument msg)          = error ("parsing error: " ++ (show msg))
        

-- | convert xml to TreeNode object
xmlToTreeNode :: L.ByteString -> TreeNode
xmlToTreeNode xml = head $ foldl' processXmlTag [] (parseHere xml)

parseHere xml = (parse defaultParseOptions xml)

-- | get name of TreeNode object (directory name or filename)
nodeToName :: TreeNode -> T.Text
nodeToName (DirNode name _ _) = name
nodeToName (FileNode name _ _ _ _) = name

-- vim: sw=4 expandtab
