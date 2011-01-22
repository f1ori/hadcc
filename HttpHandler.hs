module HttpHandler where

import System.IO
import System.Directory (doesFileExist)
import System.FilePath (takeExtension)
import Network.URI
import Network.HTTP
import Network.HTTP.Headers
import Control.Concurrent
import Control.Monad (liftM)
import Data.List
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Text.XML.Light
import GHC.Exts
import Text.Printf (printf)

import Http
import Filelist
import FilelistTypes
import DCToHub
import DCToClient
import Config

mkResponse :: String -> String -> Response String
mkResponse contenttype body = Response (2,0,0) "OK" [Header HdrContentType contenttype, Header HdrContentLength (show $ length body)] body

fileResponse :: String -> FilePath -> IO (Response String)
fileResponse contenttype path = mkResponse contenttype `liftM` readFile path

httpHandler :: Handler String
httpHandler appState addr url request = do
    putStrLn (uriPath url)
    case uriPath url of
        "/" -> fileResponse "text/html" "ui/index.html"
        "/hadcc.js" -> fileResponse "text/javascript" "ui/hadcc.js"
        "/index" -> return (mkResponse "text/plain" "index")
        "/nicklist" -> do
            nicklist <- getDojoNickList appState
            return (mkResponse "text/json; charset=ISO-8859-1" nicklist)
        path | (take 10 path) == "/filelist/" -> do
                    filelist <- getDojoFileList appState (urlDecode (drop 10 path))
                    return (mkResponse "text/json" filelist)
             | (take 10 path) == "/download/" -> do
                    filelist <- getDojoFileList appState (urlDecode (drop 10 path))
                    return (mkResponse "text/json" filelist)
             | otherwise                      -> fileHandler (urlDecode path)

fileHandler :: String -> IO (Response String)
fileHandler file = if isInfixOf ".." file
                   then return (mkResponse "text/plain" "illegal character")
                   else do
                       exists <- doesFileExist ("ui/qooxdoo/hadcc/build" ++ file)
                       if exists
                           then fileResponse (toMIME (takeExtension file)) ("ui/qooxdoo/hadcc/build" ++ file)
                           else return (Response (4,0,4) "Not Found" [] "")

toMIME :: String -> String
toMIME ".js"   = "text/javascript"
toMIME ".html" = "text/html"
toMIME ".css"  = "text/css"
toMIME ".png"  = "image/png"
toMIME ".gif"  = "image/gif"
toMIME _       = "application/octet-stream"


getDojoFileList :: AppState -> Nick -> IO String
getDojoFileList appState nick = do
    filelist <- downloadFilelist appState nick
    return (toDojoFileList3 $ xmlBzToTreeNode $ L.fromChunks [filelist])


-- | get nicklist in dojo/json format
getDojoNickList :: AppState -> IO String
getDojoNickList appState = (dojoNicklist . sort) `liftM` getNickList appState
    where
        dojoNicklist list = objToJson [("identifier", jsquote "name"), ("label", jsquote "name"),
                                       ("items", listToJson ( map (\n -> objToJson [("name",jsquote n)]) list))]


-- | convert directory node into dojo/json string
toDojoFileList3 :: TreeNode -> String
toDojoFileList3 node = objToJson [("identifier", jsquote "id"), ("label", jsquote "name"),
                                               ("items", listToJson ((fst dirsAndFiles) ++ (snd dirsAndFiles)) )]
    where
        dirsAndFiles = nodeToJson "0" "" node

        nodeToJson :: String -> String -> TreeNode -> ([String], [String])
        nodeToJson path parent (FileNode name _ size _ (Just hash)) =
                ([], [objToJson [("id", jsquote path), ("parent", jsquote parent), ("name", jsquote name), ("type", jsquote "file"),
                           ("size", (prettySize size)), ("tth", jsquote hash)] ] )

        nodeToJson path parent (DirNode name _ children)            = 
                ([objToJson [("id", jsquote path), ("name", jsquote name), ("type", jsquote "dir"),
                           ("children", listToJson (concat $ map fst recursion))]],
                 concat (map snd recursion)
                )
            where recursion = (map (\(i, child) -> nodeToJson (getID path i) path child) (childrenIterator children))



        childrenIterator children = zip (iterate (+1) 0) (sortWith nodeToName children)
        getID path id = path ++ "-" ++ (show id)


-- | quote string for javascript/json
jsquote :: String -> String
jsquote str = "'" ++ (quote str) ++ "'"
    where
        quote [] = []
        quote ('\'':xs) = '\\' : '\'' : (quote xs)
        quote ('\"':xs) = '\\' : '\"' : (quote xs)
        quote ('\\':xs) = '\\' : '\\' : (quote xs)
        quote (x:xs) = x : (quote xs)

-- | convert haskell list to json list
listToJson :: [String] -> String
listToJson list = "[" ++ intercalate "," list ++ "]"

-- | convert haskell lookup-list into json object (don't forget to quote the strings)
objToJson :: [(String, String)] -> String
objToJson list = "{" ++ intercalate "," (toJson list) ++ "}\n"
    where
        toJson [] = []
        toJson ((x1, x2) : xs) = (x1 ++ ":" ++ x2) : (toJson xs)

prettySize :: Integer -> String
prettySize size | size < 1024 = printf "%d B" size
                | size < 1024*1024 = printf "%.1f KiB" (((fromInteger size)/1024.0)::Double)
                | otherwise = printf "%.1f GiB" (((fromInteger size)/1024.0/1024.0)::Double)
-- vim: ai sw=4 expandtab
