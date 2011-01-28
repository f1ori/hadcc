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
import GHC.Exts
import Text.Printf (printf)

import Http
import FilelistTypes
import Filelist
import EventTypes
import Event
import DCToHub
import DCToClient
import Config
import HtmlTemplates
import Json

-- | main handler for http request
httpHandler :: Handler String
httpHandler appState addr url request = do
    putStrLn (uriPath url)
    case uriPath url of
        "/" -> fileResponse "text/html" "ui/index.html"
        "/hadcc.js" -> fileResponse "text/javascript" "ui/hadcc.js"
        "/index" -> return (mkResponse "text/plain" "index")
        "/nicklist" -> do
            nicklist <- getDojoNickList appState
            return (mkResponse "application/json; charset=ISO-8859-1" nicklist)
        path | (take 10 path) == "/filelist/" -> do
                    filelist <- getDojoFileList appState (urlDecode (drop 10 path))
                    return (mkResponse "application/json; charset=ISO-8859-1" filelist)
             | (take 10 path) == "/download/" -> do
                    filelist <- getDojoFileList appState (urlDecode (drop 10 path))
                    return (mkResponse "application/json" filelist)
             | (take  8 path) == "/events/" -> do
                    events <- getEvents appState (read (drop 8 path))
                    return (mkResponse "application/json; charset=ISO-8859-1" events)
             | otherwise                      -> fileHandler "ui" (urlDecode path)

-- | compose simple response object
mkResponse :: String -> String -> Response String
mkResponse contenttype body = Response (2,0,0) "OK" [Header HdrContentType contenttype, Header HdrContentLength (show $ length body)] body

-- | compose response object from file
fileResponse :: String -> FilePath -> IO (Response String)
fileResponse contenttype path = mkResponse contenttype `liftM` readFile path

-- | http handler for files
fileHandler :: FilePath -> String -> IO (Response String)
fileHandler path file = if isInfixOf ".." file
                   then return (mkResponse "text/plain" "illegal character")
                   else do
                       exists <- doesFileExist (path ++ file)
                       if exists
                           then fileResponse (toMIME (takeExtension file)) (path ++ file)
                           else return (Response (4,0,4) "Not Found" [] "")

-- | git mime type from file extension
toMIME :: String -> String
toMIME ".js"   = "text/javascript"
toMIME ".html" = "text/html"
toMIME ".css"  = "text/css"
toMIME ".png"  = "image/png"
toMIME ".gif"  = "image/gif"
toMIME _       = "application/octet-stream"


-- | get file list from peer in dojo/json format
getDojoFileList :: AppState -> Nick -> IO String
getDojoFileList appState nick = do
    filelist <- downloadFilelist appState nick
    return (toDojoFileList $ xmlBzToTreeNode $ L.fromChunks [filelist])


-- | get nicklist in dojo/json format
getDojoNickList :: AppState -> IO String
getDojoNickList appState = (dojoNicklist . sort) `liftM` getNickList appState
    where
        dojoNicklist list = objToJson [("identifier", jsquote "name"), ("label", jsquote "name"),
                                       ("items", listToJson ( map (\n -> objToJson [("name",jsquote n)]) list))]


-- | convert directory node into dojo/json string
toDojoFileList :: TreeNode -> String
toDojoFileList node = objToJson [("identifier", jsquote "id"), ("label", jsquote "name"),
                                               ("items", listToJson ((fst dirsAndFiles) ++ (snd dirsAndFiles)) )]
    where
        dirsAndFiles = nodeToJson "0" "" node

        nodeToJson :: String -> String -> TreeNode -> ([String], [String])
        nodeToJson path parent (FileNode name _ size _ (Just hash)) =
                ([], [objToJson [("id", jsquote path), ("parent", jsquote parent), ("name", jsquote name), ("type", jsquote "file"),
                           ("size", (jsquote $ prettySize size)), ("tth", jsquote hash)] ] )

        nodeToJson path parent (DirNode name _ children)            = 
                ([objToJson [("id", jsquote path), ("name", jsquote name), ("type", jsquote "dir"),
                           ("children", listToJson (concat $ map fst recursion))]],
                 concat (map snd recursion)
                )
            where recursion = (map (\(i, child) -> nodeToJson (getID path i) path child) (childrenIterator children))



        childrenIterator children = zip (iterate (+1) 0) (sortWith nodeToName children)
        getID path id = path ++ "-" ++ (show id)

-- | convert directory node into html string
toHtmlFileList :: TreeNode -> String
toHtmlFileList node = "<div class='dirs'><ul>" ++ ((\(x,_,_)->x) dirsAndFiles) ++ "</ul></div>" ++
                      ((\(_,x,_)->x) dirsAndFiles)
    where
        dirsAndFiles = nodeToHtml "0" node

        nodeToHtml :: String -> TreeNode -> (String, String, String)
        nodeToHtml path (FileNode name _ size _ (Just hash)) =
                ("", "", row [htmlquote name, prettySize size, htmlquote hash] )

        nodeToHtml path (DirNode name _ children)            = 
                (printf "<li><span onclick=\"alert(%s);\">%s</span><br/><ul>%s</ul></li>"
                        (jsquote path) (htmlquote name) (concat $ map (\(x,_,_)->x) recursion) ,
                 ( concat $ map (\(_,x,_)->x) recursion )
                     ++ (printf "<table class='files' id='%s'>%s</table>" path (concat $ map (\(_,_,x)->x) recursion) ) ,
                 ""
                )
            where recursion = (map (\(i, child) -> nodeToHtml (getID path i) child) (childrenIterator children))


        childrenIterator children = zip [0..] (sortWith nodeToName children)
        getID path id = path ++ "-" ++ (show id)
        row :: [String] -> String
        row cells = printf "<tr>%s</tr>" ((concat $ map (\c -> printf "<td>%s</td>" (htmlquote c)) cells)::String)

-- | read events from application state and format them for dojo
getEvents :: AppState -> Int -> IO String
getEvents appState lastId = do
    (newId, list) <- receiveEvents appState lastId
    return (toDojoEventList newId list)

-- | convert list of Events to json
toDojoEventList :: Int -> [Event] -> String
toDojoEventList newId events = objToJson [("newId", (show newId)), ("events", listToJson $ map eventToJson events)]
    where
        eventToJson (EChatMsg msg) = objToJson [("type", jsquote "chatmsg"), ("msg", jsquote msg)]
        eventToJson (EDownloadStarted nick file) = objToJson [
                                                          ("type", jsquote "downloadStarted"),
                                                          ("nick", jsquote nick),
                                                          ("file", jsquote file)]
        eventToJson (EDownloadFinished nick file) = objToJson [
                                                          ("type", jsquote "downloadFinished"),
                                                          ("nick", jsquote nick),
                                                          ("file", jsquote file)]
        eventToJson (EUploadStarted nick file) = objToJson [
                                                          ("type", jsquote "uploadStarted"),
                                                          ("nick", jsquote nick),
                                                          ("file", jsquote file)]
        eventToJson (EUploadFinished nick file) = objToJson [
                                                          ("type", jsquote "uploadFinished"),
                                                          ("nick", jsquote nick),
                                                          ("file", jsquote file)]
        eventToJson _                               = objToJson [("type", jsquote "unknown")]

-- | convert size in byte to string using readable units like Megabyte and Gigabyte
prettySize :: Integer -> String
prettySize size | size < 1024 = printf "%d B" size
                | size < 1024*1024 = printf "%.1f KiB" (((fromInteger size)/1024.0)::Double)
                | size < 1024*1024*1024 = printf "%.1f MiB" (((fromInteger size)/1024.0/1024.0)::Double)
                | otherwise = printf "%.1f GiB" (((fromInteger size)/1024.0/1024.0/1024.0)::Double)
-- vim: ai sw=4 expandtab
