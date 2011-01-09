module HttpHandler where

import System.IO
import Network.URI
import Network.HTTP
import Network.HTTP.Headers
import Control.Concurrent
import Data.List

import Http
import Config

mkResponse :: String -> String -> Response String
mkResponse contenttype body = Response (2,0,0) "OK" [Header HdrContentType contenttype, Header HdrContentLength (show $ length body)] body

fileResponse :: String -> FilePath -> IO (Response String)
fileResponse contenttype path = do
    body <- readFile path
    return $ Response (2,0,0) "OK" [Header HdrContentType contenttype, Header HdrContentLength (show $ length body)] body

httpHandler :: Handler String
httpHandler appState addr url request = do
    case uriPath url of
        "/" -> fileResponse "text/html" "ui/index.html"
        "/index" -> return (mkResponse "text/plain" "index")
        "/nicklist" -> do
            nicklist <- getNickList appState
            return (mkResponse "text/json" nicklist)


getNickList :: AppState -> IO String
getNickList appState = do
    withMVar (appHubHandle appState) $ \hubHandle -> do
        hPutStr hubHandle "$GetNickList|"
        hFlush hubHandle
    nicklist <- takeMVar (appNickList appState)
    return (dojoNicklist nicklist)
    where
        dojoNicklist list = "{identifier:'name',label:'name',items:[\n" ++
	                    intercalate "," (map (\n->"{name:'"++ (jsquote n)++"'}\n") list) ++
			    "]}"
        jsquote [] = []
        jsquote ('\'':xs) = '\\' : '\'' : (jsquote xs)
        jsquote ('\"':xs) = '\\' : '\"' : (jsquote xs)
        jsquote ('\\':xs) = '\\' : '\\' : (jsquote xs)
        jsquote (x:xs) = x : (jsquote xs)

