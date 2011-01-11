module HttpHandler where

import System.IO
import Network.URI
import Network.HTTP
import Network.HTTP.Headers
import Control.Concurrent
import Control.Monad (liftM)
import Data.List

import Http
import DCToHub
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
            return (mkResponse "text/json" nicklist)


getDojoNickList :: AppState -> IO String
getDojoNickList appState = dojoNicklist `liftM` getNickList appState
    where
        dojoNicklist list = "{identifier:'name',label:'name',items:[\n" ++
	                    intercalate "," (map (\n->"{name:'"++ (jsquote n)++"'}\n") list) ++
			    "]}"
        jsquote [] = []
        jsquote ('\'':xs) = '\\' : '\'' : (jsquote xs)
        jsquote ('\"':xs) = '\\' : '\"' : (jsquote xs)
        jsquote ('\\':xs) = '\\' : '\\' : (jsquote xs)
        jsquote (x:xs) = x : (jsquote xs)

