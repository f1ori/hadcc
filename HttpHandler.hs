module HttpHandler where

import Network.HTTP
import Network.HTTP.Headers
import Http

mkResponse :: String -> Response String
mkResponse body = Response (2,0,0) "OK" [Header HdrContentType "text/html", Header HdrContentLength (show $ length body)] body

httpHandler :: Handler String
httpHandler appState addr url request = return (mkResponse "test")

