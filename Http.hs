--------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Florian Richter 2011
-- License     : GPL
--

module Http where

import Network.Socket
import Network.HTTP
import Network.HTTP.Headers
import Network.URI
import Control.Exception(SomeException, finally)
import Control.Concurrent(forkIO)
import Network.BufferType
import Config
import Tcp

type Handler a = AppState -> SockAddr -> URI -> Request a -> IO (Response a)

-- | HTTP server
httpServer :: HStream a => AppState -> Handler a -> IO ()
httpServer appState handler =
        tcpServer (configHttpIp $ appConfig appState) (configHttpPort $ appConfig appState) handleClient
    where
        handleClient sock addr = do
            conn <- socketConnection "bla" sock
            mbreq <- receiveHTTP conn
            resp <- case mbreq of
                    Left err -> do
                        return bad_request_response
                    Right req -> handler appState addr (rqURI req) req
                        `catch` \e -> do 
                            putStrLn (show e)
                            return internal_server_error_response
            respondHTTP conn resp
            close conn

bad_request_response :: BufferType a => Response a
bad_request_response = Response (4,0,0) "Bad request" [] (buf_empty bufferOps)
internal_server_error_response :: BufferType a => Response a
internal_server_error_response = Response (5,0,0) "Internal server error" [] (buf_empty bufferOps)

--main = httpServer "appState" handler

-- vim: sw=4 expandtab
