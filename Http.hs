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
import Control.Exception(SomeException)
import Control.Concurrent(forkIO)
import Network.BufferType
import Config

type Handler a = AppState -> SockAddr -> URI -> Request a -> IO (Response a)

initHttpServer :: AppState -> IO Socket
initHttpServer appState = withSocketsDo $ do
    addrinfos <- getAddrInfo Nothing (Just (configHttpIp $ appConfig appState)) (Just (configHttpPort $ appConfig appState))
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    --setSocketOption sock ReuseAddr 1
    bindSocket sock (addrAddress serveraddr)
    -- accept maximal 5 connections
    listen sock 5
    return sock


-- | ̣StarHTTP server
httpServer :: HStream a => AppState -> Handler a -> IO ()
httpServer appState handler = withSocketsDo $
    do
        sock <- initHttpServer appState
        loop sock `catch` \e ->
            putStr (show e)
        sClose sock
    where
        loop sock = do
                        (connsock, clientaddr) <- accept sock
                        forkIO $ handleClient connsock clientaddr
                        loop sock
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

--main = httpServer "appState" handler

bad_request_response :: BufferType a => Response a
bad_request_response = Response (4,0,0) "Bad request" [] (buf_empty bufferOps)
internal_server_error_response :: BufferType a => Response a
internal_server_error_response = Response (5,0,0) "Internal server error" [] (buf_empty bufferOps)


{-
import Network.HTTP.Server
import Network.HTTP.Server.Response
import Network.HTTP.Server.Logger

serverConfig = Config stdLogger "127.0.0.1" 8000

mkResponse :: String -> Response String
mkResponse body = Response (2,0,0) "OK" [Header HdrContentType "text/html", Header HdrContentLength (show $ length body)] body

httpHandler :: Handler String
httpHandler addr url request = return (mkResponse "<html><body><h1>It works</h1></body></html>")

main = serverWith serverConfig httpHandler 
-}


-- vim: sw=4 expandtab
