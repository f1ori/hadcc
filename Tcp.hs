module Tcp where

import Network.Socket
import Control.Exception (finally)
import Control.Concurrent (forkIO)

initTcpServer :: String -> String -> IO Socket
initTcpServer ip port = withSocketsDo $ do
    addrinfos <- getAddrInfo Nothing (Just ip) (Just port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    bindSocket sock (addrAddress serveraddr)
    -- accept maximal 8 connections
    listen sock 8
    return sock


-- | HTTP server
tcpServer :: String -> String -> (Socket -> SockAddr -> IO ()) -> IO ()
tcpServer ip port handler = withSocketsDo $
    do
        sock <- initTcpServer ip port
        (loop sock `catch` \e ->
            putStr (show e)) `finally` sClose sock
    where
        loop sock = do
                        (connsock, clientaddr) <- accept sock
                        forkIO $ handler connsock clientaddr
                        loop sock

tcpClientConnect :: String -> String -> IO (Socket, SockAddr)
tcpClientConnect host port = withSocketsDo $ do
    addrinfos <- getAddrInfo Nothing (Just host) (Just port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    setSocketOption sock KeepAlive 1
    connect sock (addrAddress serveraddr)
    return (sock, (addrAddress serveraddr))
