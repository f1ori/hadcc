--- |
--- | This module contains the DC-stuff common to both, connections to the hub and to other peers
--- |
--- Copyright   : (c) Florian Richter 2011
--- License     : GPL
---


module DCCommon (
      State(..)
    , ConnectionState(..)
    , openDCConnection
    , startDCServer
    , getCmd
    , sendCmd
    ) where

import Network.Socket
import System.IO
import Data.List.Split
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import Foreign.Marshal.Error (void)
import Control.Exception (handle, AsyncException, finally)
import Config
import Tcp

data State = DontKnow            -- ^ Don't know, what to do with this connection, waiting for commands from peer
           | Upload L.ByteString -- ^ Remember to Upload this file on next "Send" - command
           | Download            -- ^ This connection is for downloading something from peer
           | DownloadJob Integer (Handle -> L.ByteString -> IO ()) (Handle -> IO Bool)
                                 -- ^ initiate download in message handler
                                 --   the Integer 
                                 --   the function is the download handler,
                                 --   the return value indicates, if the connection should
                                 --   be closed after the download

-- | connection state
data ConnectionState = ToClient (Maybe Nick) State
                     | ToHub

-- | start client connection
openDCConnection :: String   -- ^ host name to connect
                 -> String   -- ^ port number
                 -> ConnectionState -- ^ initial connection state
                 -> (Handle -> IO ()) -- ^ handler called right after connection is established
                 -> (Handle -> ConnectionState -> String -> IO ConnectionState) -- ^ handler called for every DC message
                 -> (ConnectionState -> IO ()) -- ^ handler called when connection closed
                 -> IO ()
openDCConnection host port conState startHandler mainHandler stopHandler = do
    (sock, addr) <- tcpClientConnect host port
    dcConnectionHandler conState startHandler mainHandler stopHandler sock addr

-- | start listening for connections (blocks)
startDCServer :: String -- ^ hostname/ip to listen on
              -> String -- ^ port number to listen on
              -> ConnectionState -- ^ initial connection state
              -> (Handle -> IO ()) -- ^ handler called right after connection is established
              -> (Handle -> ConnectionState -> String -> IO ConnectionState) -- ^ handler called for every DC message
              -> (ConnectionState -> IO ()) -- ^ handler called when connection terminated
              -> IO ()
startDCServer ip port conState startHandler mainHandler stopHandler =
    tcpServer ip port (dcConnectionHandler conState startHandler mainHandler stopHandler)

------------------------------------------------------------

-- | handles dc connection on socket level
dcConnectionHandler :: ConnectionState
                    -> (Handle -> IO ()) -- ^ handler called right after connection is established
                    -> (Handle -> ConnectionState -> String -> IO ConnectionState) -- ^ handler called for every DC message
                    -> (ConnectionState -> IO ()) -- ^ handler called after connection terminated
                    -> Socket
                    -> SockAddr
                    -> IO ()
dcConnectionHandler conState startHandler mainHandler stopHandler sock addr = do
    h <- socketToHandle sock ReadWriteMode
    hSetBuffering h (BlockBuffering Nothing)
    -- send some commands at start
    startHandler h
    content <- L.hGetContents h
    lastConState <- procMessages h conState mainHandler content
    hClose h
    stopHandler lastConState
    putStrLn "closed"

------------------------------------------------------

-- | process messages from hub or peer
procMessages :: Handle
             -> ConnectionState
             -> (Handle -> ConnectionState -> String -> IO ConnectionState)
             -> L.ByteString
             -> IO ConnectionState
procMessages h conState handler content | L.null content = return conState
                                        | otherwise = do
    let (msg, msgs_with_pipe) = L.break (==0x7c) content
    let msgs = L.tail msgs_with_pipe
    (newState2, next_msgs, finish) <- do {
        newState <- handler h conState (C.unpack msg);
        case newState of
            ToClient nick (DownloadJob size downloadHandler nextHandler) -> do
                        putStrLn "Download File"
                        let (file, next_msgs) = L.splitAt (fromInteger size) msgs
                        downloadHandler h file
                        finish <- nextHandler h
                        return ((ToClient nick Download), next_msgs, finish)
            _ -> return (newState, msgs, False)
    } `catch` (\e -> return (conState, msgs, True))
    if not finish
         then procMessages h newState2 handler next_msgs
         else return newState2


-- | extracts command from DC message
getCmd :: String -> Maybe String
getCmd msg = if null msg then Nothing
    else if (head msg) == '$'
       then Just (takeWhile (/=' ') msg)
       else if (head msg) == '<'
           then Just "$Chat"
           else Nothing


-- | send DC command via socket handle (so you don't get the formatation wrong :) )
sendCmd :: Handle -- ^ socket handle
        -> String -- ^ command
        -> String -- ^ arguments
        -> IO ()
sendCmd h cmd param = do
    hPutStr h ("$" ++ cmd ++ " " ++ param ++ "|")
    hFlush h

-- vim: sw=4 expandtab
