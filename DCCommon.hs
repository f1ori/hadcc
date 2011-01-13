module DCCommon where

import Network.Socket
import System.IO
import Data.List.Split
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import Foreign.Marshal.Error (void)
import Control.Exception (handle, AsyncException, finally)
import Config
import Tcp

data State = DontKnow
           | Upload String Integer
           | Download
           | DownloadJob Integer (L.ByteString -> IO ())
-- | connection state
data ConnectionState = ToClient (Maybe Nick) State
                     | ToHub

-- | start client connection
openDCConnection :: String   -- ^ host name to connect
                 -> String   -- ^ port number
                 -> ConnectionState -- ^ initial connection state
                 -> (Handle -> IO ()) -- ^ handler called right after connection is established
                 -> (Handle -> ConnectionState -> String -> IO ConnectionState) -- ^ handler called for every DC message
                 -> IO ()
openDCConnection host port conState startHandler mainHandler = do
    (sock, addr) <- tcpClientConnect host port
    dcConnectionHandler conState startHandler mainHandler sock addr

-- | start listening for connections (blocks)
startDCServer :: String -- ^ hostname/ip to listen on
              -> String -- ^ port number to listen on
              -> ConnectionState -- ^ initial connection state
              -> (Handle -> IO ()) -- ^ handler called right after connection is established
              -> (Handle -> ConnectionState -> String -> IO ConnectionState) -- ^ handler called for every DC message
              -> IO ()
startDCServer ip port conState startHandler mainHandler =
    tcpServer ip port (dcConnectionHandler conState startHandler mainHandler)

------------------------------------------------------------

-- | handles dc connection on socket level
dcConnectionHandler :: ConnectionState
                    -> (Handle -> IO ()) -- ^ handler called right after connection is established
                    -> (Handle -> ConnectionState -> String -> IO ConnectionState) -- ^ handler called for every DC message
                    -> Socket
                    -> SockAddr
                    -> IO ()
dcConnectionHandler conState startHandler mainHandler sock addr = do
    h <- socketToHandle sock ReadWriteMode
    hSetBuffering h (BlockBuffering Nothing)
    -- send some commands at start
    startHandler h
    content <- L.hGetContents h
    (procMessages h conState mainHandler content) `finally` hClose h
    putStrLn "closed"

------------------------------------------------------

-- | process messages from hub or peer
procMessages :: Handle -> ConnectionState -> (Handle -> ConnectionState -> String -> IO ConnectionState) -> L.ByteString -> IO ()
procMessages h conState handler content | L.null content = return ()
                                        | otherwise = do
    let (msg, msgs_with_pipe) = L.break (==0x7c) content
    let msgs = L.tail msgs_with_pipe
    newState <- handler h conState (C.unpack msg)
    (newState2, msgs2) <- case newState of
        ToClient nick (DownloadJob size downloadHandler) -> do
                    putStrLn "Download File"
                    let (file, new_msgs) = L.splitAt (fromInteger size) msgs
                    downloadHandler file
                    return ((ToClient nick Download), new_msgs)
        _ -> return (newState, msgs)
    procMessages h newState2 handler msgs2


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
