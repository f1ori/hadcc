module DCCommon where

import Network.Socket
import System.IO
import Data.List.Split
import Foreign.Marshal.Error (void)
import Control.Exception (handle, AsyncException, finally)
import Config
import Tcp

data State = DontKnow
           | Upload String Integer
           | Download
-- | connection state
data ConnectionState = ToClient (Maybe String) State
                     | ToHub


-- | set nickname in ConnectionState
setNickInState :: ConnectionState -> String -> ConnectionState
setNickInState (ToClient _ state) nick = ToClient (Just nick) state

-------------------------------------------------------------

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
    messages <- hGetContents h
    (procMessages h conState mainHandler (splitMessages messages)) `finally` hClose h
    putStrLn "closed"

------------------------------------------------------

-- | process messages from hub or peer
procMessages :: Handle -> ConnectionState -> (Handle -> ConnectionState -> String -> IO ConnectionState) -> [String] -> IO ()
procMessages h conState handler [] = return ()
procMessages h conState handler [""] = return ()
procMessages h conState handler (msg:msgs) = do
    newState <- handler h conState msg
    procMessages h newState handler msgs


-- | split messages from hub or peer
splitMessages :: String -> [String]
splitMessages stream = splitOn "|" stream
    

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
