module DCCommon where

import Network.Socket
import System.IO
import Config
import Data.List.Split

data ConnectionState = DontKnow
                     | Upload String Integer
                     | Download String


tcpLoop :: AppState -> String -> String -> ConnectionState -> (AppState -> Handle -> IO ()) -> (AppState -> Handle -> ConnectionState -> String -> IO ConnectionState) -> IO ()
tcpLoop appState host port conState start handler = do
    addrinfos <- getAddrInfo Nothing (Just host) (Just port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    setSocketOption sock KeepAlive 1
    connect sock (addrAddress serveraddr)
    h <- socketToHandle sock ReadWriteMode
    hSetBuffering h (BlockBuffering Nothing)
    -- send some commands at start
    start appState h
    messages <- hGetContents h
    procMessages appState h conState handler (splitMessages messages)
    hClose h
    putStrLn "closed"

procMessages :: AppState -> Handle -> ConnectionState -> (AppState -> Handle -> ConnectionState -> String -> IO ConnectionState) -> [String] -> IO ()
procMessages appState h conState handler [] = return ()
procMessages appState h conState handler [""] = return ()
procMessages appState h conState handler (msg:msgs) = do
    newState <- handler appState h conState msg
    procMessages appState h newState handler msgs

splitMessages :: String -> [String]
splitMessages stream = splitOn "|" stream
    
getCmd :: String -> Maybe String
getCmd msg = if null msg then Nothing
    else if (head msg) == '$'
       then Just (takeWhile (/=' ') msg)
       else if (head msg) == '<'
           then Just "$Chat"
           else Nothing

sendCmd :: Handle -> String -> String -> IO ()
sendCmd h cmd param = do
    hPutStr h ("$" ++ cmd ++ " " ++ param ++ "|")
    hFlush h

-- vim: sw=4 expandtab
