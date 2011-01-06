{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Bits
import Network.Socket
import System.IO
import Data.List.Split
import Control.Monad.State
import Control.Concurrent
import qualified Data.ByteString.Lazy as L
import Filemgmt
import Filelist
import Config
import Http
import HttpHandler


-- type Connection = StateT ConnectionState IO

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

startupHub :: AppState -> Handle -> IO ()
startupHub appState h = putMVar (appHubHandle appState) h -- save handle for syncronisation

handleHub :: AppState -> Handle -> ConnectionState -> String -> IO ConnectionState
handleHub appState h conState msg = do
    case getCmd msg of
        Just "$Lock"    -> do
	                       putStrLn "Lock"
                               withMVar (appHubHandle appState) $ \hubHandle -> do
	                           hPutStr hubHandle "$Key ........A .....0.0. 0. 0. 0. 0. 0.|"
	                           hPutStr hubHandle ("$ValidateNick " ++ (configNick $ appConfig appState) ++ "|")
			           hFlush hubHandle
			       return conState
        Just "$HubName" -> do
	                       putStrLn ("Hubname " ++ msg)
			       return conState
        Just "$Hello"   -> do
	                       putStrLn "gogogo"
                               withMVar (appHubHandle appState) $ \hubHandle -> do
	                           hPutStr hubHandle "$Version 1,0091|"
			           -- http://www.teamfair.info/wiki/index.php?title=$MyINFO
	                           hPutStr hubHandle (getMyINFOStr appState)
			           hFlush hubHandle
			       return conState
        Just "$MyINFO" -> do
	                       putStrLn ("Nickname update " ++ ((splitOn " " msg) !! 2))
			       return conState
        Just "$NickList" -> do
	                       putStrLn ("Nicklist: " ++ msg)
                               let nicklist = splitOn "$$" (tail $ dropWhile (/=' ') msg)
                               putMVar (appNickList appState) nicklist
			       return conState
        Just "$Chat" -> do
	                       putStrLn ("Chat: " ++ msg)
			       return conState
        Just "$ConnectToMe" -> do
	                       let hostport = last (splitOn " " msg)
			       let host = takeWhile (/=':') hostport
			       let port = tail $ dropWhile (/=':') hostport
	                       putStrLn ("Connect to me " ++ hostport)
			       forkIO $ tcpLoop appState host port DontKnow startupClient handleClient
			       return conState
        Nothing         -> do
	                       putStrLn "No Command:"
	                       putStrLn msg
			       return conState
        _               -> do
	                       putStrLn "Unkown Command:"
	                       putStrLn msg
			       return conState

sendCmd :: Handle -> String -> IO ()
sendCmd h cmd = do
    hPutStr h cmd
    hFlush h

startupClient :: AppState -> Handle -> IO ()
startupClient appState h = do
    putStrLn "startup client"
    hPutStr h ("$MyNick " ++ (configNick $ appConfig appState) ++ "|$Lock EXTENDEDPROTOCOLABCABCABCABCABCABC Pk=HASKELLDC00.668ABCABC|")
    hFlush h

handleClient :: AppState -> Handle -> ConnectionState -> String -> IO ConnectionState
handleClient appState h conState msg = do
    case getCmd msg of
        Just "$MyNick"    -> do
	                        putStrLn "GotNick"
				return conState
        Just "$Lock"    -> do
	                       putStrLn "Lock"
			       return conState
        Just "$Key"      -> do
			       return conState
        Just "$Quit"      -> do
			       return conState
        Just "$Supports"    -> do
	                        putStrLn "Supports"
				return conState
        Just "$Direction"   -> do
	                       putStrLn "Direction"
	                       hPutStr h "$Supports XmlBZList|"
	                       hPutStr h "$Direction Upload 6452|"
	                       hPutStr h "$Key ........A .....0.0. 0. 0. 0. 0. 0.|"
			       hFlush h
			       return conState
        Just "$Get"   -> do
	                       putStrLn "Get"
	                       let filenameOffset = tail $ dropWhile (/=' ') msg
			       let filename = takeWhile (/='$') filenameOffset
			       let offset = read $ tail $ dropWhile (/='$') filenameOffset
			       filelength <- getFileSize appState filename
                               case filelength of
                                   Just n -> do
                                       putStrLn ("Filename: " ++ filename)
                                       putStrLn ("Filesize: " ++ (show n))
                                       hPutStr h ("$FileLength " ++ (show n) ++ "|")
                                       hFlush h
                                       return (Upload filename offset)
                                   Nothing -> do
                                       putStrLn ("File not Found: " ++ filename)
                                       hPutStr h ("$Error File not Available|")
                                       hFlush h
                                       return (Upload filename offset)
        Just "$Send"   -> case conState of
	                  (Upload filename offset) -> do
	                       putStrLn "Send raw data"
                               content <- getFileContent appState filename offset
                               case content of
                                   Just c -> do
	                               L.hPutStr h c
			               hFlush h
			               hClose h
			               return DontKnow
                                   Nothing -> do
	                               hPutStr h "$Error File not found|"
			               hFlush h
			               return DontKnow
	                  _ -> do
	                       putStrLn "Send without get"
	                       putStrLn msg
	                       hPutStr h "$Error no send before get|"
			       hFlush h
			       return conState
        Nothing         -> do
	                       putStrLn "No Command:"
	                       putStrLn msg
			       return conState
        _                -> do
	                       putStrLn "Unkown command:"
	                       putStrLn msg
			       return conState

main = do
    config <- loadConfig "Hadcc.cfg"
    appState <- newAppState config
    forkIO $ httpServer appState httpHandler
    tcpLoop appState (configHubIp config) (configHubPort config) DontKnow startupHub handleHub

-- vim: sw=4 expandtab
