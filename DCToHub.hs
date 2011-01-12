module DCToHub where

import System.IO
import Control.Concurrent
import Data.List.Split

import DCCommon
import DCToClient
import Config

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
                               let nicklist = filter (/="") (splitOn "$$" (tail $ dropWhile (/=' ') msg))
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
			       forkIO $ openDCConnection host port (ToClient Nothing DontKnow)
			                                 (startupClient appState) (handleClient appState)
			       return conState
        Nothing         -> do
	                       putStrLn "No Command:"
	                       putStrLn msg
			       return conState
        _               -> do
	                       putStrLn "Unkown Command:"
	                       putStrLn msg
			       return conState

-- | synchronous fetch of nicklist (threadsafe)
getNickList :: AppState -> IO [String]
getNickList appState = do
    withMVar (appHubHandle appState) $ \hubHandle -> do
        hPutStr hubHandle "$GetNickList|"
        hFlush hubHandle
    nicklist <- takeMVar (appNickList appState)
    return nicklist
