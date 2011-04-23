module DCToHub where

import System.IO
import Control.Concurrent
import Control.Monad
import Control.Exception.Base
import Data.List.Split
import qualified Data.Map as M

import DCCommon
import DCToClient
import Event
import EventTypes
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
	                           hPutStr hubHandle "$GetNickList|"
			           -- http://www.teamfair.info/wiki/index.php?title=$MyINFO
	                           hPutStr hubHandle (getMyINFOStr appState)
			           hFlush hubHandle
			       return conState
        Just "$MyINFO" -> do
	                       putStrLn ("Nickname update " ++ msg)
	                       let nick = (splitOn " " msg) !! 2
			       modifyMVar_ (appNickList appState) (return . M.insert nick msg)
			       return conState
        Just "$Quit" -> do
	                       putStrLn ("Nickname left " ++ msg)
	                       let nick = (splitOn " " msg) !! 2
			       modifyMVar_ (appNickList appState) (return . M.delete nick)
			       return conState
        Just "$NickList" -> do
	                       putStrLn ("Nicklist: " ++ msg)
                               let nicklist = filter (/="") (splitOn "$$" (tail $ dropWhile (/=' ') msg))
			       let genGetINFOCmd nick = "$GetINFO " ++ nick ++ " " ++ (configNick $ appConfig appState) ++ "|"
                               withMVar (appHubHandle appState) $ \hubHandle -> do
			           hPutStr hubHandle $ concat $ map genGetINFOCmd nicklist
			       return conState
        Just "$Chat" -> do
	                       putStrLn ("Chat: " ++ msg)
			       sendEvent appState (EChatMsg msg)
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
			       sendEvent appState (EChatMsg msg)
			       return conState
        _               -> do
	                       putStrLn "Unkown Command:"
	                       putStrLn msg
			       return conState

dibadu :: Nick -> String -> (M.Map Nick String) -> IO (M.Map Nick String)
dibadu nick msg old = return $ M.insert nick msg old
