--- |
--- | This module contains the DC-stuff for connections to the hub
--- |
--- Copyright   : (c) Florian Richter 2011
--- License     : GPL
---

module DCToHub where

import System.IO
import Control.Concurrent
import Control.Monad
import System.Log.Logger
import Control.Exception.Base
import Data.List.Split
import qualified Data.Map as M

import DCCommon
import DCToClient
import FixedQueue
import FixedQueueTypes
import Config

startupHub :: AppState -> Handle -> IO ()
startupHub appState h = putMVar (appHubHandle appState) h -- save handle for syncronisation

handleHub :: AppState -> Handle -> ConnectionState -> String -> IO ConnectionState
handleHub appState h conState msg = do
    case getCmd msg of
        Just "$Lock"    -> do
                               withMVar (appHubHandle appState) $ \hubHandle -> do
	                           hPutStr hubHandle "$Key ........A .....0.0. 0. 0. 0. 0. 0.|"
	                           hPutStr hubHandle ("$ValidateNick " ++ (configNick $ appConfig appState) ++ "|")
			           hFlush hubHandle
			       return conState
        Just "$HubName" -> do
	                       putStrLn ("Hubname " ++ msg)
			       return conState
        Just "$Hello"   -> do
	                       let hellonick = (splitOn " " msg) !! 1
			       let mynick = (configNick $ appConfig appState)
			       if hellonick == mynick
                                   then do
                                       withMVar (appHubHandle appState) $ \hubHandle -> do
                                           hPutStr hubHandle "$Version 1,0091|"
                                           hPutStr hubHandle "$GetNickList|"
                                           -- http://www.teamfair.info/wiki/index.php?title=$MyINFO
                                           hPutStr hubHandle (getMyINFOStr appState)
                                           hFlush hubHandle
                                       return conState
                                   else return conState
        Just "$MyINFO" -> do
	                       putStrLn ("Nickname update " ++ msg)
	                       let nick = (splitOn " " msg) !! 2
	                       let info = drop 13 msg
			       modifyMVar_ (appNickList appState) (return . M.insert (filesystemSafe nick) (nick, info))
			       return conState
        Just "$Quit" -> do
	                       putStrLn ("Nickname left " ++ msg)
	                       let nick = (splitOn " " msg) !! 1
			       modifyMVar_ (appNickList appState) (return . M.delete (filesystemSafe nick))
			       return conState
        Just "$NickList" -> do
			       let mynick = (configNick $ appConfig appState)
                               let nicklist = filter (/=mynick) $ filter (/="") (splitOn "$$" (tail $ dropWhile (/=' ') msg))
			       let genGetINFOCmd nick = "$GetINFO " ++ nick ++ " " ++ (configNick $ appConfig appState) ++ "|"
                               withMVar (appHubHandle appState) $ \hubHandle -> do
			           hPutStr hubHandle $ concat $ map genGetINFOCmd nicklist
			           hFlush hubHandle
			       return conState
        Just "$Chat" -> do
	                       putStrLn ("Chat: " ++ msg)
			       putFixedQueue (appChatMsgs appState) msg
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
			       putFixedQueue (appChatMsgs appState) msg
			       return conState
        _               -> do
	                       putStrLn "Unkown Command:"
	                       putStrLn msg
			       return conState

-- vim: sw=4 expandtab
