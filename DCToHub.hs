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
import Network.Socket
import System.Log.Logger
import Control.Exception.Base
import Data.List.Split
import qualified Data.Map as M
import Text.Printf
import qualified Data.Text as T

import DCCommon
import DCToClient
import FixedQueue
import FixedQueueTypes
import Config
import Search
import Udp

createSearchSocket :: IO Socket
createSearchSocket = fst `liftM` initUdpServer

startupHub :: AppState -> Handle -> IO ()
startupHub appState h = putMVar (appHubHandle appState) h -- save handle for syncronisation

handleHub :: AppState -> Socket -> Handle -> ConnectionState -> String -> IO ConnectionState
handleHub appState searchSocket h conState msg = do
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
        Just "$Search" -> do
                               let (response, search) = dcToSearch $ T.pack msg
                               let hub = (configHubIp $ appConfig appState) ++ ":" ++ (configHubPort $ appConfig appState)
                               putStrLn $ show search
                               if (take 4 response) == "Hub:"
                                   then do
                                       -- passive search
                                       let sendnick = drop 4 response
                                       putStrLn sendnick
                                       results <- searchInDb appState search
                                       mapM_ putStrLn $ map (searchResultToDc hub $ Just sendnick) results
                                       withMVar (appHubHandle appState) $ \hubHandle -> do
                                           mapM_ (hPutStr hubHandle) $ map (searchResultToDc hub $ Just sendnick) results
			                   hFlush hubHandle
                                   else do
                                       let ip = takeWhile (/=':') response
                                       let port = read $ tail $ dropWhile (/=':') response
                                       results <- searchInDb appState search
                                       mapM_ putStrLn $ map (searchResultToDc hub Nothing) results
                                       mapM_ (sendUdp searchSocket ip port) $ map (searchResultToDc hub Nothing) results
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

searchDC :: AppState -> PortNumber -> Search -> IO ()
searchDC appState udpPort search = do
    let searchstring = searchToDC search
    withMVar (appHubHandle appState) $ \hubHandle -> do
        sendCmd hubHandle "Search" ((configMyIp $ appConfig appState) ++ ":" ++ (show udpPort) ++ " " ++ searchstring)

sendChatMsg :: AppState -> T.Text -> IO ()
sendChatMsg appState msg = do
    let nick = configNick $ appConfig appState
    let cmd = printf "<%s> %s|" nick (T.unpack $ T.strip $ chatEscape msg)
    --putStrLn ("CHAAAAAAAT: " ++ cmd)
    withMVar (appHubHandle appState) $ \hubHandle -> do
        hPutStr hubHandle cmd
        hFlush hubHandle
    where
        chatEscape str = T.replace (T.pack "$") (T.pack "&#36;") $ T.replace (T.pack "|") (T.pack "&#124;") str

-- vim: sw=4 expandtab
