module DCToClient where

import System.IO
import Control.Concurrent
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import Data.List
import Data.List.Split
import Data.Char (toLower)
import Control.Monad
import Control.Concurrent.STM
import System.Random (randomRIO)
import Config
import Filemgmt
import Filelist
import DCCommon

-- | convert string to lower case (didn't find a library function)
toLowerCase :: String -> String
toLowerCase [] = []
toLowerCase (x:xs) = (toLower x) : xs

nextToDownload :: AppState -> Nick -> IO (Maybe String)
nextToDownload appState nick = do
    jobs <- atomically $ readTVar (appJobs appState)
    putStrLn (show jobs)
    case M.lookup nick jobs of
        Just (x:xs) -> return (Just x)
        _           -> return Nothing

-- | connection startup handler, send nick
startupClient :: AppState -> Handle -> IO ()
startupClient appState h = do
    putStrLn "startup client"
    sendCmd h "MyNick" (configNick $ appConfig appState)
    sendCmd h "Lock" "EXTENDEDPROTOCOLABCABCABCABCABCABC Pk=HASKELLDC00.668ABCABC"
    hFlush h

-- | connection message handler
handleClient :: AppState -> Handle -> ConnectionState -> String -> IO ConnectionState
handleClient appState h conState msg = do
    case getCmd msg of
        Just "$MyNick"    -> do
                                -- TODO: check if known
                                let nick = tail $ dropWhile (/=' ') msg
	                        putStrLn ("Connection from: " ++ nick)
                                next <- nextToDownload appState nick
                                case next of
                                    Just file -> return (ToClient (Just nick) Download)
                                    Nothing   -> return (ToClient (Just nick) DontKnow)
        Just "$Lock"    -> do
	                       putStrLn "Lock"
                               sendCmd h "Supports" "MiniSlots XmlBZList ADCGet TTHF"
                               let ToClient nick state = conState
                               case state of
                                   Download -> sendCmd h "Direction" "Download 42"
                                   _        -> sendCmd h "Direction" "Upload 42"
                               sendCmd h "Key" "........A .....0.0. 0. 0. 0. 0. 0."
			       return conState
        Just "$Key"      -> do
                               -- who cares for keys...
			       return conState
        Just "$Quit"      -> do
                               -- nice you tell me, just disconnect!
			       return conState
        Just "$Supports"    -> do
	                       putStrLn "Supports"
                               -- TODO some checks would be nice
                               return conState
        Just "$Direction"   -> do
	                       putStrLn "Direction"
                               let direction = toLowerCase $ takeWhile (/=' ') $ tail $ dropWhile (/=' ') msg
                               putStrLn (show direction)
                               let ToClient (Just nick) state = conState
                               case state of
                                   Download -> putStrLn "state download"
                                   _        -> putStrLn "state upload"
                               case state of
                                   -- I don't like random number battles :)
                                   Download -> if direction /= "upload" then hClose h else return ()
                                   _        -> if direction /= "download" then hClose h else return ()
                                   -- wtf, what does he want?
                               next <- nextToDownload appState nick
                               case next of
                                   Just file -> sendCmd h "ADCGET" ("file " ++ file ++ " 0 -1")
                                   _         -> return ()
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
                                       sendCmd h "FileLength" (show n)
                                       return (ToClient Nothing (Upload filename offset))
                                   Nothing -> do
                                       putStrLn ("File not Found: " ++ filename)
                                       sendCmd h "Error" "File not Available"
                                       return (ToClient Nothing DontKnow)
        Just "$Send"   -> case conState of
	                  ToClient _ (Upload filename offset) -> do
	                       putStrLn "Send raw data"
                               content <- getFileContent appState filename offset
                               case content of
                                   Just c -> do
	                               L.hPut h c
			               hFlush h
			               --hClose h
                                       putStrLn "written"
			               return (ToClient Nothing DontKnow)
                                   Nothing -> do
                                       sendCmd h "Error" "File not found"
			               return (ToClient Nothing DontKnow)
	                  ToClient _ _ -> do
	                       putStrLn "Send without get"
	                       putStrLn msg
                               sendCmd h "Error" "no send before get"
			       return conState
        Just "$ADCGET"   -> do
	                       putStrLn "ADCGet"
	                       let msg_split = splitOn " " msg
	                       putStrLn (show msg_split)
                               if ((length msg_split) /= 5) || ((msg_split !! 1) /= "file")
                                   then do
                                       sendCmd h "Error" "invalid parameters to ADCGet"
                                       hClose h
			               return (ToClient Nothing DontKnow)
                                   else do
                                       let filename = msg_split !! 2
	                               let fileOffset = msg_split !! 3
	                               let fileBufSize = msg_split !! 4
			               filelength <- getFileSize appState filename
                                       case filelength of
                                           Just n -> do
                                               putStrLn ("Filename: " ++ filename)
                                               putStrLn ("Filesize: " ++ (show n))
                                               sendCmd h "ADCSND" ("file " ++ filename ++ " 0 " ++ (show n))
                                               Just content <- getFileContent appState filename (read fileOffset)
	                                       L.hPut h content
			                       hFlush h
			                       --hClose h
			                       return (ToClient Nothing DontKnow)
                                           Nothing -> do
                                               putStrLn ("File not Found: " ++ filename)
                                               sendCmd h "Error" "File not Available"
			                       return (ToClient Nothing DontKnow)
        Just "$ADCSND"   -> do
	                       putStrLn "ADCSND"
	                       let msg_split = splitOn " " msg
	                       putStrLn (show msg_split)
                               if ((length msg_split) /= 5) || ((msg_split !! 1) /= "file")
                                   then do
                                       sendCmd h "Error" "invalid parameters to ADCSND"
                                       hClose h
			               return (ToClient Nothing DontKnow)
                                   else do
                                       let filename = msg_split !! 2
	                               -- let fileOffset = read (msg_split !! 3)
	                               let fileBufSize = read (msg_split !! 4)
                                       let (ToClient (Just nick) _) = conState
                                       if filename == dcFilelist
                                           then return (ToClient Nothing (DownloadJob fileBufSize (filelistHandler nick)))
                                           else return (ToClient Nothing (DownloadJob fileBufSize downloadHandler))
        Nothing         -> do
	                       putStrLn "No Command:"
	                       putStrLn msg
			       return conState
        _                -> do
	                       putStrLn "Unkown command:"
	                       putStrLn msg
			       return conState
    where
        downloadHandler :: Handle -> L.ByteString -> IO Bool
        downloadHandler handle file = do
            L.writeFile "test.bla" file
            return True

        filelistHandler :: Nick -> Handle -> L.ByteString -> IO Bool
        filelistHandler nick handle file = do
            atomically $ do
                    jobs <- readTVar (appJobs appState)
                    writeTVar (appJobs appState) (M.update (deleteCompletely dcFilelist) nick jobs)
                    filelists <- readTVar (appFilelists appState)
                    writeTVar (appFilelists appState) (M.insert nick (B.concat $! L.toChunks file) filelists)
            --jobs <- atomically $ readTVar (appFilelists appState)
            --putStrLn (show jobs)
            putStrLn "download complete (handler)"
            return True

        deleteCompletely :: (Eq a) => a -> [a] -> Maybe [a]
        deleteCompletely item list = case delete item list of
                                      [] -> Nothing
                                      l  -> Just l

-- | synchronous download of filelist
downloadFilelist :: AppState -> Nick -> IO B.ByteString
downloadFilelist appState nick = do
    downloadFile appState nick dcFilelist
    f<-atomically $ do
        filelists <- readTVar (appFilelists appState)
        when (M.notMember nick filelists) retry
        let Just filelist = M.lookup nick filelists
        writeTVar (appFilelists appState) (M.delete nick filelists)
        return filelist
    return f

-- | asynchronous download start, should be called from different thread
downloadFile :: AppState -> Nick -> String -> IO ()
downloadFile appState nick file = do
    putStrLn "insert download"
    putStrLn (nick ++ " " ++ (configMyIp $ appConfig appState) ++ ":" ++ (configMyPort $ appConfig appState))
    atomically $ do
        jobs <- readTVar (appJobs appState)
        writeTVar (appJobs appState) (M.insertWith' (++) nick [file] jobs)
    withMVar (appHubHandle appState) $ \hubHandle -> do
        sendCmd hubHandle "ConnectToMe" (nick ++ " " ++ (configMyIp $ appConfig appState) ++ ":" ++ (configMyPort $ appConfig appState))

-- vim: sw=4 expandtab
