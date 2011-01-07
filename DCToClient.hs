module DCToClient where

import System.IO
import Control.Concurrent
import qualified Data.ByteString.Lazy as L
import Data.List.Split
import Config
import Filemgmt
import Filelist
import DCCommon


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
	                        putStrLn msg
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
	                       hPutStr h "$Supports MiniSlots XmlBZList ADCGet TTHF|"
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
                                       sendCmd h "FileLength" (show n)
                                       return (Upload filename offset)
                                   Nothing -> do
                                       putStrLn ("File not Found: " ++ filename)
                                       sendCmd h "Error" "File not Available"
                                       return (Upload filename offset)
        Just "$Send"   -> case conState of
	                  (Upload filename offset) -> do
	                       putStrLn "Send raw data"
                               content <- getFileContent appState filename offset
                               case content of
                                   Just c -> do
	                               L.hPut h c
			               hFlush h
			               hClose h
			               return DontKnow
                                   Nothing -> do
                                       sendCmd h "Error" "File not found"
			               return DontKnow
	                  _ -> do
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
                                       return DontKnow
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
			                       return DontKnow
                                           Nothing -> do
                                               putStrLn ("File not Found: " ++ filename)
                                               sendCmd h "Error" "File not Available"
                                               return DontKnow
        Nothing         -> do
	                       putStrLn "No Command:"
	                       putStrLn msg
			       return conState
        _                -> do
	                       putStrLn "Unkown command:"
	                       putStrLn msg
			       return conState

-- vim: sw=4 expandtab
