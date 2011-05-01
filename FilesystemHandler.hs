--- |
--- | This module contains functions generating content and stats for the fuse filesystem
--- |
--- Copyright   : (c) Florian Richter 2011
--- License     : GPL
---

module FilesystemHandler where

import System.IO
import System.Fuse
import System.Timeout
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad
import System.Log.Logger
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Data.Text.Encoding as E
import Data.Tuple

import Config
import Filesystem
import FilelistTypes
import Filelist
import FilelistCache
import FixedQueue
import DCToClient
import Udp
import Search

peer_timeout = 2000000

-- | small script how to use the search
searchScript = "#!/bin/bash\n\
               \# Small example how to use the search\n\n\
               \f=`dirname \"$0\"`\n\
               \exec 3<>$f/search; echo $1>&3; trap 'exec 3>&-' INT; cat <&3\n"

-- | file handle for dc shares
shareContentHandler :: AppState -> Nick -> TreeNode -> FsContent
shareContentHandler appState nick (FileNode _ _ _ _ (Just hash)) = FsFile openF openInfo
    where
        openInfo = FuseOpenInfo {
                     fsDirectIo = False
                   , fsKeepCache = False
                   , fsNonseekable = True
                   }
        openF = do
            finishedMVar <- newEmptyMVar
            contentMVar <- newEmptyMVar
            downloadFile appState (downloadHandler finishedMVar contentMVar) nick ("TTH/" ++ hash)
            result <- timeout peer_timeout $ readMVar contentMVar 
            case result of
                Just _ -> return (readF contentMVar, Nothing, closeF finishedMVar)
                Nothing -> error "peer does not connect"

        readF :: MVar L.ByteString -> ReadFunc
        readF contentMVAr size offset = do
            lazyResult <- modifyMVar contentMVAr (return . swap . L.splitAt (fromInteger size))
            return (B.concat $ L.toChunks lazyResult)

        closeF finishedMVar = takeMVar finishedMVar

        downloadHandler finishedMVar contentMVar handle content = do
            putStrLn "download handler active"
            putMVar finishedMVar ()
            putMVar contentMVar content
            -- wait for finished MVar to be empty
            putMVar finishedMVar ()
            -- prevent further reads
            void $ takeMVar contentMVar
            return True


-- | file content handler, providing data from local share
myshareContentHandler :: TreeNode -> FsContent
myshareContentHandler (FileNode _ path _ _ _) = FsFile (openF path) openInfo
    where
        openInfo = FuseOpenInfo {
                     fsDirectIo = False
                   , fsKeepCache = False
                   , fsNonseekable = False
                   }
        openF path = do
            h <- openFile path ReadMode
            return (readF h, Nothing, hClose h)
        readF h size offset = do
            hSeek h AbsoluteSeek (fromInteger offset)
            B.hGet h (fromInteger size)

-- | file content handler, providing simple text file
textContentHandler :: String -> FsContent
textContentHandler text = FsFile (return (readText, Nothing, return ())) openInfo
    where
        openInfo = FuseOpenInfo {
                     fsDirectIo = True
                   , fsKeepCache = False
                   , fsNonseekable = False
                   }
        readText size offset = return $ (B.take (fromIntegral size) $ B.drop (fromIntegral offset) (B.pack text))

-- | check function in interval (in microseconds)
checkFuncOnInterval :: Int -> IO Bool -> IO a -> IO ( Maybe a)
checkFuncOnInterval interval checkFunc workFunc = do
    result <- timeout interval workFunc
    case result of
        Just value -> return $ Just value
        Nothing    -> do
            check <- checkFunc
            if check
                then return Nothing
                else checkFuncOnInterval interval checkFunc workFunc

-- | search file handler
searchContentHandler :: AppState -> FsContent
searchContentHandler appState = FsFile openF openInfo
    where
        openF = do
            (sock, port) <- initUdpServer
            return (readF sock, Just $ writeF appState port, sClose sock)
        openInfo = FuseOpenInfo {
                     fsDirectIo = True
                   , fsKeepCache = False
                   , fsNonseekable = True
                   }
        readF sock size offset = do
            result <- checkFuncOnInterval 1000000 isFuseInterrupted (recv sock (fromInteger size))
            case result of
                Just content -> return $ B.pack $ printSearchResult $ dcToSearchResult $ E.decodeUtf8 content
                Nothing      -> return $ B.empty
        writeF appState port content offset = do
            searchDC appState port (simpleSearch $ E.decodeUtf8 content)
            return $ fromIntegral $ B.length content

chatContentHandler :: AppState -> FsContent
chatContentHandler appState = FsFile openF openInfo
    where
        openF = do
            queueIndex <- newMVar 0
            return (readF appState queueIndex, Just $ writeF appState, return ())
        openInfo = FuseOpenInfo {
                     fsDirectIo = True
                   , fsKeepCache = False
                   , fsNonseekable = True
                   }
        readF appState queueIndex size offset = do
            result <- checkFuncOnInterval 1000000 isFuseInterrupted $
                modifyMVar queueIndex $ \index -> do
                    putStrLn $ show index
                    chat <- atomically $ readTVar (appChatMsgs appState)
                    putStrLn $ show chat
                    (newIndex, msg) <- takeOneFixedQueue (appChatMsgs appState) index
                    return (newIndex, B.pack (msg ++ "\n"))
            case result of
                Just content -> return content
                Nothing      -> return B.empty
        writeF appState content offset = return 0

-- | filesystem handler providing directory structure of TreeNode
treeNodeFsHandler :: (TreeNode -> FsContent) -> TreeNode -> UserGroupID -> FileInfoHandler
treeNodeFsHandler contentHandler (DirNode name _ _) ugid "" = do
    return $! Just (getStatDir ugid, FsDir (return [name]))
treeNodeFsHandler contentHandler tree ugid path = do
    let searchpath = drop 1 path -- drop leading directory slash
    case searchNode searchpath tree of
        Just node@(FileNode _ _ size _ _) -> return $! Just (getStatFileR ugid size, contentHandler node)
        Just (DirNode _ _ children)       -> return $! Just (getStatDir ugid, FsDir (return $ map nodeToName children))
        Nothing                           -> return Nothing

-- | filesystem handler providing base structure
dcFileInfo :: AppState -> FileInfoHandler
dcFileInfo appState path = do
    putStrLn ("file info: " ++ path)
    ugid <- getUserGroupID
    case path of

        "/" -> return $ Just (getStatDir ugid, FsDir (return ["nicks", "status", "myshare", "search", "dosearch", "chat"]))

        "/nicks" -> do
		    return $ Just (getStatDir ugid, FsDir (M.keys `liftM` readMVar (appNickList appState)))

        "/status" -> return $ Just (getStatFileR ugid 0, (textContentHandler "testing"))

        "/chat" -> return $ Just (getStatFileRW ugid 0, (chatContentHandler appState))

        "/search" -> return $ Just (getStatFileRW ugid 0, (searchContentHandler appState))

        "/dosearch" -> return $ Just (getStatFileRX ugid 0, (textContentHandler searchScript))

        _ | (take 7 path) == "/nicks/" -> do
	      nicklist <- readMVar (appNickList appState)
	      let (nick, subpath) = break (=='/') (drop 7 path)
	      if nick `M.member` nicklist
	          then do
		      case subpath of
		          ""       -> return $ Just (getStatDir ugid, FsDir (return ["share", "info", "name"]))
			  "/name"  -> do
                              let Just (completeName, _) = M.lookup nick nicklist
                              return $ Just (getStatFileR ugid 0, (textContentHandler completeName))
			  "/info"  -> do
                              let Just (_, nickinfo) = M.lookup nick nicklist
                              return $ Just (getStatFileR ugid 0, (textContentHandler nickinfo))
			  _ | (take 6 subpath) == "/share" -> do
                                                              let Just (completeNick, _) = M.lookup nick nicklist
                                                              let sharepath = drop 6 subpath
                                                              share <- getFilelistCached appState completeNick
                                                              share `seq` treeNodeFsHandler (shareContentHandler appState completeNick) share ugid sharepath
                            | otherwise                    -> return Nothing
		      
		  else return Nothing

          | (take 8 path) == "/myshare" -> do
	      let subpath = drop 8 path
              myShare <- readMVar $ appFileTree appState
              treeNodeFsHandler myshareContentHandler myShare ugid subpath

	  | otherwise -> return Nothing

    
-- vim: sw=4 expandtab
