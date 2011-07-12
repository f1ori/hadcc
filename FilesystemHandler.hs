--- |
--- | This module contains functions generating content and stats for the fuse filesystem
--- |
--- Copyright   : (c) Florian Richter 2011
--- License     : GPL
---

module FilesystemHandler (
      dcFileInfo
    ) where

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
import Data.Maybe

import Config
import Filesystem
import FilelistTypes
import Filelist
import FilelistCache
import FixedQueue
import DCToClient
import DCToHub
import Udp
import Search
import Filemgmt
import TTH

peer_timeout = 6000000

-- | small script how to use the search
searchScript = "#!/bin/bash\n\
               \# Small example how to use the search\n\n\
               \f=`dirname \"$0\"`\n\
               \exec 3<>$f/search; echo $1>&3; trap 'exec 3>&-' INT; cat <&3\n"

-- | small script how to use the reload function
reloadScript = "#!/bin/bash\n\
               \# Small example how to use reloadshare\n\n\
               \f=`dirname \"$0\"`\n\
               \echo bla >>$f/reloadshare\n"

-- | file handle for dc shares
shareContentHandler :: AppState -> Nick -> TreeNode -> FsContent
shareContentHandler appState nick (FileNode _ _ _ _ (Just hash)) = FsFile openF openInfo
    where
        openInfo = FuseOpenInfo {
                     fsDirectIo = False
                   , fsKeepCache = False
                   , fsNonseekable = True
                   }
        openF ReadOnly = do
            finishedMVar <- newEmptyMVar
            contentMVar <- newEmptyMVar
            downloadFile appState (downloadHandler finishedMVar contentMVar) nick ("TTH/" ++ (T.unpack hash))
            result <- timeout peer_timeout $ readMVar contentMVar 
            case result of
                Just _ -> return $ Right (readF contentMVar, Nothing, closeF finishedMVar)
                Nothing -> return $ Left eIO
        openF _       = return $ Left eACCES

        readF :: MVar L.ByteString -> ReadFunc
        readF contentMVAr size offset = checkFuseInterruptDefault B.empty $ do
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
            content <- takeMVar contentMVar
            --putStrLn ("encounter: " ++ (show $ L.length content))
            if L.null content
                then return ()
                else do
                    putStrLn "not emptyyyyyyyyyyyy"
                    ioError (userError "file not fully consumed, cut connection")


-- | file content handler, providing data from local share
myshareContentHandler :: TreeNode -> FsContent
myshareContentHandler (FileNode _ path _ _ _) = FsFile (openF $ T.unpack path) openInfo
    where
        openInfo = FuseOpenInfo {
                     fsDirectIo = False
                   , fsKeepCache = False
                   , fsNonseekable = False
                   }
        openF path ReadOnly = do
            h <- openFile path ReadMode
            return $ Right (readF h, Nothing, hClose h)
        openF path _        = return $ Left eACCES
        readF h size offset = do
            hSeek h AbsoluteSeek (fromInteger offset)
            B.hGet h (fromInteger size)

-- | file content handler, providing simple text file
textContentHandler :: String -> FsContent
textContentHandler text = FsFile openF openInfo
    where
        openInfo = FuseOpenInfo {
                     fsDirectIo = True
                   , fsKeepCache = False
                   , fsNonseekable = False
                   }
        openF ReadOnly = return $ Right(readText, Nothing, return ())
        openF _        = return $ Left eACCES
        readText size offset = return $ (B.take (fromIntegral size) $ B.drop (fromIntegral offset) (B.pack text))

-- | interrupt workFunc every interval microseconds and check
-- | value of checkFunc, if True return Nothing
-- | otherwise return result of workFunc
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


-- | function to check for fuser interrupts
checkFuseInterrupt :: IO a -> IO (Maybe a)
checkFuseInterrupt = checkFuncOnInterval 1000000 isFuseInterrupted

-- | function to check for fuse interrupts, takes default result
checkFuseInterruptDefault :: a -> IO a -> IO a
checkFuseInterruptDefault def action = fromMaybe def `liftM` checkFuseInterrupt action


-- | search file handler
searchContentHandler :: AppState -> FsContent
searchContentHandler appState = FsFile openF openInfo
    where
        openF ReadWrite = do
            (sock, port) <- initUdpServer
            return $ Right (readF sock, Just $ writeF appState port, sClose sock)
        openF _         = return $ Left eACCES
        openInfo = FuseOpenInfo {
                     fsDirectIo = True
                   , fsKeepCache = False
                   , fsNonseekable = True
                   }
        readF sock size offset = do
            result <- checkFuseInterrupt (recv sock (fromInteger size))
            case result of
                Just content -> return $ B.pack $ printSearchResult $ dcToSearchResult $ E.decodeUtf8 content
                Nothing      -> return $ B.empty
        writeF appState port content offset = do
            searchDC appState port (simpleSearch $ E.decodeUtf8 content)
            return $ fromIntegral $ B.length content

-- | content handler for chat file
chatContentHandler :: AppState -> FsContent
chatContentHandler appState = FsFile openF openInfo
    where
        openF mode = do
            queueIndex <- newMVar 0
            return $ Right (readF appState queueIndex, Just $ writeF appState, return ())
        openInfo = FuseOpenInfo {
                     fsDirectIo = True
                   , fsKeepCache = False
                   , fsNonseekable = True
                   }
        readF appState queueIndex size offset =
            checkFuseInterruptDefault B.empty $
                modifyMVar queueIndex $ \index -> do
                    putStrLn $ show index
                    chat <- atomically $ readTVar (appChatMsgs appState)
                    putStrLn $ show chat
                    (newIndex, msg) <- takeOneFixedQueue (appChatMsgs appState) index
                    return (newIndex, B.pack (msg ++ "\n"))
        writeF appState content offset = do
            sendChatMsg appState (E.decodeUtf8 content)
            return $ fromIntegral $ B.length content

-- | reload local share on open
reloadShareContentHandler :: AppState -> FsContent
reloadShareContentHandler appState = FsFile openF openInfo
    where
        openF WriteOnly = do
            reloadOwnShare appState
            hashFileList appState
            return $ Right (readF, Just writeF, return ())
        openF _         = return $ Left eACCES

            -- do the chit
        openInfo = FuseOpenInfo {
                     fsDirectIo = True
                   , fsKeepCache = False
                   , fsNonseekable = True
                   }
        readF size offset = return $ B.pack ""
        writeF content offset = return $ fromIntegral $ B.length content


-- | filesystem handler providing directory structure of TreeNode
treeNodeFsHandler :: (TreeNode -> FsContent) -> TreeNode -> UserGroupID -> FileInfoHandler
treeNodeFsHandler contentHandler (DirNode name _ _) ugid "" = do
    return $! Just (getStatDir ugid, FsDir (return [T.unpack name]))
treeNodeFsHandler contentHandler tree ugid path = do
    let searchpath = drop 1 path -- drop leading directory slash
    case searchNode (T.pack searchpath) tree of
        Just node@(FileNode _ _ size _ _) -> return $! Just (getStatFileR ugid size, contentHandler node)
        Just (DirNode _ _ children)       -> return $! Just (getStatDir ugid, FsDir (return $ map (T.unpack.nodeToName) children))
        Nothing                           -> return Nothing


-- | filesystem handler providing directory structure of TreeNode-List
treeNodeFsHandlerL :: (TreeNode -> FsContent) -> [TreeNode] -> UserGroupID -> FileInfoHandler
treeNodeFsHandlerL contentHandler tree ugid path = do
    case searchNodeL (T.pack path) tree of
        Just node@(FileNode _ _ size _ _) -> return $! Just (getStatFileR ugid size, contentHandler node)
        Just (DirNode _ _ children)       -> return $! Just (getStatDir ugid, FsDir (return $ map (T.unpack.nodeToName) children))
        Nothing                           -> return Nothing


-- | filesystem handler providing base structure
dcFileInfo :: AppState -> FileInfoHandler
dcFileInfo appState path = do
    putStrLn ("file info: " ++ path)
    ugid <- getUserGroupID
    case path of

        "/" -> return $ Just (getStatDir ugid, FsDir
               (return ["nicks", "status", "myshare", "search", "dosearch", "chat", "reloadshare", "doreloadshare"]))

        "/nicks" -> do
		    return $ Just (getStatDir ugid, FsDir (M.keys `liftM` readMVar (appNickList appState)))

        "/status" -> return $ Just (getStatFileR ugid 0, (textContentHandler "testing"))

        "/chat" -> return $ Just (getStatFileRW ugid 0, (chatContentHandler appState))

        "/search" -> return $ Just (getStatFileRW ugid 0, (searchContentHandler appState))

        "/dosearch" -> return $ Just (getStatFileRX ugid 0, (textContentHandler searchScript))

        "/reloadshare" -> return $ Just (getStatFileRW ugid 0, (reloadShareContentHandler appState))

        "/doreloadshare" -> return $ Just (getStatFileRX ugid 0, (textContentHandler reloadScript))

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
                                                              treeNodeFsHandlerL (shareContentHandler appState completeNick) share ugid sharepath
                            | otherwise                    -> return Nothing
		      
		  else return Nothing

          | (take 8 path) == "/myshare" -> do
	      let subpath = drop 8 path
              IndexedFileTree myShare htable <- readMVar $ appFileTree appState
              treeNodeFsHandler myshareContentHandler myShare ugid subpath

	  | otherwise -> return Nothing

    
-- vim: sw=4 expandtab
