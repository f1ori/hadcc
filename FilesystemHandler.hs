--- |
--- | This module contains functions generating content and stats for the fuse filesystem
--- |
--- Copyright   : (c) Florian Richter 2011
--- License     : GPL
---

module FilesystemHandler where

import System.IO
import System.Timeout
import Control.Concurrent.MVar
import Control.Monad
import System.Log.Logger
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Data.Tuple

import Config
import Filesystem
import FilelistTypes
import Filelist
import FilelistCache
import DCToClient

peer_timeout = 2000000

shareContentHandler :: AppState -> Nick -> TreeNode -> FsContent
shareContentHandler appState nick (FileNode _ _ _ _ (Just hash)) = FsFile openF
    where
        openF = do
            finishedMVar <- newEmptyMVar
            contentMVar <- newEmptyMVar
            downloadFile appState (downloadHandler finishedMVar contentMVar) nick ("TTH/" ++ hash)
            result <- timeout peer_timeout $ readMVar contentMVar 
            case result of
                Just _ -> return (readF contentMVar, closeF finishedMVar)
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
myshareContentHandler (FileNode _ path _ _ _) = FsFile (openF path)
    where
        openF path = do
            h <- openFile path ReadMode
            return (readF h, hClose h)
        readF h size offset = do
            hSeek h AbsoluteSeek (fromInteger offset)
            B.hGet h (fromInteger size)

-- | file content handler, providing simple text file
textContentHandler :: String -> IO (ReadFunc, CloseFunc)
textContentHandler text = return (readText, return ())
    where
        readText size offset = return $ (B.take (fromIntegral size) $ B.drop (fromIntegral offset) (B.pack text))

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

        "/" -> return $ Just (getStatDir ugid, FsDir (return ["nicks", "status", "myshare"]))

        "/nicks" -> do
		    return $ Just (getStatDir ugid, FsDir (M.keys `liftM` readMVar (appNickList appState)))

        "/status" -> return $ Just (getStatFileR ugid 1024, FsFile (textContentHandler "testing"))

        _ | (take 7 path) == "/nicks/" -> do
	      nicklist <- readMVar (appNickList appState)
	      let (nick, subpath) = break (=='/') (drop 7 path)
	      if nick `M.member` nicklist
	          then do
		      case subpath of
		          ""       -> return $ Just (getStatDir ugid, FsDir (return ["share", "info", "name"]))
			  "/name"  -> do
                              let Just (completeName, _) = M.lookup nick nicklist
                              return $ Just (getStatFileR ugid 1024, FsFile (textContentHandler completeName))
			  "/info"  -> do
                              let Just (_, nickinfo) = M.lookup nick nicklist
                              return $ Just (getStatFileR ugid 1024, FsFile (textContentHandler nickinfo))
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
