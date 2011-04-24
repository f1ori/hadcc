module FilesystemHandler where

import System.IO
import Control.Concurrent.MVar
import Control.Monad
import System.Log.Logger
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B

import Config
import Filesystem
import FilelistTypes
import Filelist
import FilelistCache

shareContentHandler :: Nick -> TreeNode -> FsContent
shareContentHandler nick (FileNode _ path _ _ _) = FsDir []

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
    return $ Just (getStatDir ugid, FsDir [name])
treeNodeFsHandler contentHandler tree ugid path = do
    let searchpath = drop 1 path -- drop leading directory slash
    case searchNode searchpath tree of
        Just node@(FileNode _ _ size _ _) -> return $ Just (getStatFileR ugid size, contentHandler node)
        Just (DirNode _ _ children)       -> return $ Just (getStatDir ugid, FsDir (map nodeToName children))
        Nothing                           -> return Nothing

-- | filesystem handler providing base structure
dcFileInfo :: AppState -> FileInfoHandler
dcFileInfo appState path = do
    putStrLn ("file info: " ++ path)
    ugid <- getUserGroupID
    case path of

        "/" -> return $ Just (getStatDir ugid, FsDir ["nicks", "status", "myshare"])

        "/nicks" -> do
	            nicklist <- readMVar (appNickList appState)
		    return $ Just (getStatDir ugid, FsDir (M.keys nicklist))

        "/status" -> return $ Just (getStatFileR ugid 1024, FsFile (textContentHandler "testing"))

        _ | (take 7 path) == "/nicks/" -> do
	      nicklist <- readMVar (appNickList appState)
	      let (nick, subpath) = break (=='/') (drop 7 path)
	      if nick `M.member` nicklist
	          then do
		      case subpath of
		          ""       -> return $ Just (getStatDir ugid, FsDir ["share", "info", "name"])
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
                                                              treeNodeFsHandler (shareContentHandler completeNick) share ugid sharepath
                            | otherwise                    -> return Nothing
		      
		  else return Nothing

          | (take 8 path) == "/myshare" -> do
	      let subpath = drop 8 path
              myShare <- readMVar $ appFileTree appState
              treeNodeFsHandler myshareContentHandler myShare ugid subpath

	  | otherwise -> return Nothing

    
-- vim: sw=4 expandtab
