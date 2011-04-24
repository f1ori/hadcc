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

getNickList :: AppState -> IO (M.Map Nick (Nick, String))
getNickList appState = readMVar (appNickList appState)

myshareContentHandler :: TreeNode -> FsContent
myshareContentHandler (FileNode _ path _ _ _) = FsFile (openF path)
    where
        openF path = do
            h <- openFile path ReadMode
            return (readF h, hClose h)
        readF h size offset = do
            hSeek h AbsoluteSeek (fromInteger offset)
            B.hGet h (fromInteger size)

treeNodeFsHandler :: (TreeNode -> FsContent) -> TreeNode -> UserGroupID -> FileInfoHandler
treeNodeFsHandler contentHandler (DirNode name _ _) ugid "" = do
    return $ Just (getStatDir ugid, FsDir [name])
treeNodeFsHandler contentHandler tree ugid path = do
    let searchpath = drop 1 path -- drop leading directory slash
    case searchNode searchpath tree of
        Just node@(FileNode _ _ size _ _) -> return $ Just (getStatFileR ugid size, contentHandler node)
        Just (DirNode _ _ children)       -> return $ Just (getStatDir ugid, FsDir (map nodeToName children))
        Nothing                           -> return Nothing

dcFileInfo :: AppState -> FileInfoHandler
dcFileInfo appState path = do
    logMsg appState ("file info: " ++ path)
    ugid <- getUserGroupID
    case path of

        "/" -> return $ Just (getStatDir ugid, FsDir ["nicks", "status", "myshare"])

        "/nicks" -> do
	            nicklist <- getNickList appState
		    return $ Just (getStatDir ugid, FsDir (M.keys nicklist))

        "/status" -> return $ Just (getStatFileR ugid 1024, FsFile (textHandler "testing"))

        _ | (take 7 path) == "/nicks/" -> do
	      nicklist <- getNickList appState
	      let (nick, subpath) = break (=='/') (drop 7 path)
	      if nick `M.member` nicklist
	          then do
		      case subpath of
		          ""       -> return $ Just (getStatDir ugid, FsDir ["share", "info", "name"])
			  "/share" -> return $ Just (getStatDir ugid, FsDir [])
			  "/name"  -> do
                              let Just (completeName, _) = M.lookup nick nicklist
                              return $ Just (getStatFileR ugid 1024, FsFile (textHandler completeName))
			  "/info"  -> do
                              let Just (_, nickinfo) = M.lookup nick nicklist
                              return $ Just (getStatFileR ugid 1024, FsFile (textHandler nickinfo))
			  _        -> return Nothing
		      
		  else return Nothing

          | (take 8 path) == "/myshare" -> do
	      let subpath = drop 8 path
              logMsg appState subpath
              myShare <- readMVar $ appFileTree appState
              treeNodeFsHandler myshareContentHandler myShare ugid subpath

	  | otherwise -> return Nothing

textHandler :: String -> IO (ReadFunc, CloseFunc)
textHandler text = return (readText, return ())
    where
        readText size offset = return $ (B.take (fromIntegral size) $ B.drop (fromIntegral offset) (B.pack text))

    
-- vim: sw=4 expandtab
