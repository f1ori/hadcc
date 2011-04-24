module FilesystemHandler where

import Filesystem
import Config
import Control.Concurrent.MVar
import Control.Monad
import System.Log.Logger
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B

getNickList :: AppState -> IO (M.Map Nick (Nick, String))
getNickList appState = readMVar (appNickList appState)

nix :: IO ()
nix = return ()

dcFileInfo :: AppState -> FileInfoHandler
dcFileInfo appState path = do
    logMsg appState ("file info: " ++ path)
    ugid <- getUserGroupID
    case path of

        "/" -> return $ Just (getStatDir ugid, FsDir ["nicks", "status"])

        "/nicks" -> do
	            nicklist <- getNickList appState
		    return $ Just (getStatDir ugid, FsDir (M.keys nicklist))

        "/status" -> return $ Just (getStatFileR ugid 1024, FsFile nix (textHandler "testing") nix)

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
                              return $ Just (getStatFileR ugid 1024, FsFile nix (textHandler completeName) nix)
			  "/info"  -> do
                              let Just (_, nickinfo) = M.lookup nick nicklist
                              return $ Just (getStatFileR ugid 1024, FsFile nix (textHandler nickinfo) nix)
			  _        -> return Nothing
		      
		  else return Nothing

	  | otherwise -> return Nothing

textHandler :: String -> Integer -> Integer -> IO B.ByteString
textHandler text size offset =
        return $ (B.take (fromIntegral size) $ B.drop (fromIntegral offset) (B.pack text))

    
-- vim: sw=4 expandtab
