module FilesystemHandler where

import Filesystem
import Config
import Control.Concurrent.MVar
import Control.Monad
import System.Log.Logger
import qualified Data.Map as M

getNickList :: AppState -> IO [Nick]
getNickList appState = M.keys `liftM` readMVar (appNickList appState)


dcFileInfo :: AppState -> FileInfoHandler
dcFileInfo appState path = do
    logMsg appState ("file info: " ++ path)
    ugid <- getUserGroupID
    case path of

        "/" -> return $ Just (getStatDir ugid, ["nicks", "status"])

        "/nicks" -> do
	            nicklist <- getNickList appState
		    return $ Just (getStatDir ugid, nicklist)

        "/status" -> return $ Just (getStatFileR ugid 1024, [])

        _ | (take 7 path) == "/nicks/" -> do
	      nicklist <- getNickList appState
	      let (nick, subpath) = break (=='/') (drop 7 path)
	      if nick `elem` nicklist
	          then do
		      case subpath of
		          ""       -> return $ Just (getStatDir ugid, ["share", "info", "name"])
			  "/share" -> return $ Just (getStatDir ugid, [])
			  "/info"  -> return $ Just (getStatFileR ugid 1024, [])
			  "/name"  -> return $ Just (getStatFileR ugid 1024, [])
			  _        -> return Nothing
		      
		  else return Nothing

	  | otherwise -> return Nothing

