module FilesystemHandler where

import Filesystem

dcFileInfo :: FileInfoHandler
dcFileInfo path = do
    ugid <- getUserGroupID
    case path of
        "/" -> return $ Just (getStatDir ugid, ["nicks", "status"])
        "/nicks" -> return $ Just (getStatDir ugid, [])
        "/status" -> return $ Just (getStatFileR ugid 1024, [])
        _ -> return Nothing

