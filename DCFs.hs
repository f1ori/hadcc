import System.Fuse
import System.Posix.Types
import System.Posix.Files
import System.Posix.User
import System.Environment(withArgs)
import System.Log.Logger
import System.Log.Handler.Simple


getInfo :: FilePath -> IO (Maybe (FileStat, [FilePath]))
getInfo path = do
    ugid <- getUserGroupID
    case path of
        "/" -> return $ Just (getStatDir ugid, ["nicks", "status"])
        "/nicks" -> return $ Just (getStatDir ugid, [])
        "/status" -> return $ Just (getStatFileR ugid 0, [])
        _ -> return Nothing


type UserGroupID = (UserID, GroupID)

getUserGroupID :: IO UserGroupID
getUserGroupID = do
   uid <- getRealUserID
   gid <- getRealGroupID
   return (uid, gid)

getStatDir ugid = getStat ugid Directory "rx" 0
getStatFileR ugid size = getStat ugid RegularFile "r" size
getStatFileRW ugid size = getStat ugid RegularFile "rw" size

getStat :: UserGroupID -> EntryType -> String -> FileOffset -> FileStat
getStat (uid, gid) entryType fileModeStr size = FileStat
    { statEntryType = entryType
    , statFileMode = strToFileMode fileModeStr
    , statLinkCount = 5
    , statFileOwner = uid
    , statFileGroup = gid
    , statSpecialDeviceID = 0
    , statFileSize = size
    , statBlocks = 1
    , statAccessTime= 0
    , statModificationTime = 0
    , statStatusChangeTime = 0
    }
   where
       strToFileMode str = foldr1 unionFileModes (map chrToFileMode str)
       chrToFileMode 'r' = foldr1 unionFileModes [ ownerReadMode , groupReadMode , otherReadMode ]
       chrToFileMode 'w' = foldr1 unionFileModes [ ownerWriteMode , groupWriteMode , otherWriteMode ]
       chrToFileMode 'x' = foldr1 unionFileModes [ ownerExecuteMode , groupExecuteMode , otherExecuteMode ]
       chrToFileMode _   = nullFileMode


fsInit :: IO ()
fsInit = do
    errorM rootLoggerName "init"

fsDestroy :: IO ()
fsDestroy = do
    errorM rootLoggerName "destroy"


fsOpenDir :: FilePath -> IO Errno
fsOpenDir path = do
    errorM rootLoggerName ("opendir " ++ path)
    result <- fsReadDir path
    case result of
        Right _ -> return eOK
        Left errno -> return errno


fsReadDir :: FilePath -> IO (Either Errno [(FilePath, FileStat)])
fsReadDir path = do
    errorM rootLoggerName ("readdir " ++ path)
    ugid <- getUserGroupID
    info <- getInfo path
    case info of
        Just (stat, content) -> do
            case statEntryType stat of
                Directory -> do
                               stats <- mapM (getStats path) content
                               return $ Right ([(".", getStatDir ugid), ("..", getStatDir ugid)] ++ (zip content stats))
                _         -> return $ Left eNOTDIR
        Nothing -> return $ Left eNOENT
    where
        getStats path name = do
            let completepath = if (last path) == '/' then path ++ name else path ++ "/" ++ name
            Just (stat, files) <- getInfo completepath
            return stat


fsReleaseDir :: FilePath -> IO Errno
fsReleaseDir path = do
    errorM rootLoggerName ("closedir " ++ path)
    return eOK

fsGetStat :: FilePath -> IO (Either Errno FileStat)
fsGetStat path = do
    errorM rootLoggerName ("getstat " ++ path)
    info <- getInfo path
    case info of
        Just (stat, content) -> return $ Right stat
	Nothing              -> return $ Left eNOENT

fuseOps = defaultFuseOps {
        fuseInit = fsInit,
        fuseDestroy = fsDestroy,
        fuseOpenDirectory = fsOpenDir,
        fuseReadDirectory = fsReadDir,
        fuseReleaseDirectory = fsReleaseDir,
	fuseGetFileStat = fsGetStat
    }

main = do
    h <- fileHandler "dc.log" DEBUG
    updateGlobalLogger rootLoggerName (addHandler h)
    errorM rootLoggerName "startup"
    withArgs ["mnt"] $ fuseMain fuseOps defaultExceptionHandler
