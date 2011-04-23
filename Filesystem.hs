module Filesystem where

import System.Fuse
import System.Posix.Types
import System.Posix.Files
import System.Posix.User
import System.Environment(withArgs)
import System.Log.Logger
import System.Log.Handler.Simple
import qualified Data.ByteString.Char8 as B


type FileInfoHandler = FilePath -> IO (Maybe (FileStat, [FilePath]))


type FileHandle = ()
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
    , statLinkCount = 1
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

fsOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno FileHandle)
fsOpen path mode flags = do
    case mode of
        ReadOnly -> return $ Right ()
        _        -> return $ Left eACCES

fsRead :: FilePath -> FileHandle -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
fsRead path handle size offset = do
    errorM rootLoggerName ("read " ++ path ++ " size:" ++(show size) ++ " offset: " ++(show offset))
    errorM rootLoggerName $ show (B.take (fromIntegral size) $ B.drop (fromIntegral offset) (B.pack "alles ok"))
    case path of
        "/status" -> return $ Right (B.take (fromIntegral size) $ B.drop (fromIntegral offset) (B.pack "alles ok"))
	_         -> return $ Left eINVAL

fsRelease :: FilePath -> fh -> IO ()
fsRelease path handle = return ()

fsOpenDir :: FileInfoHandler -> FilePath -> IO Errno
fsOpenDir infoHandler path = do
    errorM rootLoggerName ("opendir " ++ path)
    result <- fsReadDir infoHandler path
    case result of
        Right _ -> return eOK
        Left errno -> return errno


fsReadDir :: FileInfoHandler -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
fsReadDir infoHandler path = do
    errorM rootLoggerName ("readdir " ++ path)
    ugid <- getUserGroupID
    info <- infoHandler path
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
            Just (stat, files) <- infoHandler completepath
            return stat


fsReleaseDir :: FileInfoHandler -> FilePath -> IO Errno
fsReleaseDir infoHandler path = do
    errorM rootLoggerName ("closedir " ++ path)
    return eOK

fsGetStat :: FileInfoHandler -> FilePath -> IO (Either Errno FileStat)
fsGetStat infoHandler path = do
    errorM rootLoggerName ("getstat " ++ path)
    info <- infoHandler path
    case info of
        Just (stat, content) -> return $ Right stat
	Nothing              -> return $ Left eNOENT

fuseOps infoHandler = defaultFuseOps {
        fuseInit = fsInit,
        fuseDestroy = fsDestroy,
        fuseOpen = fsOpen,
        fuseRead = fsRead,
        fuseRelease = fsRelease,
        fuseOpenDirectory = fsOpenDir infoHandler,
        fuseReadDirectory = fsReadDir infoHandler,
        fuseReleaseDirectory = fsReleaseDir infoHandler,
	fuseGetFileStat = fsGetStat infoHandler
    }

-- | start fuse manager, puts program in background
startupFileSystem :: FileInfoHandler -> IO ()
startupFileSystem infoHandler = do
    h <- fileHandler "dc.log" DEBUG
    updateGlobalLogger rootLoggerName (addHandler h)
    errorM rootLoggerName "startup"
    withArgs ["mnt"] $ fuseMain (fuseOps infoHandler) defaultExceptionHandler
