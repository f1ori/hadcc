--- |
--- | This module contains a simple abstraction of the fuse library
--- | This should be reusable by other projects
--- |
--- Copyright   : (c) Florian Richter 2011
--- License     : GPL
---

module Filesystem where

import System.Fuse
import System.Posix.Types
import System.Posix.Files
import System.Posix.User
import System.Environment(withArgs)
import System.Log.Logger
import System.Log.Handler.Simple
import qualified Data.ByteString.Char8 as B
import Control.Monad


type FsObject = (FileStat, FsContent)
data FsContent = FsDir [FilePath]
               | FsFile (IO (ReadFunc, CloseFunc))

type FileInfoHandler = FilePath -> IO (Maybe FsObject)

type ReadFunc = Integer -> Integer -> IO B.ByteString
type CloseFunc = IO ()

type FileHandle = (ReadFunc, CloseFunc)
type UserGroupID = (UserID, GroupID)

getUserGroupID :: IO UserGroupID
getUserGroupID = do
   uid <- getRealUserID
   gid <- getRealGroupID
   return (uid, gid)

getStatDir ugid = getStat ugid Directory "rx" 0
getStatFileR ugid size = getStat ugid RegularFile "r" size
getStatFileRW ugid size = getStat ugid RegularFile "rw" size

getStat :: Integral n => UserGroupID -> EntryType -> String -> n -> FileStat
getStat (uid, gid) entryType fileModeStr size = FileStat
    { statEntryType = entryType
    , statFileMode = strToFileMode fileModeStr
    , statLinkCount = 1
    , statFileOwner = uid
    , statFileGroup = gid
    , statSpecialDeviceID = 0
    , statFileSize = fromInteger (toInteger size)
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


fsOpen :: FileInfoHandler -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno FileHandle)
fsOpen infoHandler path mode flags = do
    case mode of
        ReadOnly -> do 
            object <- infoHandler path
	    case object of
	        Just (_, FsFile openFunc)  -> do
                          (readFunc, closeFunc) <- openFunc
                          return $ Right (readFunc, closeFunc)
	        Just (_, FsDir _)  -> return $ Left eNOENT
                Nothing            -> return $ Left eNOENT

	_ -> return $ Left eACCES

fsRead :: FilePath -> FileHandle -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
fsRead path (readFunc, _) size offset = Right `liftM` readFunc (fromIntegral size) (fromIntegral offset)
        --"/status" -> return $ Right (B.take (fromIntegral size) $ B.drop (fromIntegral offset) (B.pack "alles ok"))

fsRelease :: FilePath -> FileHandle -> IO ()
fsRelease path (_, closeFunc) = closeFunc

fsOpenDir :: FileInfoHandler -> FilePath -> IO Errno
fsOpenDir infoHandler path = do
    result <- fsReadDir infoHandler path
    case result of
        Right _ -> return eOK
        Left errno -> return errno


fsReadDir :: FileInfoHandler -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
fsReadDir infoHandler path = do
    ugid <- getUserGroupID
    info <- infoHandler path
    case info of
        Just (stat, FsDir list)   -> do
                      stats <- mapM (getStats path) list
                      return $ Right ([(".", getStatDir ugid), ("..", getStatDir ugid)] ++ (zip list stats))
        Just (stat, FsFile _) ->
                      return $ Left eNOTDIR
        Nothing                   ->
                      return $ Left eNOENT
    where
        getStats path name = do
            let completepath = if (last path) == '/' then path ++ name else path ++ "/" ++ name
            Just (stat, _) <- infoHandler completepath
            return stat


fsReleaseDir :: FileInfoHandler -> FilePath -> IO Errno
fsReleaseDir infoHandler path = do
    return eOK

fsGetStat :: FileInfoHandler -> FilePath -> IO (Either Errno FileStat)
fsGetStat infoHandler path = do
    info <- infoHandler path
    case info of
        Just (stat, _) -> return $ Right stat
	Nothing        -> return $ Left eNOENT

fuseOps startHandler stopHandler infoHandler = defaultFuseOps {
        fuseInit = startHandler,
        fuseDestroy = stopHandler,
        fuseOpen = fsOpen infoHandler,
        fuseRead = fsRead,
        fuseRelease = fsRelease,
        fuseOpenDirectory = fsOpenDir infoHandler,
        fuseReadDirectory = fsReadDir infoHandler,
        fuseReleaseDirectory = fsReleaseDir infoHandler,
	fuseGetFileStat = fsGetStat infoHandler
    }

-- | start fuse manager, puts program in background
startupFileSystem :: IO () -> IO () -> FileInfoHandler -> IO ()
startupFileSystem startHandler stopHandler infoHandler = do
    withArgs ["mnt", "-f"] $ fuseMain (fuseOps startHandler stopHandler infoHandler) defaultExceptionHandler

-- vim: sw=4 expandtab
