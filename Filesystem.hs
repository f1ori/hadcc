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
-- | filesystem entry
data FsContent = FsDir (IO [FilePath])
               | FsFile (OpenMode -> IO (Either Errno (ReadFunc, Maybe WriteFunc, CloseFunc))) FuseOpenInfo

-- | function, that provides the content of the filesystem
type FileInfoHandler = FilePath -> IO (Maybe FsObject)

-- | type signatures of read function
type ReadFunc = Integer -> Integer -> IO B.ByteString
-- | type signatures of write function
type WriteFunc = B.ByteString -> Integer -> IO ByteCount
-- | type signatures of close function
type CloseFunc = IO ()

type FileHandle = (ReadFunc, Maybe WriteFunc, CloseFunc)
type UserGroupID = (UserID, GroupID)

-- | get user and group id of current user
getUserGroupID :: IO UserGroupID
getUserGroupID = do
   uid <- getRealUserID
   gid <- getRealGroupID
   return (uid, gid)

-- helpers for usefull stats
getStatDir ugid = getStat ugid Directory "rx" 0
getStatFileR ugid size = getStat ugid RegularFile "r" size
getStatFileRW ugid size = getStat ugid RegularFile "rw" size
getStatFileRX ugid size = getStat ugid RegularFile "rx" size

-- | helper for generic file/directory stat
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


-- | fuse open handler
fsOpen :: FileInfoHandler -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno (FuseOpenInfo, FileHandle))
fsOpen infoHandler path mode flags = do
    object <- infoHandler path
    case object of
        Just (_, FsFile openFunc openInfo)  -> do
                  result <- openFunc mode
                  case result of
                      Left errno ->
                          return $ Left errno
                      Right (readFunc, writeFunc, closeFunc) ->
                          return $ Right (openInfo ,(readFunc, writeFunc, closeFunc))
        Just (_, FsDir _)  -> return $ Left eNOENT
        Nothing            -> return $ Left eNOENT


-- | fuse read handler
fsRead :: FilePath -> FileHandle -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
fsRead path (readFunc, _, _) size offset = Right `liftM` readFunc (fromIntegral size) (fromIntegral offset)

-- | fuse write handler
fsWrite :: FilePath -> FileHandle -> B.ByteString -> FileOffset -> IO (Either Errno ByteCount)
fsWrite path (_, writeFunc, _) stuff offset = case writeFunc of
   Just writeF -> Right `liftM` writeF stuff (fromIntegral offset)
   Nothing     -> return $ Left eACCES

-- | fuse release (close systemcall) handler
fsRelease :: FilePath -> FileHandle -> IO ()
fsRelease path (_, _, closeFunc) = closeFunc

-- | fuse opendir handler
fsOpenDir :: FileInfoHandler -> FilePath -> IO Errno
fsOpenDir infoHandler path = do
    result <- fsReadDir infoHandler path
    case result of
        Right _ -> return eOK
        Left errno -> return errno


-- | fuse readdir handler
fsReadDir :: FileInfoHandler -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
fsReadDir infoHandler path = do
    ugid <- getUserGroupID
    info <- infoHandler path
    case info of
        Just (stat, FsDir list)   -> do
                      dirlist <- list
                      stats <- mapM (getStats path) dirlist
                      return $ Right ([(".", getStatDir ugid), ("..", getStatDir ugid)] ++ (zip dirlist stats))
        Just (stat, FsFile _ _) ->
                      return $ Left eNOTDIR
        Nothing                   ->
                      return $ Left eNOENT
    where
        getStats path name = do
            let completepath = if (last path) == '/' then path ++ name else path ++ "/" ++ name
            Just (stat, _) <- infoHandler completepath
            return stat


-- | fuse releasedir handler
fsReleaseDir :: FileInfoHandler -> FilePath -> IO Errno
fsReleaseDir infoHandler path = do
    return eOK

-- | fuse getstat handler
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
        fuseWrite = fsWrite,
        fuseRelease = fsRelease,
        fuseOpenDirectory = fsOpenDir infoHandler,
        fuseReadDirectory = fsReadDir infoHandler,
        fuseReleaseDirectory = fsReleaseDir infoHandler,
	fuseGetFileStat = fsGetStat infoHandler
    }

-- | start fuse manager, puts program in background
startupFileSystem :: FilePath -> IO () -> IO () -> FileInfoHandler -> IO ()
startupFileSystem mountpoint startHandler stopHandler infoHandler = do
    --withArgs [mountpoint] $ fuseMain (fuseOps startHandler stopHandler infoHandler) defaultExceptionHandler
    withArgs [mountpoint, "-f", "-d"] $ fuseMain (fuseOps startHandler stopHandler infoHandler) defaultExceptionHandler

-- vim: sw=4 expandtab
