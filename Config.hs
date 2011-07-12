--- |
--- | This module contains general Configuration stuff and general type definitions needed everywhere
--- |
--- Copyright   : (c) Florian Richter 2011
--- License     : GPL
---

module Config where

import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import Data.ConfigFile
import Data.Char
import Data.Either.Utils
import System.Directory
import FilelistTypes
import TTHTypes
import FixedQueueTypes
import FilelistCacheTypes

type Nick = String

type DownloadHandler = Handle -> L.ByteString -> IO ()
-- | a download job, with filename and download handler
data DcJob = Job String DownloadHandler

-- | static application configuration
data AppConfig = AppConfig {
      configHubIp :: String
    , configHubPort :: String
    , configMyIp :: String
    , configMyPort :: String
    , configNick :: String
    , configEMail :: String
    , configDescription :: String
    , configShareSize :: String
    , configShareDir :: String
    , configMountpoint :: String
    , configCacheFile :: FilePath
    }

-- | application state, passed to almost all threads
data AppState = AppState {
    -- | the static configuration
      appConfig :: AppConfig
    -- | own share
    , appFileTree :: MVar IndexedFileTree
    -- | job for the nicks, a file to download
    , appJobs :: TVar (M.Map Nick DcJob)
    -- | connections to nicks
    , appConnections :: MVar [Nick]
    -- | filelists of nicks to transfer it to other function/thread (TODO remove)
    , appFilelists :: TVar (M.Map Nick B.ByteString)
    -- | unused
    , appUploads :: MVar [String]
    -- | unused
    , appDownloads :: MVar [String]
    -- | network handle for hub communication
    , appHubHandle :: MVar Handle
    -- | current nicklist on hub (filesystemsafe -> original, info-string)
    , appNickList :: MVar (M.Map Nick (Nick, String))
    -- | cache for hashes, see TTH.hs
    , appTTHCache :: TTHCache
    -- | last chat msgs
    , appChatMsgs :: FixedQueue String
    -- | cache filelists for filesystem
    , appFilelistCache :: FilelistCache
    }

-- | load config from file into config-object
loadConfig :: FilePath -> IO AppConfig
loadConfig configFile = do
    val <- readfile emptyCP configFile
    let cp = forceEither val
    homedir <- getHomeDirectory
    let cacheFile = homedir ++ "/.hadcc.cache"
    -- TODO: check welformed (no spaces in nick, numbers, ...)
    return AppConfig {
                   configHubIp = forceEither $ get cp "hub" "ip"
                 , configHubPort = forceEither $ get cp "hub" "port"
                 , configMyIp = forceEither $ get cp "dc" "myip"
                 , configMyPort = forceEither $ get cp "dc" "myport"
                 , configNick = forceEither $ get cp "dc" "nick"
                 , configEMail = forceEither $ get cp "dc" "email"
                 , configDescription = forceEither $ get cp "dc" "description"
                 , configShareSize = forceEither $ get cp "dc" "sharesize"
                 , configShareDir = forceEither $ get cp "dc" "sharedir"
                 , configMountpoint = forceEither $ get cp "filesystem" "mountpoint"
                 , configCacheFile = cacheFile
                 }


-- | create new and empty application state object
newAppState :: AppConfig -> IO AppState
newAppState appConfig = do
    fileTree <- newEmptyMVar
    jobs <- newTVarIO M.empty
    connections <- newMVar []
    filelists <- newTVarIO M.empty
    uploads <- newMVar []
    downloads <- newMVar []
    nicklist <- newMVar M.empty
    hubHandle <- newEmptyMVar
    tthCache <- newEmptyMVar
    chatMsgs <- newFixedQueue
    filelistCache <- newFilelistCache
    return AppState {
                     appConfig = appConfig
                   , appFileTree = fileTree
                   , appJobs = jobs
                   , appConnections = connections
                   , appFilelists = filelists
                   , appUploads = uploads
                   , appDownloads = downloads
                   , appHubHandle = hubHandle
                   , appNickList = nicklist
                   , appTTHCache = tthCache
                   , appChatMsgs = chatMsgs
                   , appFilelistCache = filelistCache
                   }


-- | generate DC myinfo command for hub communication
getMyINFOStr :: AppState -> String
getMyINFOStr appState = "$MyINFO $ALL " ++ nick ++ " " ++ desc ++ "<hdc V:0.1,M:A,H:1/0/0,S:10>$ $LAN(T1)1$" ++ email ++ "$" ++ shareSize ++ "$|"
    where
        config = appConfig appState
        nick = configNick config
        desc = configDescription config
        email = configEMail config
        shareSize = configShareSize config

filesystemSafe :: Nick -> Nick
filesystemSafe nick = filter (\x -> isAlphaNum x || elem x ".:-_+" ) nick

-- vim: sw=4 expandtab
