module Config where

import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.ConfigFile
import Data.Char
import Data.Either.Utils
import FilelistTypes
import TTHTypes
import FixedQueueTypes

type Nick = String

-- | static application configuration
data AppConfig = AppConfig {
      configHttpIp :: String
    , configHttpPort :: String
    , configHubIp :: String
    , configHubPort :: String
    , configMyIp :: String
    , configMyPort :: String
    , configNick :: String
    , configEMail :: String
    , configDescription :: String
    , configShareSize :: String
    , configShareDir :: String
    , configDownloadDir :: String
    }

-- | application state, passed to almost all threads
data AppState = AppState {
    -- | the static configuration
      appConfig :: AppConfig
    -- | filetree
    , appFileTree :: MVar TreeNode
    -- | jobs for the nicks, list of files to download
    , appJobs :: TVar (M.Map Nick [String])
    -- | filelists of nicks
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
    , appTTHCache :: MVar TTHCache
    -- | last chat msgs
    , appChatMsgs :: FixedQueue String
    -- | last chat msgs
    , appLogHandle :: MVar Handle
    }

-- | load config from file into config-object
loadConfig :: FilePath -> IO AppConfig
loadConfig configFile = do
    val <- readfile emptyCP configFile
    let cp = forceEither val
    -- TODO: check welformed (no spaces in nick, numbers, ...)
    return AppConfig {
                   configHttpIp = forceEither $ get cp "http" "ip"
                 , configHttpPort = forceEither $ get cp "http" "port"
                 , configHubIp = forceEither $ get cp "hub" "ip"
                 , configHubPort = forceEither $ get cp "hub" "port"
                 , configMyIp = forceEither $ get cp "dc" "myip"
                 , configMyPort = forceEither $ get cp "dc" "myport"
                 , configNick = forceEither $ get cp "dc" "nick"
                 , configEMail = forceEither $ get cp "dc" "email"
                 , configDescription = forceEither $ get cp "dc" "description"
                 , configShareSize = forceEither $ get cp "dc" "sharesize"
                 , configShareDir = forceEither $ get cp "dc" "sharedir"
                 , configDownloadDir = forceEither $ get cp "dc" "downloaddir"
                 }


-- | create new and empty application state object
newAppState :: AppConfig -> IO AppState
newAppState appConfig = do
    fileTree <- newEmptyMVar
    jobs <- newTVarIO M.empty
    filelists <- newTVarIO M.empty
    uploads <- newMVar []
    downloads <- newMVar []
    nicklist <- newMVar M.empty
    hubHandle <- newEmptyMVar
    tthCache <- newEmptyMVar
    chatMsgs <- newFixedQueue
    handle <- openFile "dc.log" AppendMode
    logHandle <- newMVar handle
    return AppState {
                     appConfig = appConfig
                   , appFileTree = fileTree
                   , appJobs = jobs
                   , appFilelists = filelists
                   , appUploads = uploads
                   , appDownloads = downloads
                   , appHubHandle = hubHandle
                   , appNickList = nicklist
                   , appTTHCache = tthCache
                   , appChatMsgs = chatMsgs
                   , appLogHandle = logHandle
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

logMsg :: AppState -> String -> IO ()
logMsg appState msg = withMVar (appLogHandle appState) $ \h -> do
                       hPutStrLn h msg
		       hFlush h
