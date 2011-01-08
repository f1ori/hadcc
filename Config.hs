module Config where

import System.IO
import Control.Concurrent
import Data.ConfigFile
import Data.Either.Utils
import FilelistTypes
import TTHTypes


data AppConfig = AppConfig {
      configHttpIp :: String
    , configHttpPort :: String
    , configHubIp :: String
    , configHubPort :: String
    , configNick :: String
    , configEMail :: String
    , configDescription :: String
    , configShareSize :: String
    , configShareDir :: String
    , configDownloadDir :: String
    }
data AppState = AppState {
      appConfig :: AppConfig
    , appFileTree :: MVar Node
    , appUploads :: MVar [String]
    , appDownloads :: MVar [String]
    , appHubHandle :: MVar Handle
    , appNickList :: MVar [String]
    , appTTHCache :: MVar TTHCache
    }

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
                 , configNick = forceEither $ get cp "dc" "nick"
                 , configEMail = forceEither $ get cp "dc" "email"
                 , configDescription = forceEither $ get cp "dc" "description"
                 , configShareSize = forceEither $ get cp "dc" "sharesize"
                 , configShareDir = forceEither $ get cp "dc" "sharedir"
                 , configDownloadDir = forceEither $ get cp "dc" "downloaddir"
                 }


newAppState :: AppConfig -> IO AppState
newAppState appConfig = do
    fileTree <- newEmptyMVar
    uploads <- newMVar []
    downloads <- newMVar []
    nicklist <- newEmptyMVar
    hubHandle <- newEmptyMVar
    tthCache <- newEmptyMVar
    return AppState {
                     appConfig = appConfig
                   , appFileTree = fileTree
                   , appUploads = uploads
                   , appDownloads = downloads
                   , appHubHandle = hubHandle
                   , appNickList = nicklist
                   , appTTHCache = tthCache
                   }


getMyINFOStr :: AppState -> String
getMyINFOStr appState = "$MyINFO $ALL " ++ nick ++ " " ++ desc ++ "<hdc V:0.1,M:A,H:1/0/0,S:10>$ $LAN(T1)1$" ++ email ++ "$" ++ shareSize ++ "$|"
    where
        config = appConfig appState
        nick = configNick config
        desc = configDescription config
        email = configEMail config
        shareSize = configShareSize config
