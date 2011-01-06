module Config where

import Control.Concurrent
import Filelist
import Data.ConfigFile
import Data.Either.Utils

data AppConfig = AppConfig {
      configHttpIp :: String
    , configHttpPort :: String
    , configHubIp :: String
    , configHubPort :: String
    , configNick :: String
    , configShareDir :: String
    , configDownloadDir :: String
    }
data AppState = AppState {
      appConfig :: AppConfig
    , appFileTree :: MVar Node
    , appUploads :: MVar [String]
    , appDownloads :: MVar [String]
    }

loadConfig :: FilePath -> IO AppConfig
loadConfig configFile = do
    val <- readfile emptyCP configFile
    let cp = forceEither val
    return AppConfig {
                   configHttpIp = forceEither $ get cp "http" "ip"
                 , configHttpPort = forceEither $ get cp "http" "port"
                 , configHubIp = forceEither $ get cp "hub" "ip"
                 , configHubPort = forceEither $ get cp "hub" "port"
                 , configNick = forceEither $ get cp "dc" "nick"
                 , configShareDir = forceEither $ get cp "dc" "sharedir"
                 , configDownloadDir = forceEither $ get cp "dc" "downloaddir"
                 }


newAppState :: AppConfig -> IO AppState
newAppState appConfig = do
    fileTreeNode <- getFileList (configShareDir appConfig)
    fileTree <- newMVar fileTreeNode
    uploads <- newMVar []
    downloads <- newMVar []
    return AppState {
                     appConfig = appConfig
                   , appFileTree = fileTree
                   , appUploads = uploads
                   , appDownloads = downloads
                   }
