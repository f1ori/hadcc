--- |
--- | Main module, startup code
--- |
--- Copyright   : (c) Florian Richter 2011
--- License     : GPL
---

import Control.Concurrent
import System.Log.Logger
import System.Log.Handler.Simple
import Config
import Filelist
import TTH
import DCCommon
import DCToHub
import DCToClient
import Filesystem
import FilesystemHandler

start appState = do
    let config = appConfig appState
    forkIO $ startDCServer (configMyIp config) (configMyPort config) (ToClient Nothing DontKnow) (startupClient appState) (handleClient appState)
    forkIO $ openDCConnection (configHubIp config) (configHubPort config) ToHub (startupHub appState) (handleHub appState)
    return ()
    
stop appState = return ()

main = do
    config <- loadConfig "Hadcc.cfg"
    appState <- newAppState config
    loadTTHCache appState "Hadcc.cache"
    putMVar (appFileTree appState) =<< getFileList appState (configShareDir config)
    withMVar (appFileTree appState) (\tree -> putStrLn $ treeNodeToXml tree)
    hashFileList appState
    withMVar (appFileTree appState) (\tree -> putStrLn $ show tree)
    startupFileSystem (configMountpoint config) (start appState) (stop appState) (dcFileInfo appState)

-- vim: sw=4 expandtab
