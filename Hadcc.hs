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
    forkOS $ startDCServer (configMyIp config) (configMyPort config) (ToClient Nothing DontKnow) (startupClient appState) (handleClient appState)
    forkOS $ openDCConnection (configHubIp config) (configHubPort config) ToHub (startupHub appState) (handleHub appState)
    return ()
    
stop appState = return ()

main = do
    config <- loadConfig "Hadcc.cfg"
    appState <- newAppState config
    loadTTHCache appState "Hadcc.cache"
    putMVar (appFileTree appState) =<< getFileList appState (configShareDir config)
    withMVar (appFileTree appState) (\tree -> putStrLn $ treeNodeToXml tree)
    hashFileList appState
    withMVar (appFileTree appState) (\tree -> putStrLn $ treeNodeToXml tree)
    startupFileSystem (start appState) (stop appState) (dcFileInfo appState)

-- vim: sw=4 expandtab
