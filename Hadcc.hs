import Control.Concurrent
import Config
import Filelist
import TTH
--import Http
--import HttpHandler
import DCCommon
import DCToHub
import DCToClient
import Filesystem
import FilesystemHandler


main = do
    config <- loadConfig "Hadcc.cfg"
    appState <- newAppState config
    loadTTHCache appState "Hadcc.cache"
    putMVar (appFileTree appState) =<< getFileList appState (configShareDir config)
    withMVar (appFileTree appState) (\tree -> putStrLn $ treeNodeToXml tree)
    hashFileList appState
    withMVar (appFileTree appState) (\tree -> putStrLn $ treeNodeToXml tree)
    -- forkIO $ httpServer appState httpHandler
    forkIO $ startDCServer (configMyIp config) (configMyPort config) (ToClient Nothing DontKnow) (startupClient appState) (handleClient appState)
    forkIO $ openDCConnection (configHubIp config) (configHubPort config) ToHub (startupHub appState) (handleHub appState)
    startupFileSystem dcFileInfo

-- vim: sw=4 expandtab
