import Control.Concurrent
import Config
import Filelist
import TTH
import Http
import HttpHandler
import DCCommon
import DCToHub
import DCToClient


main = do
    config <- loadConfig "Hadcc.cfg"
    appState <- newAppState config
    loadTTHCache appState "Hadcc.cache"
    putMVar (appFileTree appState) =<< getFileList appState (configShareDir config)
    withMVar (appFileTree appState) (\tree -> putStrLn $ show tree)
    hashFileList appState
    withMVar (appFileTree appState) (\tree -> putStrLn $ show tree)
    forkIO $ httpServer appState httpHandler
    forkIO $ startDCServer (configMyIp config) (configMyPort config) (ToClient Nothing DontKnow) (startupClient appState) (handleClient appState)
    openDCConnection (configHubIp config) (configHubPort config) ToHub (startupHub appState) (handleHub appState)

-- vim: sw=4 expandtab
