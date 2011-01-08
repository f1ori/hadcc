import Control.Concurrent
import Config
import Filelist
import TTH
import Http
import HttpHandler
import DCCommon
import DCToHub


main = do
    config <- loadConfig "Hadcc.cfg"
    appState <- newAppState config
    loadTTHCache appState "Hadcc.cache"
    putMVar (appFileTree appState) =<< getFileList appState (configShareDir config)
    withMVar (appFileTree appState) (\tree -> putStrLn $ show tree)
    hashFileList appState
    withMVar (appFileTree appState) (\tree -> putStrLn $ show tree)
    forkIO $ httpServer appState httpHandler
    tcpLoop appState (configHubIp config) (configHubPort config) DontKnow startupHub handleHub

-- vim: sw=4 expandtab
