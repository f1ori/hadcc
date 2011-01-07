import Control.Concurrent
import Config
import Http
import HttpHandler
import DCCommon
import DCToHub


main = do
    config <- loadConfig "Hadcc.cfg"
    appState <- newAppState config
    forkIO $ httpServer appState httpHandler
    tcpLoop appState (configHubIp config) (configHubPort config) DontKnow startupHub handleHub

-- vim: sw=4 expandtab
