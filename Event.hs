module Event where

import qualified Data.Sequence as S
import Data.Foldable (toList)
import Control.Concurrent.STM
import Control.Monad (when)
import EventTypes
import Config

max_entries = 100

-- | put event in event list
sendEvent :: AppState -> Event -> IO ()
sendEvent appState event = atomically $ do
    (curNum, list) <- readTVar (appEventList appState)
    writeTVar (appEventList appState) (curNum+1, event S.<| (S.take max_entries list))

-- | blocks until events with num>last available
receiveEvents :: AppState -> Int -> IO (Int, [Event])
receiveEvents appState last = atomically $ do
    (curNum, list) <- readTVar (appEventList appState)
    when (last >= curNum) retry
    return (curNum, reverse $ toList $ S.take (curNum - last) list)

-- vim: sw=4 expandtab
