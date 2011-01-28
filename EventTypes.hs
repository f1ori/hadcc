module EventTypes where

import qualified Data.Sequence as S
import Control.Concurrent.STM.TVar (TVar, newTVarIO)

type Nick = String

-- | event type for http polling
data Event = EChatMsg String
           | EDownloadStarted Nick String
           | EDownloadStatus Nick String Integer
           | EDownloadFinished Nick String
           | EUploadStarted Nick String
           | EUploadStatus Nick String Integer
           | EUploadFinished Nick String

type EventList = (Int, S.Seq Event)

-- | create event list
newEventList :: IO (TVar EventList)
newEventList = newTVarIO (0, S.empty)

-- vim: sw=4 expandtab
