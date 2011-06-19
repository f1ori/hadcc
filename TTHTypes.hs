--- |
--- | type definitions for TTH.hs to prevent cicular imports
--- |
--- Copyright   : (c) Florian Richter 2011
--- License     : GPL
---

module TTHTypes where

import System.Posix.Types
import qualified Data.Map as M
import qualified Data.Text as T
import Control.Concurrent
import Database.HDBC.Sqlite3

-- hash and modification date as values
type TTHCache = MVar Connection

-- vim: sw=4 expandtab
