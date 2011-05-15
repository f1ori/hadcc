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

-- hash and modification date as values
type TTHCache = M.Map T.Text (T.Text, EpochTime)

-- vim: sw=4 expandtab
