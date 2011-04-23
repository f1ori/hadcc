module TTHTypes where

import System.Posix.Types
import qualified Data.Map as M

-- hash and modification date as values
type TTHCache = M.Map FilePath (String, EpochTime)

