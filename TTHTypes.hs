module TTHTypes where

import Time
import qualified Data.Map as M

-- hash and modification date as values (CalendarTime is easier to serialize than ClockTime)
type TTHCache = M.Map FilePath (String, CalendarTime)

