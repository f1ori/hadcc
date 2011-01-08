module FilelistTypes where

import System.FilePath
import Time

data Node = DirNode  {
	               dirNodeName     :: String
	             , dirNodePath     :: FilePath
                     , dirNodeChildren :: [Node]
                     }
          | FileNode {
	               fileNodeName    :: String
	             , fileNodePath    :: FilePath
	             , fileNodeSize    :: Integer
	             , fileNodeModTime :: CalendarTime
	             , fileNodeHash    :: Maybe String
	             }
	    deriving (Eq, Show)

