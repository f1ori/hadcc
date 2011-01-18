module FilelistTypes where

import System.FilePath
import Time

data TreeNode = DirNode  {
	               dirNodeName     :: String
	             , dirNodePath     :: FilePath
                     , dirNodeChildren :: [TreeNode]
                     }
          | FileNode {
	               fileNodeName    :: String
	             , fileNodePath    :: FilePath
	             , fileNodeSize    :: Integer
	             , fileNodeModTime :: CalendarTime
	             , fileNodeHash    :: Maybe String
	             }
	    deriving (Eq, Show)

