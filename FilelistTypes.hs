--- |
--- | This module contains type definitions for Filelist.hs to prevent circular imports
--- |
--- Copyright   : (c) Florian Richter 2011
--- License     : GPL
---


module FilelistTypes where

import System.Posix.Types
import System.FilePath

data TreeNode = DirNode  {
	               dirNodeName     :: String
	             , dirNodePath     :: FilePath
                     , dirNodeChildren :: [TreeNode]
                     }
          | FileNode {
	               fileNodeName    :: String
	             , fileNodePath    :: FilePath
	             , fileNodeSize    :: Integer
	             , fileNodeModTime :: EpochTime
	             , fileNodeHash    :: Maybe String
	             }
	    deriving (Eq, Show)

-- vim: sw=4 expandtab
