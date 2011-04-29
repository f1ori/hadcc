--- |
--- | This module contains type definitions for Filelist.hs to prevent circular imports
--- |
--- Copyright   : (c) Florian Richter 2011
--- License     : GPL
---


module FilelistTypes where

import System.Posix.Types
import System.FilePath
import Control.DeepSeq
import Foreign.C.Types

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

instance NFData CTime

instance NFData TreeNode where
    rnf (DirNode name path children) = rnf name `seq` rnf path `seq` rnf children
    rnf (FileNode name path size modTime hash) = rnf name `seq` rnf path `seq` rnf size `seq` rnf modTime `seq` rnf hash
-- vim: sw=4 expandtab
