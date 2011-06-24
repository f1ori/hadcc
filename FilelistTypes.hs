{-# LANGUAGE BangPatterns #-}
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
import Data.HashTable
import Control.Monad
import qualified Data.Text as T

-- | filelist entry, a main datastructure of hadcc
data TreeNode = DirNode  {
                       dirNodeName     :: !T.Text
                     , dirNodePath     :: !T.Text
                     , dirNodeChildren :: ![TreeNode]
                     }
          | FileNode {
                       fileNodeName    :: !T.Text
                     , fileNodePath    :: !T.Text
                     , fileNodeSize    :: !Integer
                     , fileNodeModTime :: !EpochTime
                     , fileNodeHash    :: !(Maybe T.Text)
                     }
            deriving (Eq, Show)

instance NFData CTime

-- deepseq stuff
instance NFData TreeNode where
    rnf (DirNode name path children) = rnf name `seq` rnf path `seq` rnf children
    rnf (FileNode name path size modTime hash) = rnf name `seq` rnf path `seq` rnf size `seq` rnf modTime `seq` rnf hash

-- | Filelist with index on tth-hashes
data IndexedFileTree = IndexedFileTree TreeNode (HashTable T.Text TreeNode)

instance NFData IndexedFileTree where
    rnf (IndexedFileTree node hashtable) = rnf node `seq` rnf hashtable

instance NFData (HashTable key val)


-- | create IndexedFileTree from TreeNode and fill the index
newIndexedFileTree :: TreeNode -> IO IndexedFileTree
newIndexedFileTree tree = IndexedFileTree tree `liftM` fromList hashText (treeToHashList tree)
    where
        treeToHashList :: TreeNode -> [(T.Text, TreeNode)]
        treeToHashList (DirNode _ _ children) = concat $ map treeToHashList children
        treeToHashList node@(FileNode _ _ _ _ (Just hash)) = [(hash, node)]
        treeToHashList node@(FileNode _ _ _ _ Nothing) = []

hashText = hashString . T.unpack

-- vim: sw=4 expandtab
