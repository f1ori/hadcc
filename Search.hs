module Search where

import Text.Printf
import Config
import qualified Data.Text as T

data Search = SearchRec {
      searchSizeRestricted :: Bool
    , searchIsMaxSize :: Bool
    , searchSize :: Integer
    , searchDataType :: SearchDataType
    , searchPattern :: String
    }
    deriving (Show)

data SearchDataType = Any
                    | Audio
                    | Archive
                    | Document
                    | Executable
                    | Picture
                    | Video
                    | Folder
                    | TTH
    deriving (Show)

data SearchResult = SearchResult {
      srNick :: Nick
    , srFile :: String
    , srPath :: FilePath
    , srSize :: Integer
    , srFreeSlots :: Integer
    , srTotalSlots :: Integer
    , srHash :: String
    }
    deriving (Show)

escapePattern :: String -> String
escapePattern (' ':xs) = '$':(escapePattern xs)
escapePattern ('|':xs) = '$':(escapePattern xs)
escapePattern ('?':xs) = '$':(escapePattern xs)
escapePattern (x:xs)   =  x :(escapePattern xs)


simpleSearch :: T.Text -> Search
simpleSearch str = SearchRec {
      searchSizeRestricted = False
    , searchIsMaxSize = False
    , searchSize = 0
    , searchDataType = Any
    , searchPattern = (T.unpack $ T.strip str)
    }

tthSearch :: String -> Search
tthSearch tth = SearchRec {
      searchSizeRestricted = False
    , searchIsMaxSize = False
    , searchSize = 0
    , searchDataType = TTH
    , searchPattern = ("TTH:" ++ tth)
    }

searchToDC :: Search -> String
searchToDC search = printf "%c?%c?%d?%d?%s"
                        (boolToDc $ searchSizeRestricted search)
                        (boolToDc $ searchIsMaxSize search)
                        (searchSize search)
                        (dataTypeToDc $ searchDataType search)
                        (searchPattern search)
    where
        boolToDc True = 'T'
        boolToDc False = 'F'
        dataTypeToDc :: SearchDataType -> Integer
        dataTypeToDc Any        = 1
        dataTypeToDc Audio      = 2
        dataTypeToDc Archive    = 3
        dataTypeToDc Document   = 4
        dataTypeToDc Executable = 5
        dataTypeToDc Picture    = 6
        dataTypeToDc Video      = 7
        dataTypeToDc Folder     = 8
        dataTypeToDc TTH        = 9


-- | parse active search result (including $SR)
dcToSearchResult :: T.Text -> SearchResult
dcToSearchResult msg = SearchResult {
      srNick = T.unpack nick
    , srFile = T.unpack file
    , srPath = T.unpack path
    , srSize = size
    , srFreeSlots = 0
    , srTotalSlots = 0
    , srHash = T.unpack hash
    }
    where
        (cmd, afterCmd) = (\(x,y)->(x,T.tail y)) $ T.breakOn space msg
        (nick, afterNick) = (\(x,y)->(x,T.tail y)) $ T.breakOn space afterCmd
        (pathSizeAndSlots, hashAndHub) = (\(x,y)->(T.init x,y)) $ T.breakOnEnd five afterNick
        hash = T.takeWhile (/=' ') $ T.drop 4 hashAndHub
        (pathAndSize, slots) = (\(x,y)->(T.init x,y)) $ T.breakOnEnd space pathSizeAndSlots
        isDirectory = (T.count five pathAndSize) == 0
        size = if isDirectory then 0 else read $ T.unpack $ T.tail $ T.dropWhile (/='\x5') pathAndSize
        completePath = T.takeWhile (/='\x5') pathAndSize
        (path, file) = if isDirectory then (completePath, T.empty) else T.breakOnEnd (T.pack "\\") completePath
        space = T.pack " "
        five  = T.pack ['\x5']

printSearchResult :: SearchResult -> String
printSearchResult sr = printf "%s %s %s %s %s\n"
                        (srNick sr)
                        (srFile sr)
                        (prettySize $ srSize sr)
                        (srPath sr)
                        (srHash sr)
	
-- | convert size in byte to string using readable units like Megabyte and Gigabyte
prettySize :: Integer -> String
prettySize size | size < 1024 = printf "%d B" size
                | size < 1024*1024 = printf "%.1f KiB" (((fromInteger size)/1024.0)::Double)
                | size < 1024*1024*1024 = printf "%.1f MiB" (((fromInteger size)/1024.0/1024.0)::Double)
                | otherwise = printf "%.1f GiB" (((fromInteger size)/1024.0/1024.0/1024.0)::Double)

-- vim: sw=4 expandtab
