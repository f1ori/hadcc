module Search where

import Text.Printf
import qualified Data.Text as T
import Text.Regex
import Data.Maybe
import Control.Monad
import Control.Concurrent.MVar
import qualified Data.HashTable as H
import Data.Char (isAlphaNum)

import Config
import FilelistTypes

data Search = Search {
      searchSizeRestricted :: Bool
    , searchIsMaxSize :: Bool
    , searchSize :: Integer
    , searchDataType :: SearchDataType
    , searchPattern :: String
    , searchHash :: String
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
    deriving (Show, Eq)

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

-- | escape search pattern for dc
escapePattern :: String -> String
escapePattern (' ':xs) = '$':(escapePattern xs)
escapePattern ('|':xs) = '$':(escapePattern xs)
escapePattern ('?':xs) = '$':(escapePattern xs)
escapePattern (x:xs)   =  x :(escapePattern xs)


-- | construct simple search with pattern
simpleSearch :: T.Text -> Search
simpleSearch str = Search {
      searchSizeRestricted = False
    , searchIsMaxSize = False
    , searchSize = 0
    , searchDataType = Any
    , searchPattern = (T.unpack $ T.strip str)
    , searchHash = ""
    }

-- | construct simple search for TTH
tthSearch :: String -> Search
tthSearch tth = Search {
      searchSizeRestricted = False
    , searchIsMaxSize = False
    , searchSize = 0
    , searchDataType = TTH
    , searchPattern = ""
    , searchHash = tth
    }

-- | construct valid dc msg from search object
searchToDC :: Search -> String
searchToDC search = printf "%c?%c?%d?%d?%s"
                        (boolToDc $ searchSizeRestricted search)
                        (boolToDc $ searchIsMaxSize search)
                        (searchSize search)
                        (dataTypeToDc dataType)
                        (if dataType == TTH then searchHash search else searchPattern search)
    where
        boolToDc True = 'T'
        boolToDc False = 'F'
        dataType = searchDataType search
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

-- | construct valid dc command from SearchResult
searchResultToDc :: String -> Maybe String -> SearchResult -> String
searchResultToDc hub passiveUser searchResult = if (srFile searchResult) /= ""
    then printf "$SR %s %s%c%d %d/%d%c%s%s (%s)%s|"
            (srNick searchResult)
            (srFile searchResult)
            five
            (srSize searchResult)
            (srFreeSlots searchResult)
            (srTotalSlots searchResult)
            five
            "TTH:"
            (srHash searchResult)
            hub
            (rest passiveUser)
    else printf "$SR %s %s %d/%d%c%s (%s)%s|"
            (srNick searchResult)
            (srPath searchResult)
            (srFreeSlots searchResult)
            (srTotalSlots searchResult)
            five
            "Hubname"
            hub
            (rest passiveUser)
    where
        rest (Just passiveUser) = "\x5\&" ++ passiveUser
        rest Nothing = ""
        five :: Char
        five = '\x5'

-- | parse a dc search command (with $Search)
dcToSearch :: T.Text -> (String, Search)
dcToSearch msg = (T.unpack response, Search {
      searchSizeRestricted = sizeRestricted
    , searchIsMaxSize = isMaxSize
    , searchSize = size
    , searchDataType = dataType
    , searchPattern = pattern
    , searchHash = hash
    })
    where
        (cmd, afterCmd) = (\(x,y)->(x,T.tail y)) $ T.breakOn space msg
        (response, searchstr) = (\(x,y)->(x,T.tail y)) $ T.breakOn space afterCmd
        splitSearch = T.splitOn (T.pack "?") searchstr
        sizeRestricted = strToBool $ T.unpack (splitSearch !! 0)
        isMaxSize = strToBool $ T.unpack (splitSearch !! 1)
        size =  read $ T.unpack (splitSearch !! 2)
        dataType = dcToDataType $ T.unpack (splitSearch !! 3)
        patternStr = splitSearch !! 4
        pattern = T.unpack $ if dataType == TTH then T.pack "" else unescape patternStr
        hash = T.unpack $ if dataType == TTH then T.drop 4 $ unescape patternStr else T.pack ""
        strToBool "T" = True
        strToBool  _  = False
        dcToDataType "2" = Audio
        dcToDataType "3" = Archive
        dcToDataType "4" = Document
        dcToDataType "5" = Executable
        dcToDataType "6" = Picture
        dcToDataType "7" = Video
        dcToDataType "8" = Folder
        dcToDataType "9" = TTH
        dcToDataType  _  = Any
        space = T.pack " "
        unescape str = T.strip $ T.replace (T.pack "$") (T.pack " ") str

-- | pretty print search result
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


-- | execute search in local filelist
searchInDb :: AppState -> Search -> IO [SearchResult]
searchInDb appState search = do
    let nick = configNick $ appConfig appState
    IndexedFileTree tree htable <- readMVar $ appFileTree appState
    if isTTHSearch
        then do
             node <- H.lookup htable (T.pack $ searchHash search)
             return $ map (nodeToResult nick) $ maybeToList node
        else return $ map (nodeToResult nick) $ take 5 $ filter (matchReg regex) (toList tree)
    where
        isTTHSearch = (searchDataType search) == TTH

        regex = map (toRegex.T.unpack) $ T.splitOn (T.pack " ") $ T.pack $ searchPattern search
        toRegex str = mkRegexWithOpts (escapeRegex str) False False
        escapeRegex = filter isAlphaNum

        toList :: TreeNode -> [TreeNode]
        toList node@(FileNode _ _ _ _ _) = [node]
        toList node@(DirNode _ _ children) = [node] ++ (concat $ map toList children)

        matchReg :: [Regex] -> TreeNode -> Bool
        matchReg [] node = True
        matchReg (regex:regexes) node@(FileNode name _ _ _ _)
            | isJust $ matchRegex regex (T.unpack name) = matchReg regexes node
            | otherwise = False
        matchReg (regex:regexes) node@(DirNode name _ _)
            | isJust $ matchRegex regex (T.unpack name) = matchReg regexes node
            | otherwise = False

        nodeToResult :: Nick -> TreeNode -> SearchResult
        nodeToResult nick (FileNode name path size _ hash) = SearchResult {
                  srNick = nick
                , srFile = (T.unpack name)
                , srPath = ""
                , srSize = size
                , srFreeSlots = 10
                , srTotalSlots = 10
                , srHash = T.unpack $ fromMaybe T.empty hash
                }
        nodeToResult nick (DirNode name path _) = SearchResult {
                  srNick = nick
                , srFile = ""
                , srPath = (T.unpack name)
                , srSize = 0
                , srFreeSlots = 10
                , srTotalSlots = 10
                , srHash = ""
                }



-- vim: sw=4 expandtab
