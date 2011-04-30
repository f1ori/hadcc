module Search where

import Text.Printf

data Search = SearchRec {
      searchSizeRestricted :: Bool
    , searchIsMaxSize :: Bool
    , searchSize :: Integer
    , searchDataType :: SearchDataType
    , searchPattern :: String
    }

data SearchDataType = Any
                    | Audio
                    | Archive
                    | Document
                    | Executable
                    | Picture
                    | Video
                    | Folder
                    | TTH

escapePattern :: String -> String
escapePattern (' ':xs) = '$':(escapePattern xs)
escapePattern ('|':xs) = '$':(escapePattern xs)
escapePattern ('?':xs) = '$':(escapePattern xs)
escapePattern (x:xs)   =  x :(escapePattern xs)


simpleSearch :: String -> Search
simpleSearch str = SearchRec {
      searchSizeRestricted = False
    , searchIsMaxSize = False
    , searchSize = 0
    , searchDataType = Any
    , searchPattern = (str)
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
searchToDC search = printf "%c?%c?%d?%s?%s"
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
