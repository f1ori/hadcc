module Json where

import Data.List

-- | quote string for javascript/json
jsquote :: String -> String
jsquote str = "'" ++ (quote str) ++ "'"
    where
        quote [] = []
        quote ('\r':'\n':xs) = '\\' : 'n' : (quote xs)
        quote ('\n':xs) = '\\' : 'n' : (quote xs)
        quote ('\'':xs) = '\\' : '\'' : (quote xs)
        quote ('\"':xs) = '\\' : '\"' : (quote xs)
        quote ('\\':xs) = '\\' : '\\' : (quote xs)
        quote (x:xs) = x : (quote xs)

-- | convert haskell list to json list
listToJson :: [String] -> String
listToJson list = "[" ++ intercalate "," list ++ "]"

-- | convert haskell lookup-list into json object (don't forget to quote the strings)
objToJson :: [(String, String)] -> String
objToJson list = "{" ++ intercalate "," (toJson list) ++ "}\n"
    where
        toJson [] = []
        toJson ((x1, x2) : xs) = (x1 ++ ":" ++ x2) : (toJson xs)

