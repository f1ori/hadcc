module HtmlTemplates where

import Text.Printf

nicklistToHtml :: [String] -> String
nicklistToHtml list = "<table><tr><th>Nick</th></tr>" ++ table ++ "</table>"
    where
        table = concat $ map (\n -> printf "<tr><td onclick=\"openNick('%s');\">%s</td></tr>" (htmlquote n) (htmlquote n)) list


htmlquote :: String -> String
htmlquote []       = []
htmlquote ('&':xs) = "&amp;" ++ (htmlquote xs)
htmlquote ('<':xs) = "&lt;" ++ (htmlquote xs)
htmlquote ('>':xs) = "&gt;" ++ (htmlquote xs)
htmlquote (x:xs)   = x : (htmlquote xs)
