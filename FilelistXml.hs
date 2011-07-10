module FilelistXml (
    parseXml
    ) where

import Data.Attoparsec as P
import qualified Data.Attoparsec.Lazy as LP
import qualified Data.Attoparsec.Char8 as P8
import Data.Attoparsec.Char8 (char, char8, endOfLine, isDigit_w8)
import Control.Applicative hiding (many)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Control.Monad (liftM)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Attoparsec.Enumerator
import qualified Data.Enumerator as E

import FilelistTypes


-- | xml definition line, will be ignored
xmlDefTag :: Parser ()
xmlDefTag = P.string (B.pack "<?xml") *> P8.skipWhile (/='>') <* P8.char '>' <?> "xmlDefTag"


-- | read on "token" like tagname, attribute name, ...
takeToken :: Parser B.ByteString
takeToken = P.takeWhile (inClass "a-zA-Z0-9_") <?> "takeToken"


-- | read a quoted string like attribute value
quotedString :: Parser B.ByteString
quotedString = P8.char '"' *> P8.takeWhile (/= '"') <* P8.char '"'


-- | read tag
xmlTag :: String     -- name of tag
       -> Parser a   -- parser vor inner content
       -> Parser ([(B.ByteString, B.ByteString)], Maybe a) -- return attributes and result of inner content
xmlTag name subparser = do
    P8.skipSpace
    (P8.stringCI $ B.pack ('<':name)) <?> "tag"
    !attrs <- P.many xmlAttr
    P8.skipSpace
    !inner <- (singleTag *> pure Nothing) <|> (Just `liftM` (enclosingTag <?> "enclode"))
    return $! (attrs, inner)
    where
        singleTag = P.string (B.pack "/>")
	enclosingTag = (P8.char '>' <?> "close") *> P8.skipSpace *> (subparser <?>"sub") <* (P8.stringCI (B.pack "</")<?>"endtag") <* P8.stringCI (B.pack name) <* P8.char '>'


-- | parse one attribute
xmlAttr :: Parser (B.ByteString, B.ByteString)
xmlAttr = do
    P8.skipSpace
    !name <- takeToken
    P8.skipSpace
    P8.char '='
    P8.skipSpace
    !value <- quotedString <|> takeToken
    return $! (name, value)


-- | parse main filelisting tag
filelistingTag :: Parser [TreeNode]
filelistingTag = (fromJust . snd) `liftM` xmlTag "FileListing" (P.many nodeTag <?> "test") <?> "filelistingTag"


-- | parse one node
nodeTag :: Parser TreeNode
nodeTag = do
    P8.skipSpace
    (fileNodeTag <?> "fileNode") <|> (dirNodeTag <?> "dirNode")


-- | parse a file node
fileNodeTag :: Parser TreeNode
fileNodeTag = do
    (!attrs, _) <- xmlTag "File" (return ())
    let !size = read $ B.unpack $ fromJust $ lookup (B.pack "Size") attrs
    let !name = decodeUtf8 $ fromJust $ lookup (B.pack "Name") attrs
    let !tth = decodeUtf8 `liftM` lookup (B.pack "TTH") attrs
    P8.skipSpace
    return $! FileNode name T.empty size 0 tth


-- | parse a directory node
dirNodeTag :: Parser TreeNode
dirNodeTag = do
    (!attrs, !(Just children)) <- xmlTag "Directory" (P.many nodeTag)
    let !name = decodeUtf8 $ fromJust $ lookup (B.pack "Name") attrs
    P8.skipSpace
    return $! DirNode name T.empty children


-- | parse whole filelist xml file
wholeXml :: Parser [TreeNode]
wholeXml = P8.skipSpace *> xmlDefTag *> filelistingTag <?> "filelist.xml"

-- | call filelist parser
parseXml :: L.ByteString -> [TreeNode]
--parseXml xml = fromJust $ LP.maybeResult $ LP.parse wholeXml xml
parseXml xml = fromJust `liftM` E.run_ $ enumLBS xml E.$$ iterParser wholeXml


-- | Enumerates a lazy bytestring.
enumLBS :: (Monad m) => L.ByteString -> E.Enumerator B.ByteString m a
enumLBS bs = E.enumList 1 (L.toChunks bs)
{-# INLINE enumLBS #-}


