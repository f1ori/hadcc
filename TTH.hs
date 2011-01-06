module TTH 
    (
      getHashForFile
    ) where

import Data.Digest.TigerHash
import Data.Digest.TigerHash.ByteString
import qualified Data.ByteString.Lazy as L
import System
import Control.Monad


getHashForFile :: FilePath -> IO String
getHashForFile path = do
    content <- L.readFile path
    return (b32TigerHash (tigerTreeHash content))

-- test with runhaskell tth.hs tth file.ext
--main = liftM (!!1) getArgs >>= getHashForFile >>= putStrLn
