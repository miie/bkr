

module Bkr.Hasher ( getFileHash
                  , getHashForString
                  --, getFileHash'
                  ) where

import qualified Data.ByteString.Lazy.UTF8 as UB
import qualified Data.ByteString.Lazy as B

import Data.Digest.Pure.MD5 (md5, MD5Digest)

--import qualified Crypto.Hash.MD5 as CH
--import qualified Data.ByteString.UTF8 as SB
--import System.IO.Strict (SIO)
--import qualified System.IO.Strict as SIO

--import Control.Monad (mapM)

{-| Gets a MD5Digest for a file. |-}
getFileHash :: FilePath -> IO MD5Digest
getFileHash path = do
     --print "Getting Hash"
     --L.readFile path >>= return . md5
     readF <- B.readFile path
     return $! md5 readF

-- Just testing...
{-getFileHash' paths = do
     --print "Getting Hash"
     --L.readFile path >>= return . md5
     allFiles <- mapM L.readFile paths
     return $ map md5 allFiles 
-}

{-| Gets a MD5Digest for a String. Example:
@
print $ show $ getHashForString "sdknafsfadsäöåfas"
@
|-}
getHashForString :: String -> MD5Digest
getHashForString str = md5 $ UB.fromString str
