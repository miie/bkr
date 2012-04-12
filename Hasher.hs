
module Hasher ( getFileHash
              , getHashForString
              ) where

import Data.Digest.Pure.MD5 (md5, MD5Digest)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as B

{-| Gets a MD5Digest for a file. |-}
getFileHash :: FilePath -> IO MD5Digest
getFileHash path = L.readFile path >>= return . md5

{-| Gets a MD5Digest for a String. Example:
@
print $ show $ getHashForString "sdknafsfadsäöåfas"
@
|-}
getHashForString :: String -> MD5Digest
getHashForString str = md5 $ B.fromString str