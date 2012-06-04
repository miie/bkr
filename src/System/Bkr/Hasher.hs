

module System.Bkr.Hasher ( getFileHash
                  , getHashForString
                  ) where

import Data.Digest.Pure.MD5 (md5, MD5Digest)

import qualified Data.ByteString.Lazy.UTF8 as UBL
import qualified Data.ByteString.Lazy as BL

{-| Gets a MD5Digest for a file. -}
getFileHash :: FilePath -> IO MD5Digest
getFileHash path = do
     --L.readFile path >>= return . md5
     
     readF <- BL.readFile path
     return $! md5 readF

{-| Gets a MD5Digest for a String. Example:

@
print $ show $ getHashForString \"sdknafsfadsäöåfas\"
@
-}
getHashForString :: String -> MD5Digest
getHashForString = md5 . UBL.fromString
