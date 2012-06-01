
module Bkr.BkrFundare ( BkrMeta(..)
                      , getBkrMeta
                      ) where

import Bkr.Hasher

import System.Directory (getModificationTime)

data BkrMeta = BkrMeta { fullPath :: FilePath
                       , fileChecksum :: String
                       , pathChecksum :: String
                       , modificationTime :: String
                       , modificationTimeChecksum :: String
                       } deriving Show

{-|
True for:
fileChecksum && pathChecksum
or
pathChecksum && modificationTimeChecksum
-}
instance Eq BkrMeta where
     meta1 == meta2 = (fileChecksum meta1 == fileChecksum meta2 && pathChecksum meta1 == pathChecksum meta2) || (pathChecksum meta1 == pathChecksum meta2 && modificationTimeChecksum meta1 == modificationTimeChecksum meta2)

{-| Gets BkrMeta for a FilePath. -}
getBkrMeta :: FilePath -> IO BkrMeta
getBkrMeta path = do
     fileHash <- getFileHash path
     modTime <- getModificationTime path
     return $ BkrMeta path (show fileHash) (show $ getHashForString path) (show modTime) (show $ getHashForString $ show modTime)
