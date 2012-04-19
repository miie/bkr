
module BkrFundare ( BkrMeta(..)
                    --,
                  ) where

{-
data BkrMeta = BkrMeta          { fullPath :: FilePath
                                , checksum :: String
                                }         
             | BkrMetaCheck     { pathChecksum :: String
                                , fileChecksum :: String
                                } deriving (Show, Eq)
-}

data BkrMeta = BkrMeta { fullPath :: FilePath
                       , fileChecksum :: String
                       , pathChecksum :: String
                       } deriving (Show)

instance Eq BkrMeta where
     meta1 == meta2 = fileChecksum meta1 == fileChecksum meta2 && pathChecksum meta1 == pathChecksum meta2