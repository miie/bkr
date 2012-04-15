
module BkrFundare ( BkrMeta(..)
                    --,
                  ) where

data BkrMeta = BkrMeta          { fullPath :: FilePath
                                , checksum :: String
                                }         
             | BkrMetaCheck     { pathChecksum :: String
                                , fileChecksum :: String
                                } deriving (Show, Eq)