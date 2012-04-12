
module BkrFundare ( BkrMeta(..)
                    --,
                  ) where

data BkrMeta = BkrMeta { fullPath :: FilePath
                       , checksum :: String
                       } deriving (Show, Eq)