{-# LANGUAGE OverloadedStrings #-}

module BkrAwsConfig ( getS3Config
                    , getS3BucketName
                    ) where

-- Credentials
import Aws.Aws
import Aws.Credentials
import qualified Data.ByteString.Char8 as B

-- Configuration
import           Aws.Http
import           Aws.S3.Info
import           Aws.Ses.Info
import           Aws.Signature
import           Aws.SimpleDb.Info
import           Aws.Sqs.Info

import qualified Data.Text as T

import BkrConfig
import Data.Maybe (fromJust)

getS3Config :: IO Configuration
getS3Config = do
     -- Get AWS access and secret keys from bkr.conf
     confPairs <- getConfPairsFromFileS' "bkr.conf"
     let cr = Credentials (B.pack $ fromJust $ lookup "awsaccesskeyid" confPairs) (B.pack $ fromJust $ lookup "awssecretaccesskey" confPairs)
     
     return Configuration { timeInfo = Timestamp
                          , credentials = cr
                          , sdbInfo = sdbHttpsPost sdbUsEast
                          , sdbInfoUri = sdbHttpsGet sdbUsEast
                          , s3Info = s3 HTTP s3EndpointUsClassic False
                          , s3InfoUri = s3 HTTP s3EndpointUsClassic True
                          , sqsInfo = sqs HTTP sqsEndpointUsClassic False
                          , sqsInfoUri = sqs HTTP sqsEndpointUsClassic True
                          , sesInfo = sesHttpsPost sesUsEast
                          , sesInfoUri = sesHttpsGet sesUsEast
                          , logger = defaultLog Warning
                          }

getS3BucketName :: IO T.Text
getS3BucketName = do
     confPairs <- getConfPairsFromFileS' "bkr.conf"
     return $ T.pack $ fromJust $ lookup "s3bucketname" confPairs