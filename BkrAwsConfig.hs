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

getS3Config :: Configuration
getS3Config = do
  let cr = Credentials (B.pack "AKIAJNREVFV4JQNNNQRQ") (B.pack "dqQmkG7q4TwJemnEoqVeSppTuOcMp6RoBRZCpibp")
  Configuration { timeInfo = Timestamp
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

getS3BucketName :: T.Text
getS3BucketName = T.pack "ms-bkr"