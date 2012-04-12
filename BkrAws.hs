{-# LANGUAGE OverloadedStrings #-}

import qualified Aws
import qualified Aws.S3 as S3
import Data.Conduit
import Data.Conduit.Binary
import Data.IORef
import Data.Monoid

import BkrAwsConfig

{-
-- Credentials
import Aws.Credentials
import Aws.Aws
import qualified Data.ByteString.Char8 as B

-- Configuration
import           Aws.Http
import           Aws.S3.Info
import           Aws.Ses.Info
import           Aws.Signature
import           Aws.SimpleDb.Info
import           Aws.Sqs.Info
-}
-- A small function to save the object's data into a file.
saveObject :: Aws.HTTPResponseConsumer ()
saveObject status headers source = source $$ sinkFile "tst.txt"

listObject :: Aws.HTTPResponseConsumer ()
listObject status headers source = do
           obj <- source $$ Data.Conduit.Binary.lines
           print $ show obj

main :: IO ()
main = do
  -- Set up AWS credentials and the default configuration.
  --cfg <- baseConfiguration'
  --let cfg = baseConfiguration''
  let cfg = getS3Config

  -- Create an IORef to store the response Metadata (so it is also available in case of an error).
  metadataRef <- newIORef mempty

  -- Create a request object with S3.getObject and run the request with simpleAwsRef.
  --Aws.simpleAwsRef cfg metadataRef $ S3.getObject "haskell-aws" "cloud-remote.pdf" saveObject
  --Aws.simpleAwsRef cfg metadataRef $ S3.getObject "ms-bkr" "_DSC2283.NEF" saveObject
  bucket <- Aws.simpleAwsRef cfg metadataRef $ S3.getBucket "ms-bkr"
  print $ show bucket

  -- Print the response metadata.
  print =<< readIORef metadataRef

{-
getCredentials :: Credentials
getCredentials = Credentials (B.pack "AKIAJNREVFV4JQNNNQRQ") (B.pack "dqQmkG7q4TwJemnEoqVeSppTuOcMp6RoBRZCpibp")

baseConfiguration' :: IO Configuration
baseConfiguration' = do
  --let cr = getCredentials
  let cr = Credentials (B.pack "AKIAJNREVFV4JQNNNQRQ") (B.pack "dqQmkG7q4TwJemnEoqVeSppTuOcMp6RoBRZCpibp")
  return Configuration {
                      timeInfo = Timestamp
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

baseConfiguration'' :: Configuration
baseConfiguration'' = do
  --let cr = getCredentials
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
-}