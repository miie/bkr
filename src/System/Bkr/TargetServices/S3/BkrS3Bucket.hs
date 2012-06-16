{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module System.Bkr.TargetServices.S3.BkrS3Bucket ( getBkrObjects
                                         , putBackupFile
                                         , putBkrMetaFile
                                         , deleteS3Object
                                         ) where

import System.Bkr.BkrConfig
import System.Bkr.BkrFundare
import System.Bkr.Hasher
import System.Bkr.BkrLogging
import System.Bkr.TargetServices.S3.BkrAwsConfig

import System.IO
import Network.HTTP.Conduit
import Data.IORef (newIORef, readIORef)
import Data.Monoid (mempty)
import System.FilePath.Posix (takeFileName)
import Prelude hiding (catch)
import Control.Concurrent (threadDelay)

import qualified Aws
import qualified Aws.S3 as S3
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Control.Exception as C

getBkrObjectKeys :: T.Text -> [T.Text] -> IO [T.Text]
getBkrObjectKeys gbMarker objList = do

     -- Get AWS credentials
     cfg <- getS3Config
     
     -- Create an IORef to store the response Metadata (so it is also available in case of an error).
     metadataRef <- newIORef mempty
     
     -- Get bucket info with simpleAwsRef. S3.getBucket returns a GetBucketResponse object.
     bucketName <- getS3BucketName
     -- add catch S3Error and print check aws settings. 
     s3BkrBucket <- Aws.simpleAwsRef cfg metadataRef S3.GetBucket { S3.gbBucket    = bucketName
                                                                  , S3.gbDelimiter = Nothing
                                                                  , S3.gbMarker    = Just gbMarker
                                                                  , S3.gbMaxKeys   = Nothing
                                                                  , S3.gbPrefix    = Just $ T.pack "bkrm"
                                                                  } `C.catch` \ (ex :: C.SomeException) -> do
                                                                  logCritical "Failed to get objects from S3 bucket, please check that your S3 credentials in the bkr configuration file are set correctly. The error was:"
                                                                  C.throwIO ex
     
     -- Print the response metadata.
     --print =<< readIORef metadataRef
     -- Log the response metadata.
     ioResponseMetaData <- readIORef metadataRef
     logDebug $ "getBkrObjectKeys: response metadata: " ++ show ioResponseMetaData

     -- Get bucket contents with gbrContents. gbrContents gets [ObjectInfo]
     let bkrBucketContents = S3.gbrContents s3BkrBucket
     
     -- Get object keys (the bkr object filenames)
     let objects = map S3.objectKey bkrBucketContents

     -- S3 is limited to fetch 1000 objects so make sure that we get all objects
     if length objects > 999
        then getBkrObjectKeys (last objects) (objList ++ objects)
        else return $ objList ++ objects

getBkrObjects :: IO [BkrMeta]
getBkrObjects = do
          
     objectKeys <- getBkrObjectKeys (T.pack "") []
     logNotice $ "Got " ++ show (length objectKeys) ++ " objects from S3"

     return $ map getMetaKeys objectKeys

getMetaKeys :: T.Text -> BkrMeta
getMetaKeys key = BkrMeta { fullPath                 = "" 
                          , pathChecksum             = T.unpack $ kSplit !! 1
                          , fileChecksum             = T.unpack $ kSplit !! 2
                          , modificationTime         = ""
                          , modificationTimeChecksum = ""
                          }
                          where kSplit = T.split (=='.') key

putBackupFile :: FilePath -> IO ()
putBackupFile filePath = do
        
     let uploadName = T.pack $ show (getHashForString filePath) ++ "::" ++ takeFileName filePath
     -- Get MD5 hash for file
     --contentMD5 <- getFileHash path
     --putFile path uploadName (Just $ BUTF8.fromString $ show contentMD5)
     putFile filePath uploadName Nothing 0

putBkrMetaFile :: FilePath -> IO ()
putBkrMetaFile filePath = do

     let uploadName = T.pack $ "bkrm." ++ takeFileName filePath
     -- Get MD5 hash for file
     --contentMD5 <- getFileHash path
     --putFile path uploadName (Just $ BUTF8.fromString $ show contentMD5)
     putFile filePath uploadName Nothing 0

{-| Upload file to S3. putFile will handle a failed attempt to upload the file by waiting 60 seconds and then retrying. If this fails five times it will raise an IO Error. -}
putFile :: FilePath -> T.Text -> Maybe B.ByteString -> Int -> IO ()
putFile filePath uploadName contentMD5 noOfRetries =
     putFile' filePath uploadName contentMD5 `C.catch` \ (ex :: C.SomeException) ->
              if noOfRetries > 5
                 then ioError $ userError $ "Failed to upload file " ++ filePath
                 else do
                      logCritical $ "putFile: got exception: " ++ show ex
                      logCritical "Wait 60 sec then try again"
                      threadDelay $ 60 * 1000000
                      putFile filePath uploadName contentMD5 (noOfRetries + 1)

putFile' :: FilePath -> T.Text -> Maybe B.ByteString -> IO ()
putFile' filePath uploadName contentMD5 = do
     
     -- Get S3 config
     cfg <- getS3Config

     -- Create an IORef to store the response Metadata (so it is also available in case of an error).
     metadataRef <- newIORef mempty
     
     -- TODO: change to read the file lazy and upload using RequestBodyLBS ...or maybe no, we probably don't gain anything from doing this lazy
     --hndl <- openBinaryFile path ReadMode
     --fileContents <- LB.hGetContents hndl
     --Aws.simpleAwsRef cfg metadataRef $ S3.putObject uploadName getS3BucketName (RequestBodyLBS $ fileContents)
     fileContents <- B.readFile filePath

     -- Get bucket name
     bucketName <- getS3BucketName
     
     -- Check if we should use reduced redundancy
     useReducedRedundancy <- getUseS3ReducedRedundancy
     
     -- Replace space with underscore in the upload name (S3 does not handle blanks in object names). Doing this is safe since the whole original path is stored in the meta file.
     logDebug ("putFile: will upload file " ++ filePath)
     _ <- Aws.simpleAwsRef cfg metadataRef S3.PutObject { S3.poObjectName          = T.replace " " "_" uploadName 
                                                        , S3.poBucket              = bucketName
                                                        , S3.poContentType         = Nothing
                                                        , S3.poCacheControl        = Nothing
                                                        , S3.poContentDisposition  = Nothing
                                                        , S3.poContentEncoding     = Nothing
                                                        , S3.poContentMD5          = contentMD5
                                                        , S3.poExpires             = Nothing
                                                        , S3.poAcl                 = Nothing
                                                        , S3.poStorageClass        = useReducedRedundancy
                                                        , S3.poRequestBody         = RequestBodyBS fileContents 
                                                        , S3.poMetadata            = []
                                                        }
     logDebug "putFile: upload done"

     -- If lazy upload, close the handle
     --logDebug "putFile: close file handle"
     --hClose hndl
     
     -- Log the response metadata.
     --ioResponseMetaData <- readIORef metadataRef
     --logDebug $ "putFile: response metadata: " ++ (show ioResponseMetaData)
     readIORef metadataRef >>= logDebug . ("putFile: response metadata: " ++) . show

deleteS3Object :: T.Text -> IO ()
deleteS3Object objectName = do
     
     -- Get S3 config
     cfg <- getS3Config

     -- Create an IORef to store the response Metadata (so it is also available in case of an error).
     metadataRef <- newIORef mempty
     
     -- Get bucket name
     bucketName <- getS3BucketName
     
     -- Delete the object
     logDebug ("deleteObject: will delete object " ++ (show objectName))
     _ <- Aws.simpleAwsRef cfg metadataRef S3.DeleteObject { S3.doObjectName = objectName
                                                           , S3.doBucket     = bucketName
                                                           }

     logDebug "deleteObject: delete done"
     readIORef metadataRef >>= logDebug . ("deleteObject: response metadata: " ++) . show
