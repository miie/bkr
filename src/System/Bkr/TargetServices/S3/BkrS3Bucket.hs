{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module System.Bkr.TargetServices.S3.BkrS3Bucket ( getBkrObjects
                                                , putBackupFile
                                                , putBkrMetaFile
                                                , deleteBackedFile
                                                --, getFileNameForPut
                                                ) where

import System.Bkr.BkrConfig (getUseS3ReducedRedundancy, getConfPairsFromByteString')
import System.Bkr.BkrFundare (BkrMeta(..))
import System.Bkr.Hasher (getHashForString, getFileHash')
import System.Bkr.BkrLogging (logCritical, logDebug, logNotice)
import System.Bkr.TargetServices.S3.BkrAwsConfig (getS3Config, getS3BucketName)

import System.IO (IOMode(..), Handle, hClose)
import Network.HTTP.Conduit (RequestBody(RequestBodyBS))
import Data.IORef (newIORef, readIORef)
import Data.Monoid (mempty)
import System.FilePath.Posix (takeFileName)
import Prelude hiding (catch)
import Control.Concurrent (threadDelay)
import Data.Maybe (fromJust)
import Data.Conduit (($$))
import Data.Conduit.Binary (sinkIOHandle)
--import Codec.Binary.UTF8.String (decode)

import qualified Aws
import qualified Aws.S3 as S3
import qualified Data.Text as T
import qualified Data.ByteString as B
--import qualified Data.ByteString.UTF8 as BU
--import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Base64 as BB64
--import qualified Data.Text.Encoding as TE
import qualified Control.Exception as C
import qualified Data.Knob as K

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
                                                                  } `C.catch` \ (ex :: S3.S3Error) -> do
                                                                  logCritical "Failed to get objects from S3 bucket, please check that your S3 credentials in the bkr configuration file are set correctly. The error was:"
                                                                  handleS3Error ex
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

{-| TODO: add more doc...

Replace space with underscore in the upload name (S3 does not handle blanks in object names). Doing this is safe since the whole original path is stored in the meta file. -}
getFileNameForPut :: FilePath -> T.Text
getFileNameForPut filePath = T.replace " " "_" $ T.pack $ show (getHashForString filePath) ++ "::" ++ takeFileName filePath

putBackupFile :: FilePath -> IO ()
putBackupFile filePath = do
        
     --let uploadName = T.pack $ show (getHashForString filePath) ++ "::" ++ takeFileName filePath
     
     -- Get MD5 hash for file
     contentMD5 <- getFileHash' filePath
     
     -- Upload the file, the MD5 hash needs to be base64 encoded (S3 requirement)
     putFile filePath (getFileNameForPut filePath) (Just $ BB64.encode contentMD5) 0
     --putFile filePath (getFileNameForPut filePath) Nothing 0

putBkrMetaFile :: FilePath -> IO ()
putBkrMetaFile filePath = do

     let uploadName = T.pack $ takeFileName filePath
     -- Get MD5 hash for file
     contentMD5 <- getFileHash' filePath
     
     -- Upload the file, the MD5 hash needs to be base64 encoded (S3 requirement)
     putFile filePath uploadName (Just $ BB64.encode contentMD5) 0
     --putFile filePath uploadName Nothing 0

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

deleteBackedFile :: T.Text -> IO ()
deleteBackedFile bkrmFileName = do

     -- Get BkrMeta from the backed upped bkrm file
     bkrMeta <- getBkrObject bkrmFileName

     -- Delete the backed file and the bkrm file
     _ <- mapM deleteS3Object [(getFileNameForPut $ fullPath bkrMeta), bkrmFileName]
     return ()

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

{-| A small function to save the object's data into a file handle. -}
saveObject :: IO Handle -> Aws.HTTPResponseConsumer ()
saveObject hndl _ _ source = source $$ sinkIOHandle hndl

{-| Takes the name of a backed upped bkrm file and returns the corresponding BkrMeta object. Example of a bkrm file: bkrm.ae15567db73c2bc31a8c4140098ef49b.19b6056f332ecc693ffdee66dbe33896.

This function uses the Knob package for in-memory temporary storage of the downloaded bkrm file.
-}
getBkrObject :: T.Text -> IO BkrMeta
getBkrObject bkrmFileName = do
     
     -- Get S3 config
     cfg <- getS3Config

     -- Create an IORef to store the response Metadata (so it is also available in case of an error).
     metadataRef <- newIORef mempty 
     
     -- Get knob object and knob handle (knob is a in-memory virtual file) 
     knob <- K.newKnob (B.pack [])
     knobHndl <- K.newFileHandle knob "tmp.knob" WriteMode
     
     -- Get the .bkrm file
     bucketName <- getS3BucketName
     _ <- Aws.simpleAwsRef cfg metadataRef $ S3.getObject bucketName bkrmFileName (saveObject $ return knobHndl)
     -- Get data (text) from the knob virtual file
     knobDataContents <- K.getContents knob
     -- Close knob
     hClose knobHndl
     -- Get the config pair and get path and checksum from the pair
     pairS <- getConfPairsFromByteString' knobDataContents
     let path_                     = fromJust $ lookup "fullpath" pairS
     let checksum_                 = fromJust $ lookup "checksum" pairS
     let modificationTime_         = fromJust $ lookup "modificationtime" pairS
     let modificationTimeChecksum_ = fromJust $ lookup "modificationtimechecksum" pairS
     return $ BkrMeta path_ checksum_ (show $ getHashForString path_) modificationTime_ modificationTimeChecksum_

handleS3Error :: S3.S3Error -> IO ()
handleS3Error err = do

     case show $ S3.s3ErrorCode err of
          "\"InvalidAccessKeyId\"" -> logNotice "handleS3Error: invalid access key id"
          _                        -> logCritical $ "unknown error: " ++ show err