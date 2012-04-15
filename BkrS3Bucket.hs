{-# LANGUAGE OverloadedStrings #-}

module BkrS3Bucket ( getBkrObjects
                   , putBackupFile
                   , putBkrMetaFile
                   ) where

import qualified Aws
import qualified Aws.S3 as S3
import Data.Conduit (($$))
import Data.Conduit.Binary (sinkIOHandle, sourceIOHandle)
import Data.IORef (newIORef)
import Data.Monoid (mempty)

import Maybe (fromJust)
import Control.Monad (forM)

import System.IO
import System.Directory (getTemporaryDirectory, removeFile)
import qualified Data.Knob as K
import Data.ByteString (pack)

import qualified Data.Text as T

import BkrAwsConfig
import BkrConfig
import BkrFundare
import Hasher

import Network.HTTP.Conduit
--import qualified Data.ByteString.Lazy.UTF8 as B
--import System.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BUTF8
import qualified Data.ByteString.Lazy as LB
import System.FilePath.Posix (takeFileName)

getBkrObjectKeys :: T.Text -> [T.Text] -> IO [T.Text]
getBkrObjectKeys gbMarker objList = do

     -- Get AWS credentials
     cfg <- getS3Config
     
     -- Create an IORef to store the response Metadata (so it is also available in case of an error).
     metadataRef <- newIORef mempty
     
     -- Get bucket info with simpleAwsRef. S3.getBucket returns a GetBucketResponse object.
     bucketName <- getS3BucketName
     --s3BkrBucket <- Aws.simpleAwsRef cfg metadataRef $ S3.GetBucket bucketName Nothing (Just gbMarker) Nothing (Just $ T.pack "bkrm")
     s3BkrBucket <- Aws.simpleAwsRef cfg metadataRef S3.GetBucket { S3.gbBucket    = bucketName
                                                                  , S3.gbDelimiter = Nothing
                                                                  , S3.gbMarker    = Just gbMarker
                                                                  , S3.gbMaxKeys   = Nothing
                                                                  , S3.gbPrefix    = Just $ T.pack "bkrm"
                                                                  }
     
     -- Print the response metadata.
     --print =<< readIORef metadataRef

     -- Get bucket contents with gbrContents. gbrContents gets [ObjectInfo]
     let bkrBucketContents = S3.gbrContents s3BkrBucket
     print $ show $ length bkrBucketContents
     --print $ show contents
     
     -- Get object keys (the bkr object filenames)
     let objects = map S3.objectKey bkrBucketContents
     -- S3 is limited to fetch 1000 objects so make sure 
     if (length objects) > 999
        then getBkrObjectKeys (last objects) (objList ++ objects)
        else return $ objList ++ objects

getBkrObjects :: IO [BkrMeta]
getBkrObjects = do
          
     objectKeys <- getBkrObjectKeys (T.pack "") []
     bkrObjects <- getBkrObject objectKeys
     return bkrObjects

{-| Deprecated -}
getBkrObjectsOld :: IO [BkrMeta]
getBkrObjectsOld = do

     -- Get AWS credentials
     cfg <- getS3Config
     
     -- Create an IORef to store the response Metadata (so it is also available in case of an error).
     metadataRef <- newIORef mempty
     
     -- Get bucket info with simpleAwsRef. S3.getBucket returns a GetBucketResponse object.
     bucketName <- getS3BucketName
     s3BkrBucket <- Aws.simpleAwsRef cfg metadataRef $ S3.getBucket bucketName
     --print $ show bucket
     --print $ show $ S3.gbrContents bucket
     
     -- Print the response metadata.
     --print =<< readIORef metadataRef

     -- Get bucket contents with gbrContents. gbrContents gets [ObjectInfo]
     let bkrBucketContents = S3.gbrContents s3BkrBucket
     print $ show $ length bkrBucketContents
     --print $ show contents
     --print $ show $ S3.objectKey $ contents !! 0
     
     -- Get object keys (the bkr object filenames)
     let objectKeys = map S3.objectKey bkrBucketContents
     --let t0 = Prelude.head t
     --print t0
     --let t1 = Prelude.last $ T.split (=='.') t0
     --print t1
     
     -- Filter the object keys for Bkr meta (.bkrm) objects (files)
     let bkrObjectFiles = filter (\x -> hasBkrExtension x) objectKeys
     --print "bkrObjectFiles: "
     --print bkrObjectFiles
     --print $ show $ length bkrObjectFiles
     
     bkrObjects <- getBkrObject objectKeys
     return bkrObjects

{-| Deprecated. Filter function for filtering .bkrm objects (files). |-}
hasBkrExtension :: T.Text -> Bool
hasBkrExtension t = do
     if (Prelude.last $ T.split (=='.') t) == "bkrm"
        then True
        else False

{-| A small function to save the object's data into a file handle. |-}
saveObject :: IO Handle -> Aws.HTTPResponseConsumer ()
saveObject hndl status headers source = source $$ sinkIOHandle hndl

{-| Takes a list of bkr objects, gets them one by one from S3, parses content creating and returning a list of BkrObject's. This function uses the Knob package for in-memory temporary storage of the downloaded bkr object. |-}
getBkrObject :: [T.Text] -> IO [BkrMeta]
getBkrObject objNames = do

     -- Get S3 config
     cfg <- getS3Config

     -- Create an IORef to store the response Metadata (so it is also available in case of an error).
     metadataRef <- newIORef mempty

     -- Get tmp dir
     tmpDir <- getTemporaryDirectory

     objects <- forM objNames $ \fileName -> do   
        -- Get knob object and knob handle (knob is a in-memory virtual file) 
        knob <- K.newKnob (pack [])
        knobHndl <- K.newFileHandle knob "test.txt" WriteMode
        -- Get the object (.bkrm sfile)
        bucketName <- getS3BucketName
        Aws.simpleAwsRef cfg metadataRef $ S3.getObject bucketName fileName (saveObject $ return knobHndl)
        -- Get data (text) from the knob virtual file        
        knobDataContents <- K.getContents knob
        -- Close knob
        hClose knobHndl
        -- Get the config pair and get path and checksum from the pair
        pairS <- getConfPairsFromByteString' knobDataContents
        let path = fromJust $ lookup "fullpath" pairS
        let checksum = fromJust $ lookup "checksum" pairS

        return [BkrMeta path checksum]
     return (concat objects)

{-| Like getBkrObject but uses a temporary file instead of a virtual file when fetching and reading the bkr object files. |-}
getBkrObject' :: [T.Text] -> IO [BkrMeta]
getBkrObject' fileNames = do

     -- Get S3 config
     cfg <- getS3Config

     -- Create an IORef to store the response Metadata (so it is also available in case of an error).
     metadataRef <- newIORef mempty

     -- Get tmp dir
     tmpDir <- getTemporaryDirectory

     objects <- forM fileNames $ \fileName -> do
        -- Get tmp file path and handle
        (tmpPath, hndl) <- openBinaryTempFileWithDefaultPermissions tmpDir "tmp.bkrm"
        -- Get the object (.bkrm sfile)
        bucketName <- getS3BucketName
        Aws.simpleAwsRef cfg metadataRef $ S3.getObject bucketName fileName (saveObject $ return hndl)
        -- Get a new handle to the tmp file, read it and get the path and checksum
        hndl <- openBinaryFile tmpPath ReadMode
        -- Get the conf pair from the tmp file and get path and checksum from the pair
        pairsS <- getConfPairsFromFileS' tmpPath
        let path = fromJust $ lookup "fullpath" pairsS
        let checksum = fromJust $ lookup "checksum" pairsS
        -- Close the handle and delete the tmp file
        hClose hndl
        removeFile tmpPath

        return [BkrMeta path checksum]
     return (concat objects)

--getObject hndl status headers source = source $$ sourceIOHandle hndl

putBackupFile :: FilePath -> IO ()
putBackupFile path = do
        
     let uploadName = T.pack $ (show $ getHashForString path) ++ "::" ++ takeFileName path
     -- Get MD5 hash for file
     --contentMD5 <- getFileHash path
     --putFile path uploadName (Just $ BUTF8.fromString $ show contentMD5)
     putFile path uploadName Nothing

putBkrMetaFile :: FilePath -> IO ()
putBkrMetaFile path = do

     let uploadName = T.pack $ "bkrm." ++ (takeFileName path)
     -- Get MD5 hash for file
     --contentMD5 <- getFileHash path
     --putFile path uploadName (Just $ BUTF8.fromString $ show contentMD5)
     putFile path uploadName Nothing

putFile :: FilePath -> T.Text -> Maybe B.ByteString -> IO ()
putFile path uploadName contentMD5 = do
     
     -- Get S3 config
     cfg <- getS3Config

     -- Create an IORef to store the response Metadata (so it is also available in case of an error).
     metadataRef <- newIORef mempty
     
     --hndl <- openBinaryFile path ReadMode
     --fileContents <- LB.hGetContents hndl
     fileContents <- B.readFile path
     -- TODO: change to read the file lazy and upload using RequestBodyLBS
     --Aws.simpleAwsRef cfg metadataRef $ S3.putObject uploadName getS3BucketName (RequestBodyLBS $ fileContents)
     bucketName <- getS3BucketName
     --Aws.simpleAwsRef cfg metadataRef $ S3.putObject uploadName bucketName (RequestBodyBS fileContents)
     -- Use reduced redudancy TODO: make this a config setting!
     --Aws.simpleAwsRef cfg metadataRef $ (S3.PutObject uploadName bucketName Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just S3.ReducedRedundancy) (RequestBodyBS fileContents) [])
     Aws.simpleAwsRef cfg metadataRef S3.PutObject { S3.poObjectName          = uploadName
                                                   , S3.poBucket              = bucketName
                                                   , S3.poContentType         = Nothing
                                                   , S3.poCacheControl        = Nothing
                                                   , S3.poContentDisposition  = Nothing
                                                   , S3.poContentEncoding     = Nothing
                                                   , S3.poContentMD5          = contentMD5
                                                   , S3.poExpires             = Nothing
                                                   , S3.poAcl                 = Nothing
                                                   , S3.poStorageClass        = Just S3.ReducedRedundancy
                                                   , S3.poRequestBody         = RequestBodyBS fileContents 
                                                   , S3.poMetadata            = []
                                                   }

     --hClose hndl
     
     -- Print the response metadata.
     --print =<< readIORef metadataRef
     --print "done"
     return ()


