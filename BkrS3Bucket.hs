{-# LANGUAGE OverloadedStrings #-}

module BkrS3Bucket ( getBkrObjects
                   , putFile
                   , putFile'
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
import qualified Data.ByteString.Lazy as LB
import System.FilePath.Posix (takeFileName)

getBkrObjects :: IO [BkrMeta]
getBkrObjects = do
     
     -- Get AWS credentials
     let cfg = getS3Config
     
     -- Create an IORef to store the response Metadata (so it is also available in case of an error).
     metadataRef <- newIORef mempty
     
     -- Get bucket info with simpleAwsRef. S3.getBucket returns a GetBucketResponse object.
     s3BkrBucket <- Aws.simpleAwsRef cfg metadataRef $ S3.getBucket getS3BucketName
     --print $ show bucket
     --print $ show $ S3.gbrContents bucket
     
     -- Print the response metadata.
     --print =<< readIORef metadataRef

     -- Get bucket contents with gbrContents. gbrContents gets [ObjectInfo]
     let bkrBucketContents = S3.gbrContents s3BkrBucket
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
     
     bkrObjects <- getBkrObject bkrObjectFiles
     return bkrObjects

{-| Filter function for filtering .bkrm objects (files). |-}
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
     let cfg = getS3Config

     -- Create an IORef to store the response Metadata (so it is also available in case of an error).
     metadataRef <- newIORef mempty

     -- Get tmp dir
     tmpDir <- getTemporaryDirectory

     objects <- forM objNames $ \fileName -> do
        -- Get tmp file path and handle
        (tmpPath, hndl) <- openBinaryTempFileWithDefaultPermissions tmpDir "tmp.bkrm"
        -- Get knob object and knob handle (knob is a in-memory virtual file) 
        knob <- K.newKnob (pack [])
        knobHndl <- K.newFileHandle knob "test.txt" WriteMode
        -- Get the object (.bkrm sfile)
        Aws.simpleAwsRef cfg metadataRef $ S3.getObject getS3BucketName fileName (saveObject $ return knobHndl)
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
     let cfg = getS3Config

     -- Create an IORef to store the response Metadata (so it is also available in case of an error).
     metadataRef <- newIORef mempty

     -- Get tmp dir
     tmpDir <- getTemporaryDirectory

     objects <- forM fileNames $ \fileName -> do
        -- Get tmp file path and handle
        (tmpPath, hndl) <- openBinaryTempFileWithDefaultPermissions tmpDir "tmp.bkrm"
        -- Get the object (.bkrm sfile)
        Aws.simpleAwsRef cfg metadataRef $ S3.getObject getS3BucketName fileName (saveObject $ return hndl)
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

--putFile :: FilePath -> IO [BkrMeta]
putFile path = do

     -- Get S3 config
     let cfg = getS3Config

     -- Create an IORef to store the response Metadata (so it is also available in case of an error).
     metadataRef <- newIORef mempty
     
     --hndl <- openBinaryFile path ReadMode
     --fileContents <- LB.hGetContents hndl
     let uploadName = T.pack $ (show $ getHashForString path) ++ "::" ++ takeFileName path
     fileContents <- B.readFile path
     -- TODO: change to read the file lazy and upload using RequestBodyLBS
     --Aws.simpleAwsRef cfg metadataRef $ S3.putObject uploadName getS3BucketName (RequestBodyLBS $ fileContents)
     Aws.simpleAwsRef cfg metadataRef $ S3.putObject uploadName getS3BucketName (RequestBodyBS fileContents)

     --hClose hndl
     
     -- Print the response metadata.
     --print =<< readIORef metadataRef
     --print "done"
     return ()

putFile' path = do

     -- Get S3 config
     let cfg = getS3Config

     -- Create an IORef to store the response Metadata (so it is also available in case of an error).
     metadataRef <- newIORef mempty
     
     --hndl <- openBinaryFile path ReadMode
     --fileContents <- LB.hGetContents hndl
     let uploadName = T.pack $ takeFileName path
     fileContents <- B.readFile path
     -- TODO: change to read the file lazy and upload using RequestBodyLBS
     --Aws.simpleAwsRef cfg metadataRef $ S3.putObject uploadName getS3BucketName (RequestBodyLBS $ fileContents)
     Aws.simpleAwsRef cfg metadataRef $ S3.putObject uploadName getS3BucketName (RequestBodyBS fileContents)

     --hClose hndl
     
     -- Print the response metadata.
     --print =<< readIORef metadataRef
     --print "done"
     return ()
