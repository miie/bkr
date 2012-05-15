
module BkrConfig ( FileUpdateCheckType(..)
                 , getConfPairsFromFile
                 , getConfPairsFromFile'
                 , getConfPairsFromFile_
                 , getConfPairsFromFile_'
                 , getConfPairsFromFileS
                 , getConfPairsFromFileS'
                 , getConfPairsFromByteString
                 , getConfPairsFromByteString'
                 , writeBkrMetaFile
                 , getValue
                 , getValueS
                 , getBackupFolders
                 , getFilesToIgnore
                 , getFileExtensionsToIgnore
                 , getFoldersToIgnore
                 , getUseS3ReducedRedundancy
                 , getLogLevel
                 , getFileUpdateCheckType
                 ) where

import qualified Data.Text as T
--import Data.List (lines)
import System.IO
import qualified System.IO.Strict as S
import qualified Data.ByteString.Char8 as BS

import System.Directory (getTemporaryDirectory, getModificationTime)
--import System.IO
import Hasher
--import Control.Monad (mapM)
import Control.Monad (liftM)
--import List (find)

import Data.String.Utils (split, strip)
--import Data.Maybe (fromJust)
--import System.IO.Error (ioError, userError)
import BkrLogging
import Aws.S3.Model (StorageClass(..))
import System.Log.Logger (Priority(..))

data FileUpdateCheckType = FUCChecksum
                         | FUCDate
                         | FUCSmart
                         deriving Eq

{-| Read lines from s and filter on empty lines and lines beginning with # |-}
getFilteredLines :: String -> [T.Text]
getFilteredLines s = [ x | x <- map T.pack (lines s), x /= T.empty, T.head (T.stripStart x) /= '#' ]

{-| TODO: add better description!
Gets configuration pairs. This function takes a FilePath and reads the file lazy which might lead to unexpected consequences. If you want to have more control over the file handle use getConfPairsFromFile_ and if you want the file to be read strictly without the unwanted (or wanted) lazines side effects use getConfPairsFromFileS. 
|-}
getConfPairsFromFile :: FilePath -> IO [(T.Text, T.Text)]
getConfPairsFromFile path = do
     logDebug "getConfPairsFromFile called"
     -- Read the config file, split into lines and pack as Text
     hndl <- openBinaryFile path ReadMode
     readF <- hGetContents hndl
     
     return $ map getConfPair (getFilteredLines readF)

{-| Like getConfPairsFromFile but gets a String pair instead of Text. |-}
getConfPairsFromFile' :: FilePath -> IO [(String, String)]
getConfPairsFromFile' path = do
     logDebug "getConfPairsFromFile' called"
     
     pairs <- getConfPairsFromFile path
     return $ map textToString pairs

{-| Like getConfPairsFromFile but takes a file handle instead of FilePath. |-}
getConfPairsFromFile_ :: Handle -> IO [(T.Text, T.Text)]
getConfPairsFromFile_ hndl = do
     logDebug "getConfPairsFromFile_ called"
     -- Read the config file, split into lines and pack as Text
     readF <- hGetContents hndl

     return $ map getConfPair (getFilteredLines readF)

{-| Like getConfPairsFromFile' but takes a file handle instead of FilePath. |-}
getConfPairsFromFile_' :: Handle -> IO [(String, String)]
getConfPairsFromFile_' hndl = do
     logDebug "getConfPairsFromFile_' called"
     
     pairs <- getConfPairsFromFile_ hndl
     return $ map textToString pairs

{-| Like getConfPairsFromFile but reads file contents strictly. |-}
getConfPairsFromFileS :: FilePath -> IO [(T.Text, T.Text)]
getConfPairsFromFileS path = do
     logDebug "getConfPairsFromFileS called"
     -- Read the config file, split into lines and pack as Text
     hndl <- openBinaryFile path ReadMode
     -- Read contents strictly
     readF <- S.hGetContents hndl
     hClose hndl
     
     return $ map getConfPair (getFilteredLines readF)

{-| Like getConfPairsFromFile' but reads file contents strictly. |-}
getConfPairsFromFileS' :: FilePath -> IO [(String, String)]
getConfPairsFromFileS' path = do
     logDebug "getConfPairsFromFileS' called"
     pairs <- getConfPairsFromFileS path
     return $ map textToString pairs

{-| Take a ByteString text, convert to lines and return Text pairs. |-}
getConfPairsFromByteString :: BS.ByteString -> IO [(T.Text, T.Text)]
getConfPairsFromByteString bS = do
     logDebug "getConfPairsFromByteString called"
     --let fileLines = map T.pack (lines $ BS.unpack bS)
     -- Get lines and filter lines beginning with #
     let fileLines = [ x | x <- map T.pack (lines $ BS.unpack bS), x /= T.empty, T.head (T.stripStart x) /= '#' ]
     return $ map getConfPair fileLines

{-| Like getConfPairsFromByteString but returns String. |-}
getConfPairsFromByteString' :: BS.ByteString -> IO [(String, String)]
getConfPairsFromByteString' bS = do
     logDebug "getConfPairsFromByteString' called"
     pairs <- getConfPairsFromByteString bS
     return $ map textToString pairs

{-| Get a pair for Text line. |-}
getConfPair :: T.Text -> (T.Text, T.Text)
getConfPair line = (T.strip $ head s, T.strip $ last s)
                    where s = T.split (==':') line

{-| Convert a Text pair to a String pair. |-}
textToString :: (T.Text, T.Text) -> (String, String)
textToString x = (T.unpack $ fst x, T.unpack $ snd x)

{-| Gets the value for a list of value pairs. The function matches on the first key found and does not check for multiple keys with the same name. If the key is not found Nothing is returned. |-}
getValue :: T.Text -> [(T.Text, T.Text)] -> Maybe T.Text
getValue _ [] = Nothing
getValue key (x:xs) = if fst x == key
                           then Just $ snd x
                           else getValue key xs

{-| Same as getValue but for String value pairs |-}
getValueS :: String -> [(String, String)] -> Maybe String
--getValueS value values = lookup value values
getValueS = lookup

-- Bkr specific fuctions

{-| Take a bkr conf pair and write a .bkrm file in a temporary directory. |-}
writeBkrMetaFile :: (String, String) -> IO FilePath
writeBkrMetaFile confPair = do
     logDebug "writeBkrMetaFile called"
     -- Get tmp dir
     tmpDir <- getTemporaryDirectory
     -- Get hash of the file name
     let fullPathHash = show $ getHashForString $ fst confPair
     -- Get the full file path to the .bkrm file (<full path hash:file hash.bkrm>)
     let fullPath = tmpDir ++ fullPathHash ++ "." ++ snd confPair
     -- Get file modification time
     modTime <- getModificationTime $ fst confPair 
     -- Open a file handle
     hndl <- openBinaryFile fullPath WriteMode
     -- Map over a list of the lines to write to the file
     let hPutStrLnHndl = hPutStrLn hndl
     _ <- mapM hPutStrLnHndl ["[BkrMetaInfo]", 
                              "fullpath: " ++ fst confPair, 
                              "checksum: " ++ snd confPair, 
                              "modificationtime: " ++ show modTime, 
                              "fullpathchecksum: " ++ show (getHashForString $ fst confPair), 
                              "modificationtimechecksum: " ++ show (getHashForString $ show modTime)]
     -- Close the handle and return the file path
     hClose hndl
     return fullPath

getConfSetting :: String -> IO (Maybe String)
{-
getConfSetting key = do
     --confPairs <- getConfPairsFromFileS' "bkr.conf"
     --return $ getValueS key confPairs
     getConfPairsFromFileS' "bkr.conf" >>= return . getValueS key
-}
getConfSetting key = liftM (getValueS key) (getConfPairsFromFileS' "bkr.conf")

{-| Get a list of the folders to back up. If the setting cannot be found an IO Error is raised. |-}
getBackupFolders :: IO [FilePath]
getBackupFolders = do
     logDebug "getBackupFolders called"

     confSetting <- getConfSetting "folderstobackup"
     case confSetting of
          Just x  -> return $ map strip (split "," x)
          Nothing -> ioError $ userError "Failed to find the configuration setting folderstobackup. Please check the configuration."

{-| Get a list of files to ignore. If the settings cannot be found an empty list is returned |-}
getFilesToIgnore :: IO [FilePath]
getFilesToIgnore = do
     logDebug "getFilesToIgnore called"

     confSetting <- getConfSetting "filestoignore"
     case confSetting of
          Just x  -> return $ map strip (split "," x)
          Nothing -> do
                  logDebug $ "getFilesToIgnore: " ++ "the setting filestoignore was not found."
                  return []

{-| Get a list of files to ignore be extension. If the settings cannot be found an empty list is returned |-}
getFileExtensionsToIgnore :: IO [FilePath]
getFileExtensionsToIgnore = do
    logDebug "getFileExtensionsToIgnore called"

    confSetting <- getConfSetting "fileextensionstoignore"
    case confSetting of
         Just x  -> return $ map strip (split "," x)
         Nothing -> do
                 logDebug $ "getFileExtensionsToIgnore: " ++ "the setting fileextensionstoignore was not found."
                 return []

{-| Get a list of folders to ignore. If the settings cannot be found an empty list is returned |-}
getFoldersToIgnore :: IO [FilePath]
getFoldersToIgnore = do
    logDebug "getFoldersToIgnore called"

    confSetting <- getConfSetting "folderstoignore"
    case confSetting of
         Just x  -> return $ map strip (split "," x)
         Nothing -> do
                 logDebug $ "getFoldersToIgnore: " ++ "the setting folderstoignore was not found."
                 return []

{-| Get if we should use S3 reduced redundancy, if the setting cannot be found return reduced redundacy (there is not real reason not to use reduced redundancy for this kind of application.
|-} 
getUseS3ReducedRedundancy :: IO (Maybe StorageClass)
getUseS3ReducedRedundancy = do
     logDebug "getUseS3ReducedRedundancy called"

     confSetting <- getConfSetting "uses3reducedredundancy"
     case confSetting of
          Just x -> if x == "yes"
                       then return $ Just ReducedRedundancy
                       else return $ Just Standard
          Nothing -> do
                  logDebug $ "getUseS3ReducedRedundancy: " ++ "the setting uses3reducedredundancy was not found."
                  return $ Just ReducedRedundancy

{-| Get the log priority and default to debug if it could not be found or is misconfigured. |-}
getLogLevel :: IO Priority
getLogLevel = do
     --print "getLogLevel called"

     confSetting <- getConfSetting "loglevel"
     case confSetting of
          Just x  -> case x of
                         "notify"   -> return NOTICE
                         "critical" -> return CRITICAL
                         _          -> return DEBUG
          Nothing -> return DEBUG

{-| Get the file update check type, default to smart if it could not be found or is misconfigured. |-}
getFileUpdateCheckType :: IO FileUpdateCheckType
getFileUpdateCheckType = do
     logDebug "getFileUpdateCheckType called"

     confSetting <- getConfSetting "fileupdatecheck"
     case confSetting of
          Just x -> case x of
                         "checksum" -> return FUCChecksum
                         "date"     -> return FUCDate
                         _          -> return FUCSmart
          _      -> return FUCSmart