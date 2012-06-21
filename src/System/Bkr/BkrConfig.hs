
module System.Bkr.BkrConfig ( FileUpdateCheckType(..)
                            , getConfPairsFromFile
                            , getConfPairsFromFile'
                            , getConfPairsFromFile_
                            , getConfPairsFromFile_'
                            , getConfPairsFromFileS
                            , getConfPairsFromFileS'
                            , getConfPairsFromByteString
                            , getConfPairsFromByteString'
                            , getConfFile
                            , getValue
                            , getValueS
                            , getBackupFolders
                            , getFilesToIgnore
                            , getFileExtensionsToIgnore
                            , getFoldersToIgnore
                            , getUseS3ReducedRedundancy
                            , getLogLevel
                            , getFileUpdateCheckType
                            , getLogFileLocation
                            , getLogFileMaximumSize
                            , getIfSynchronizationMode
                            ) where

--import System.Bkr.Hasher (getHashForString)
import System.Bkr.BkrWriteExampleConfFile (writeExampleConfFile)

import System.IO (Handle, IOMode(..), openBinaryFile, hGetContents, hClose)
import System.Directory (getTemporaryDirectory, doesFileExist, getHomeDirectory, getTemporaryDirectory, getAppUserDataDirectory, createDirectoryIfMissing)
import System.FilePath.Posix (takeDirectory)
import Control.Monad (liftM)
import Data.String.Utils (split, strip, replace)
import Aws.S3.Model (StorageClass(..))
import System.Log.Logger (Priority(..))
import System.Environment (getArgs)

import qualified System.IO.Strict as S
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T

data FileUpdateCheckType = FUCChecksum
                         | FUCDate
                         | FUCSmart
                         deriving Eq

{-| Read lines from s and filter on empty lines and lines beginning with # -}
getFilteredLines :: String -> [T.Text]
getFilteredLines s = [ x | x <- map T.pack (lines s), x /= T.empty, T.head (T.stripStart x) /= '#' ]

{-| Gets configuration pairs. This function takes a FilePath and reads the file lazy which might lead to unexpected consequences. If you want to have more control over the file handle use getConfPairsFromFile_ and if you want the file to be read strictly without the unwanted (or wanted) lazines side effects use getConfPairsFromFileS. -}
getConfPairsFromFile :: FilePath -> IO [(T.Text, T.Text)]
getConfPairsFromFile path = do
     --logDebug "getConfPairsFromFile called"
     -- Read the config file, split into lines and pack as Text
     hndl <- openBinaryFile path ReadMode
     readF <- hGetContents hndl
     
     -- Just testing some styles. I'm not sure which one looks better, the first one is prabably easier to read.
     --return $ map getConfPair (getFilteredLines readF)
     return $ getConfPair `map` getFilteredLines readF

{-| Like getConfPairsFromFile but gets a String pair instead of Text. -}
getConfPairsFromFile' :: FilePath -> IO [(String, String)]
getConfPairsFromFile' path = --do
     --logDebug "getConfPairsFromFile' called"
     
     --pairs <- getConfPairsFromFile path
     --return $ map textToString pairs
     getConfPairsFromFile path >>= return . map textToString

{-| Like getConfPairsFromFile but takes a file handle instead of FilePath. -}
getConfPairsFromFile_ :: Handle -> IO [(T.Text, T.Text)]
getConfPairsFromFile_ hndl = do
     --logDebug "getConfPairsFromFile_ called"
     -- Read the config file, split into lines and pack as Text
     readF <- hGetContents hndl

     return $ map getConfPair (getFilteredLines readF)

{-| Like getConfPairsFromFile' but takes a file handle instead of FilePath. -}
getConfPairsFromFile_' :: Handle -> IO [(String, String)]
getConfPairsFromFile_' hndl = --do
     --logDebug "getConfPairsFromFile_' called"
     
     --pairs <- getConfPairsFromFile_ hndl
     --return $ map textToString pairs
     getConfPairsFromFile_ hndl >>= return . map textToString

{-| Like getConfPairsFromFile but reads file contents strictly. -}
getConfPairsFromFileS :: FilePath -> IO [(T.Text, T.Text)]
getConfPairsFromFileS path = do
     --logDebug "getConfPairsFromFileS called"
     -- Read the config file, split into lines and pack as Text
     hndl <- openBinaryFile path ReadMode
     -- Read contents strictly
     readF <- S.hGetContents hndl
     hClose hndl
     
     return $ map getConfPair (getFilteredLines readF)

{-| Like getConfPairsFromFile' but reads file contents strictly. -}
getConfPairsFromFileS' :: FilePath -> IO [(String, String)]
getConfPairsFromFileS' path = --do
     --logDebug "getConfPairsFromFileS' called"

     --pairs <- getConfPairsFromFileS path
     --return $ map textToString pairs
     getConfPairsFromFileS path >>= return . map textToString

{-| Take a ByteString text, convert to lines and return Text pairs. -}
getConfPairsFromByteString :: BS.ByteString -> IO [(T.Text, T.Text)]
getConfPairsFromByteString bS = do
     --logDebug "getConfPairsFromByteString called"
     --let fileLines = map T.pack (lines $ BS.unpack bS)
     -- Get lines and filter lines beginning with #
     let fileLines = [ x | x <- map T.pack (lines $ BS.unpack bS), x /= T.empty, T.head (T.stripStart x) /= '#' ]
     return $ map getConfPair fileLines

{-| Like getConfPairsFromByteString but returns String. -}
getConfPairsFromByteString' :: BS.ByteString -> IO [(String, String)]
getConfPairsFromByteString' bS = --do
     --logDebug "getConfPairsFromByteString' called"

     --pairs <- getConfPairsFromByteString bS
     --return $ map textToString pairs
     getConfPairsFromByteString bS >>= return . map textToString

{-| Get a pair for Text line. |-}
getConfPair :: T.Text -> (T.Text, T.Text)
getConfPair line = (T.strip $ head s, T.strip $ last s)
                    where s = T.split (==':') line

{-| Convert a Text pair to a String pair. |-}
textToString :: (T.Text, T.Text) -> (String, String)
textToString x = (T.unpack $ fst x, T.unpack $ snd x)

{-| Gets the value for a list of value pairs. The function matches on the first key found and does not check for multiple keys with the same name. If the key is not found Nothing is returned. -}
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

{-| Check if bkr.conf is passed as the first argument. We do not check if the bkr.conf file is valid, just that a file was passed as the first argument. -}
getArgIfValid :: IO (Maybe FilePath)
getArgIfValid = do
     args <- getArgs
     case args of
          [x] -> do
                  fileExists <- doesFileExist x
                  if fileExists
                     then return $ Just x
                     else return Nothing
          _   -> return Nothing

{-| Check if there is a bkr.conf file in the same directory as the bkr executable. We do not check that the bkr.conf file is valid, only if it exists. -}
getBkrConfFromDot :: IO (Maybe FilePath)
getBkrConfFromDot = do
     fileExists <- doesFileExist "./bkr.conf"
     if fileExists
        then return $ Just "./bkr.conf"
        else return Nothing

{-| Check if there is a $HOME/.bkr.conf file. We do not check if the .bkr.conf file is valid, only if it exists. If the file cannot be found the bkr.conf.example file is copied to $HOME/.bkr.conf and the user is instructed to edit it. -}
getBkrFromHomeDir :: IO (Maybe FilePath)
getBkrFromHomeDir = do
     homeDir <- getHomeDirectory
     let filePath = (++) homeDir "/.bkr.conf"
     fileExists <- doesFileExist filePath
     if fileExists
        then return $ Just filePath
        else do
              --copyFile "./bkr.conf.example" filePath
              writeExampleConfFile filePath
              print $ "bkr configuration file could not be found. An example configuration file has been created in your home directory, " ++ filePath ++ ". Please edit the configuration file and run bkr again."
              return Nothing

{-|
Gets file path to the Bkr configuration file. File locations checked (in order):
    1. The first command line argument
    2. ./bkr.conf
    3. $HOME/.bkr.conf
-}
getConfFile :: IO (Maybe FilePath)
getConfFile = do
     argIfValid <- getArgIfValid
     case argIfValid of
          Just x -> return $ Just x
          _      -> do
                     bkrFromDot <- getBkrConfFromDot
                     case bkrFromDot of
                          Just x -> return $ Just x
                          _      -> do
                                     bkrFromHome <- getBkrFromHomeDir
                                     case bkrFromHome of
                                          Just x -> return $ Just x
                                          _      -> return Nothing
{-getConfFile = do
     argIfValid <- getArgIfValid
     when ((isNothing argIfValid) == False) (return argIfValid)

     bkrFromDot <- getBkrConfFromDot
     when ((isNothing bkrFromDot) == False) (return bkrFromDot)

     bkrFromHome <- getBkrFromHomeDir
     when ((isNothing bkrFromHome) == False) (return bkrFromHome)
     
     return Nothing-}

getConfSetting :: String -> IO (Maybe String)
getConfSetting key = do
     confFile <- getConfFile
     case confFile of
          Just x   -> liftM (getValueS key) (getConfPairsFromFileS' x)
          Nothing  -> return Nothing

{-| Get a list of the folders to back up. If the setting cannot be found an IO Error is raised. -}
getBackupFolders :: IO [FilePath]
getBackupFolders = do
     --logDebug "getBackupFolders called"

     confSetting <- getConfSetting "folderstobackup"
     case confSetting of
          Just x  -> return $ map strip (split "," x)
          Nothing -> ioError $ userError "Failed to find the configuration setting folderstobackup. Please check the configuration."

{-| Get a list of files to ignore. If the settings cannot be found an empty list is returned. -}
getFilesToIgnore :: IO [FilePath]
getFilesToIgnore = do
     --logDebug "getFilesToIgnore called"

     confSetting <- getConfSetting "filestoignore"
     case confSetting of
          Just x  -> return $ map strip (split "," x)
          Nothing -> do
                  --logDebug $ "getFilesToIgnore: " ++ "the setting filestoignore was not found."
                  return []

{-| Get a list of files to ignore be extension. If the settings cannot be found an empty list is returned. -}
getFileExtensionsToIgnore :: IO [FilePath]
getFileExtensionsToIgnore = do
    --logDebug "getFileExtensionsToIgnore called"

    confSetting <- getConfSetting "fileextensionstoignore"
    case confSetting of
         Just x  -> return $ map strip (split "," x)
         Nothing -> do
                 --logDebug $ "getFileExtensionsToIgnore: " ++ "the setting fileextensionstoignore was not found."
                 return []

{-| Get a list of folders to ignore. If the settings cannot be found an empty list is returned. -}
getFoldersToIgnore :: IO [FilePath]
getFoldersToIgnore = do
    --logDebug "getFoldersToIgnore called"

    confSetting <- getConfSetting "folderstoignore"
    case confSetting of
         Just x  -> return $ map strip (split "," x)
         Nothing -> do
                 --logDebug $ "getFoldersToIgnore: " ++ "the setting folderstoignore was not found."
                 return []

{-| Get if we should use S3 reduced redundancy, if the setting cannot be found return reduced redundacy (there is not real reason not to use reduced redundancy for this kind of application. |-}
getUseS3ReducedRedundancy :: IO (Maybe StorageClass)
getUseS3ReducedRedundancy = do
     --logDebug "getUseS3ReducedRedundancy called"

     confSetting <- getConfSetting "uses3reducedredundancy"
     case confSetting of
          Just x -> if x == "yes"
                       then return $ Just ReducedRedundancy
                       else return $ Just Standard
          Nothing -> do
                  --logDebug $ "getUseS3ReducedRedundancy: " ++ "the setting uses3reducedredundancy was not found."
                  return $ Just ReducedRedundancy

{-| Get the log priority and default to debug if it could not be found or is misconfigured. -}
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

{-| Get the file update check type, default to smart if it could not be found or is misconfigured. -}
getFileUpdateCheckType :: IO FileUpdateCheckType
getFileUpdateCheckType = do
     --logDebug "getFileUpdateCheckType called"

     confSetting <- getConfSetting "fileupdatecheck"
     case confSetting of
          Just x -> case x of
                         "checksum" -> return FUCChecksum
                         "date"     -> return FUCDate
                         _          -> return FUCSmart
          _      -> return FUCSmart

{-| Get log file location. $HOME, $TMP and $APPDATA are replaced with user's home directory, system temp directory and user's application data directory respectively. If the directory is not present it's created. -}
getLogFileLocation :: IO (Maybe FilePath)
getLogFileLocation = do
     confSetting <- getConfSetting "logfilelocation"
     case confSetting of
          Just x -> do
                     homeDir <- getHomeDirectory
                     tmpDir <- getTemporaryDirectory
                     appDir <- getAppUserDataDirectory "bkr"
                     let logFilePath = strReplace ["$HOME", "$TMP", "$APPDATA"] [homeDir, tmpDir, appDir] x 

                     createDirectoryIfMissing True (takeDirectory logFilePath)

                     return $ Just $ logFilePath
          _      -> return Nothing

     where strReplace :: [String] -> [String] -> String -> String
           strReplace (x:xs) (xR:xRs) s = strReplace xs xRs (replace x xR s)
           strReplace _ _ s = s

{-| Get log file maximum size in bytes. If the size cannot be found default to 5MB. -}
getLogFileMaximumSize :: IO Int
getLogFileMaximumSize = do
     confSetting <- getConfSetting "logfilemaximumsize"
     case confSetting of
          Just x -> return $ rInt x
          _      -> return 5242880
     
     where rInt :: String -> Int
           rInt = read

{-| Get if we should use synchronization mode. Default to False if not found or misconfigured -}
getIfSynchronizationMode :: IO Bool
getIfSynchronizationMode = do
     confSetting <- getConfSetting "synchronizationmode"
     case confSetting of
          Just x -> case x of
                         "yes" -> return True
                         _     -> return False
          _      -> return False