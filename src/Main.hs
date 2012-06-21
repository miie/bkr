{-|
Easy to use backup tool utilizing cloud services (S3 only right now) as backup storage.

bkr is in very early development stage. Right now bkr is rather a synchronization then a backup utility. bkr uploads files from wanted folders to a remote storage service, next time it runs it checks for changes and uploads new or altered files but does not keep copies of altered files (hence rather synchronization then backup). For more information about installation and setup, release notes and more please visit https:\/\/github.com\/ingesson\/bkr. All suggestions and bug reports are of course more then welcome.
-}

import System.Bkr.BkrFundare (BkrMeta(..))
import System.Bkr.BkrConfig (getConfFile, getLogLevel, getBackupFolders, getIfSynchronizationMode)
import System.Bkr.BkrLogging (setupLogging, logNotice, logDebug)
import System.Bkr.BkrLocalFile (writeBkrMetaFile)
import qualified System.Bkr.BkrLocalFile as F
import qualified System.Bkr.TargetServices.S3.BkrS3Bucket as S3B

import System.Directory (removeFile)
import Control.Monad (when)
import Data.Maybe (isNothing, fromJust)

import qualified Data.Text as T

main :: IO ()
main = do
     
     -- Check for valid configuration file and return () if it cannot be found (error message is shown by getConfFile).
     confFile <- getConfFile
     when (isNothing confFile) (return ())
     
     -- Set up logging
     getLogLevel >>= setupLogging
     logNotice "Bkr started"
     logDebug $ "Using config file " ++ (fromJust confFile)
     
     -- Get BkrMeta objects from S3
     logNotice "Getting Bkr files from S3"
     bkrS3Meta <- S3B.getBkrObjects

     -- Get local BkrMeta objects
     logNotice "Getting local files"
     bkrLocalMeta <- getBackupFolders >>= F.getBkrMetaForLocalPaths     
     
     -- Filter objects to get local objects that are not backed up
     logNotice "Checking which files should be uploaded"
     let objToUpload = filter (`notElem` bkrS3Meta) bkrLocalMeta
     -- Create a list with a triple (bkrObj, no of bkrObj, nth bkrObj) to use as a counter
     let len = length objToUpload
     let counterList = zip3 objToUpload [len | _ <- [(1 :: Int)..]] [1..] -- We can use non ending lists since zip ends when the shortest (objToUpload) list ends. Got to love this lazy stuff.
     logNotice $ show len ++ " files will be uploaded"
     -- For each element in objToUpload upload the local file then create a .bkrm file and upload it
     _ <- mapM putFiles counterList

     -- Check if bkr is configured to run in synchronization mode, if yes check for backuped items that should be deleted
     syncMode <- getIfSynchronizationMode
     when syncMode (do
                     logNotice "bkr set in synchronization mode, will check for backed up files to delete (local files will not be affected)."
                     let objToDeleteFromBackup = filter (`notElem` bkrLocalMeta) bkrS3Meta
                     let delLen = length objToDeleteFromBackup
                     logNotice $ show delLen ++ " objects will be deleted from the backup"
                     _ <- mapM deleteObject objToDeleteFromBackup
                     logNotice "synchronization done"
                   )

     logNotice "bkr backup finished"

putFiles :: (BkrMeta, Int, Int) -> IO ()
putFiles (bkrObj, len, nthObj) = do
     
     let localPath = fullPath bkrObj
     logNotice $ "Uploading " ++ show nthObj ++ "/" ++ show len ++ ": " ++ localPath
     -- Put local file
     S3B.putBackupFile localPath
     -- Get .bkrm file
     tmpFilePath <- writeBkrMetaFile localPath bkrObj
     S3B.putBkrMetaFile tmpFilePath >> removeFile tmpFilePath

deleteObject :: BkrMeta -> IO ()
deleteObject bkrObj = do

     print $ show bkrObj
     -- Get the object name and delete the object from S3
     S3B.deleteBackedFile $ T.pack $ "bkrm." ++ (pathChecksum bkrObj) ++ "." ++ (fileChecksum bkrObj)