{-|
Easy to use backup tool utilizing cloud services (S3 only right now) as backup storage.

bkr is in very early development stage. Right now bkr is rather a synchronization then a backup utility. bkr uploads files from wanted folders to a remote storage service, next time it runs it checks for changes and uploads new or altered files but does not keep copies of altered files (hence rather synchronization then backup). For more information about installation and setup, release notes and more please visit https:\/\/github.com\/ingesson\/bkr. All suggestions and bug reports are of course more then welcome.
-}

import System.Bkr.BkrFundare (BkrMeta(..))
import System.Bkr.BkrConfig (getConfFile, getLogLevel, getBackupFolders, writeBkrMetaFile)
import System.Bkr.BkrLogging (setupLogging, logNotice, logDebug)
import qualified System.Bkr.BkrLocalFile as F
import qualified System.Bkr.TargetServices.S3.BkrS3Bucket as S3B

import System.Directory (removeFile)
import Control.Monad (when)
import Data.Maybe (isNothing, fromJust)
--import Data.Global (declareIORef)
--import List (filter, zip3, concat)
--import Control.Monad (mapM, forM)
--import Data.Maybe (fromJust)
--import Data.String.Utils (split)

main :: IO ()
main = do
     
     -- Check for valid configuration file and return () if it cannot be found (error message is shown by getConfFile).
     --print "will get conf file"
     confFile <- getConfFile
     when (isNothing confFile) (return ())
     
     -- Set up logging
     --print "will set up logging"
     getLogLevel >>= setupLogging
     logNotice "Bkr started"
     logDebug $ "Using config file " ++ (fromJust confFile)
     
     -- Get BkrMeta objects from S3
     logNotice "Getting Bkr files from S3"
     bkrS3Meta <- S3B.getBkrObjects

     -- Get local BkrMeta objects
     logNotice "Getting local files"
     --bkrLocalMeta <- getBackupFolders >>= mapM F.getBkrObjects
     bkrLocalMeta <- getBackupFolders >>= F.getBkrMetaForLocalPaths     
     
     -- Filter objects to get local objects that are not backed up
     logNotice "Checking which files should be uploaded"
     --let objToUpload = filter (`notElem` bkrS3Meta) (concat bkrLocalMeta)
     let objToUpload = filter (`notElem` bkrS3Meta) bkrLocalMeta
     -- Create a list with a triple (bkrObj, no of bkrObj, nth bkrObj) to use as a counter
     let len = length objToUpload
     let counterList = zip3 objToUpload [len | _ <- [(1 :: Int)..]] [1..] -- We can use non ending lists since zip ends when the shortest (objToUpload) list ends. Got to love this lazy stuff.
     logNotice $ show len ++ " files will be uploaded"
     -- For each element in objToUpload upload the local file then create a .bkrm file and upload it
     _ <- mapM putFiles counterList
     logNotice "done"  

putFiles :: (BkrMeta, Int, Int) -> IO ()
putFiles (bkrObj, len, nthObj) = do
     
     let localPath = fullPath bkrObj
     logNotice $ "Uploading " ++ show nthObj ++ "/" ++ show len ++ ": " ++ localPath
     -- Put local file
     S3B.putBackupFile localPath
     -- Get .bkrm file
     tmpFilePath <- writeBkrMetaFile (localPath, fileChecksum bkrObj)
     S3B.putBkrMetaFile tmpFilePath >> removeFile tmpFilePath
     --return ()