
import Bkr.BkrFundare
import Bkr.BkrConfig
import Bkr.BkrLogging
import qualified Bkr.BkrLocalFile as F
import qualified Bkr.TargetServices.S3.BkrS3Bucket as S3B

import System.Directory (removeFile)
--import List (filter, zip3, concat)
--import Control.Monad (mapM, forM)
--import Data.Maybe (fromJust)
--import Data.String.Utils (split)



main :: IO ()
main = do
     
     -- Set up logging
     getLogLevel >>= setupLogging
  
     logNotice "Bkr started"
     
     -- Get BkrMeta objects from S3
     logNotice "Getting Bkr files from S3"
     bkrS3Meta <- S3B.getBkrObjects

     -- Get local BkrMeta objects
     logNotice "Getting local files"
     --bkrLocalMeta <- getBackupFolders >>= mapM F.getBkrObjects
     bkrLocalMeta <- getBackupFolders >>= F.getBkrMeta'''     
     
     -- Filter objects to get local objects that are not backed up
     logNotice "Checking which files should be uploaded"
     --let objToUpload = filter (`notElem` bkrS3Meta) (concat bkrLocalMeta)
     let objToUpload = filter (`notElem` bkrS3Meta) bkrLocalMeta
     -- Create a list with a triple (bkrObj, no of bkrObj, nth bkrObj) to use as a counter
     let len = length objToUpload
     --let counterList = zip3 objToUpload [len | x <- [1..]] [1..] -- We can use non ending lists since zip ends when the shortest (objToUpload) list ends. Got to love this lazy stuff.
     let counterList = zip3 objToUpload [len] [1..] -- We can use non ending lists since zip ends when the shortest (objToUpload) list ends. Got to love this lazy stuff.
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
     S3B.putBkrMetaFile tmpFilePath
     removeFile tmpFilePath
     return ()