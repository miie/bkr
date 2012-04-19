
import BkrFundare
import qualified BkrLocalFile as F
import qualified BkrS3Bucket as S3B
import BkrConfig
import List (filter, zip3, concat)
import System.Directory (removeFile)
import Control.Monad (mapM, forM)
import Data.Maybe (fromJust)
import Data.String.Utils (split)

import BkrLogging

main :: IO ()
main = do
     
     -- Set up logging
     setupLogging
  
     logNotice "Bkr started"
     
     -- Get BkrMeta objects from S3
     logNotice "Getting Bkr files from S3"
     bkrS3Meta <- S3B.getBkrObjects
     --debugF $ show bkrS3Meta
     -- Get local BkrMeta objects
     logNotice "Getting local files"
     confPairs <- getConfPairsFromFileS' "bkr.conf"
     --bkrLocalMeta <- F.getBkrObjects "/Users/michaelsmietana/Pictures/Annat"
     bkrLocalMeta <- mapM F.getBkrObjects (split "," (fromJust $ getValueS "folderstobackup" confPairs)) -- -> move the getting folders to backup to BkrConfig + remove spaces before and after and normalize
     
     -- Filter the objects and get local objects that are not on S3
     logNotice "Checking which files should be uploaded"
     let objToUpload = filter (`notElem` bkrS3Meta) (concat bkrLocalMeta)
     --print $ show objToUpload
     -- Create a list with a triple (bkrObj, no of bkrObj, nth bkrObj) to use as a counter
     let len = length objToUpload
     let counterList = zip3 objToUpload [len | x <- [1..]] [1..] -- We can use non ending lists since zip ends when the shortest (objToUpload) list ends. Got to love this lazy stuff.
     
     logNotice $ (show len) ++ " files will be uploaded"
     -- For each element in objToUpload upload the local file then create a .bkrm file and upload it
     --mapM putFiles objToUpload
     mapM putFiles counterList
     logNotice "done"  

putFiles :: (BkrMeta, Int, Int) -> IO ()
--putFiles bkrObj = do
putFiles (bkrObj, len, nthObj) = do
     
     let localPath = fullPath bkrObj
     logNotice $ "Uploading " ++ (show nthObj) ++ "/" ++ (show len) ++ ": " ++ localPath
     -- Put local file
     S3B.putBackupFile localPath
     -- Get .bkrm file
     tmpFilePath <- writeBkrMetaFile (localPath, fileChecksum bkrObj)
     S3B.putBkrMetaFile tmpFilePath
     removeFile tmpFilePath
     return ()