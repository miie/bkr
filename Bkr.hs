
import BkrFundare
import qualified BkrLocalFile as F
import qualified BkrS3Bucket as S3B
import BkrConfig
import List (filter, zip3)
import System.Directory (removeFile)
import Control.Monad (mapM, forM)
import Data.Maybe (fromJust)

main :: IO ()
main = do
     
     -- Get BkrMeta objects from S3
     print "Getting Bkr files from S3"
     bkrS3Meta <- S3B.getBkrObjects
     --print $ show bkrS3Meta
     -- Get local BkrMeta objects
     print "Getting local files"
     confPairs <- getConfPairsFromFileS' "bkr.conf"
     --bkrLocalMeta <- F.getBkrObjects "/Users/michaelsmietana/Pictures/Annat"
     bkrLocalMeta <- F.getBkrObjects $ fromJust $ getValueS "folderstobackup" confPairs
     --print $ show bkrLocalMeta
     -- Filter the objects and get local objects that are not on S3
     print "Checking which files should be uploaded"
     let objToUpload = filter (`notElem` bkrS3Meta) bkrLocalMeta
     --print $ show objToUpload
     -- Create a list with a triple (bkrObj, no of bkrObj, nth bkrObj) to use as a counter
     let len = length objToUpload
     let counterList = zip3 objToUpload [len | x <- [1..]] [1..] -- We can use non ending lists since zip ends when the shortest (objToUpload) list ends. Got to love this lazy stuff.
     
     print $ (show len) ++ " files will be uploaded"
     -- For each element in objToUpload upload the local file then create a .bkrm file and upload it
     --mapM putFiles objToUpload
     mapM putFiles counterList
     print "done"  

putFiles :: (BkrMeta, Int, Int) -> IO ()
--putFiles bkrObj = do
putFiles (bkrObj, len, nthObj) = do
     
     let localPath = fullPath bkrObj
     print $ "Uploading " ++ (show nthObj) ++ "/" ++ (show len) ++ ": " ++ localPath
     -- Put local file
     S3B.putBackupFile localPath
     -- Get .bkrm file
     tmpFilePath <- writeBkrMetaFile (localPath, checksum bkrObj)
     S3B.putBkrMetaFile tmpFilePath
     removeFile tmpFilePath
     return ()