
import BkrFundare
import qualified BkrLocalFile as F
import qualified BkrS3Bucket as S3B
import BkrConfig
import List (filter)
import System.Directory (removeFile)
import Control.Monad (mapM, forM)

main :: IO ()
main = do
     
     -- Get BkrMeta objects from S3
     print "Getting Bkr files from S3"
     bkrS3Meta <- S3B.getBkrObjects
     --print $ show bkrS3Meta
     -- Get local BkrMeta objects
     print "Getting local files"
     bkrLocalMeta <- F.getBkrObjects "/Users/michaelsmietana/Pictures/Annat"
     --print $ show bkrLocalMeta
     -- Filter the objects and get local objects that are not on S3
     print "Checking which files should be uploaded"
     let objToUpload = filter (`notElem` bkrS3Meta) bkrLocalMeta
     --print $ show objToUpload
     
     print $ show $ length objToUpload ++ " files will be uploaded"
     -- For each element in objToUpload upload the local file then create a .bkrm file and upload it
     mapM putFiles objToUpload
     print "done"  

putFiles :: BkrMeta -> IO ()
putFiles bkrObj = do
     
     let localPath = fullPath bkrObj
     print $ "Uploading: " ++ localPath
     -- Put local file
     S3B.putFile localPath
     -- Get .bkrm file
     tmpFilePath <- writeBkrMetaFile (localPath, checksum bkrObj)
     S3B.putFile' tmpFilePath
     removeFile tmpFilePath
     return ()