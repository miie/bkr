

import qualified BkrLocalFile as F 
import BkrS3Bucket
import BkrConfig
import Maybe (fromJust)
import Hasher
import System.Directory (removeFile)

main :: IO ()
main = do
     {-
     allFiles <- getAllFiles "/Users/michaelsmietana/Pictures/bkrtst/_DSC2283.NEF"
     print allFiles
     fileHash <- getFileHash $ allFiles !! 0
     print $ show fileHash
     bkrMeta <- getBkrMeta $ allFiles !! 0
     print $ show bkrMeta
     
     tst0 <- getBkrMeta "/Users/michaelsmietana/Pictures/bkrtst/_DSC2283.NEF"
     tst1 <- getBkrMeta "/Users/michaelsmietana/Pictures/bkrtst/_DSC2283.NEF"
     if tst0 == tst1
        then print "true"
        else print "false"
     
     let tst2 = BkrMeta "/Users/michaelsmietana/Pictures/bkrtst/_DSC2283.NEF" "572c5146bf298063c74141b7cf488639"
     if tst0 == tst2
        then print "true"
        else print "false"
     -}
     {-- NOTE <- do test and fix this!!!
     tst3 <- getBkrMeta "/Users/michaelsmietana/Pictures/bkrtst/_DSC2284e.NEF"
     if tst0 == tst3
        then print "true"
        else print "false"
     --}
     --pairs <- getConfPairsFromFile "tst.bkrm"
     --print $ show pairs
     --pairsS <- getConfPairsFromFile' "tst.bkrm"
     --print $ show $ fromJust $ lookup "fullpath" pairsS
     
     {-
     bkrObjects <- getBkrObjects
     print $ show bkrObjects

     print $ show $ getHashForString "sdknafsfadsäöåfas"
     
     tstPath <- writeBkrMetaFile ("/Users/michaelsmietana/Pictures/bkrtst/_DSC2284.NEF", "3490bbfa9558673777dcd4973d76685d")
     print $ show tstPath
     --removeFile tstPath

     localBkrObjects <- F.getBkrObjects "/Users/michaelsmietana/Pictures/bkrtst/"
     print $ show localBkrObjects

     putFile "/Users/michaelsmietana/Pictures/bkrtst/_DSC2285.NEF"
     -}
     allF <- F.getAllFiles "/Users/michaelsmietana/Pictures/Stig"
     print $ show $ getFileHash' allF
     print "done"