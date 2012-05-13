
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

import qualified BkrLocalFile as F 
import BkrS3Bucket
import BkrConfig
import Maybe (fromJust)
import Hasher
import System.Directory (removeFile, doesDirectoryExist)
import BkrFundare
import Data.Binary
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C
import BkrLocalMeta
import BkrLogging
import Filesystem.Path.CurrentOS as FS

main :: IO ()
main = do
     getLogLevel >>= setupLogging
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
     --allF <- F.getAllFiles "/Users/michaelsmietana/Pictures/Stig"
     --print $ show $ getFileHash' allF
     --print "done"
     {-
     let tst0 = BkrMeta "/Users" "ldsölkfnöasdkn" "dslkölksdnflö"
     let tst1 = BkrMeta "/Users/ms" "ldskldsn" "sldknlsdknlnsdlknsdl"
     --BL.writeFile "./tst.txt" (BL.concat $ map encode [tst0, tst1])
     BL.writeFile "./tst.txt" (encode tst0)
     BL.appendFile "./tst.txt" ("\n" ++ encode tst1)
     
     getFile <- BL.readFile "./tst.txt"
     let fileLines = C.lines getFile
     
     --d <- decode getFile
     let d :: BkrMeta = decode $ fileLines !! 0
     print $ show d
     -}
     --setTable
     
     --f <- F.getAllFolders' "/Users/michaelsmietana/Desktop" []
     --print $ show f
     {-
     let metatst1 = BkrMeta "" "10" "100" "" ""
     let metatst2 = BkrMeta "sdfa" "10" "100" "" ""
     print $ show $ metatst1 == metatst2
     
     let metatst3 = BkrMeta "asd" "100" "10" "asd" "10"
     let metatst4 = BkrMeta "" "" "10" "" "10"
     print $ show $ metatst3 == metatst4     
     -}
     
     t <- F.getAllFolders "/Users/michaelsmietana/Desktop/tst"
     print $ show t
     putStrLn $ t !! 0
     exists <- mapM doesDirectoryExist t
     print $ show exists
     print $ show $ FS.decodeString $ t !! 0
     print $ show $ toText $ FS.decodeString $ t !! 0
     putStrLn $ show $ toText $ FS.decodeString $ t !! 0
     --putStrLn $ toText $ FS.decodeString $ t !! 0
     print $ map FS.valid (map FS.decodeString t)
     
     --t <- F.getBkrMeta''' ["/Users/michaelsmietana/Desktop"]
     --print $ show t