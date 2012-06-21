
{-# LANGUAGE ScopedTypeVariables #-}

module System.Bkr.BkrLocalFile ( getBkrMetaForLocalPaths
                               , getFileNameForBkrmFile
                               , writeBkrMetaFile
                               --, getBkrObjects
                               --, getAllFolders
                               --, getAllFiles
                               ) where

import System.Bkr.Hasher
import System.Bkr.BkrFundare
import System.Bkr.BkrLocalMeta
import System.Bkr.BkrConfig (getFilesToIgnore, getFileExtensionsToIgnore, getFoldersToIgnore, getFileUpdateCheckType, FileUpdateCheckType(..))

import System.IO (IOMode(..), openBinaryFile, hClose, hPutStrLn)
import Control.Monad (filterM, liftM)
import System.Directory (doesDirectoryExist, getDirectoryContents, doesFileExist, getModificationTime, getTemporaryDirectory)
import System.FilePath ((</>), normalise, takeExtensions, takeDirectory)
import Prelude hiding (catch)
import Control.Exception

--import Data.Digest.Pure.MD5 (md5, MD5Digest)
--import qualified Data.ByteString.Lazy as L

{-| Filter [FilePath] on file extension. The first argument is a list with file extensions to filter on (for example [\".txt\", \".doc\"]) and the second a list of the file paths to filter. -}
filterExt :: [FilePath] -> [FilePath] -> [FilePath]
filterExt ignoreList = filter (\x -> takeExtensions x `notElem` ignoreList)

{-| Filter [FilePath] on folders. The first argument is a list with folders to filter on (for example [\"/Users/username/donotbackup\", \"/Users/username/donotbackupeither\"] and the second a list of the file paths to filter. -}
filterFolder :: [FilePath] -> [FilePath] -> [FilePath]
filterFolder ignoreList = filter (\x -> takeDirectory x `notElem` ignoreList)

{-| Filter [FilePath] on files. The first argument is a list with files to filter on (for example [\"donotbackup\", \"donotbackupeither\"] and the second a list of the file paths to filter. -}
filterFile :: [FilePath] -> [FilePath] -> [FilePath]
--filterFile ignoreList = filter (\x -> x `notElem` ignoreList)
filterFile ignoreList = filter (`notElem` ignoreList)

{-| Handle IOException. Check if the passed FilePath is a file and return [FilePath] if it is and [] if not. Used, for instance, if you want to make sure that you don't miss a file if FilePath passed to getDirectoryContents is a file. -}
handleIOReturnIfFile :: IOException -> FilePath -> IO [FilePath]
handleIOReturnIfFile _ path = do
     pathIsFile <- doesFileExist path
     if pathIsFile
        then return [path]
        else return []

{-| Get files to ignore from bkr.conf and add some default files that always should be ignored. -}
getFilesToFilter :: IO [FilePath]
getFilesToFilter = --do
     --filesToFilter <- getFilesToIgnore
     --return $ [".", "..", ".bkrmeta"] ++ filesToIgnore
     getFilesToIgnore >>= return . (++) [".", "..", ".bkrmeta"]

{-| Get all files for a given folder filtered on file extension and files to ignore. -}
getFilesInFolder :: FilePath -> IO [FilePath]
getFilesInFolder path = do
     names <- getDirectoryContents path `catch` \ (ex :: IOException) -> handleIOReturnIfFile ex path
     filesToIgnore <- getFilesToFilter
     fileExtToIgnore <- getFileExtensionsToIgnore
     -- Filter files on extension and files to ignore
     let properNames = filterExt fileExtToIgnore $ filterFile filesToIgnore names
     -- Map files with topPath to get full path and filter on files
     filterM doesFileExist (map (path </>) properNames)

{-| Get all folders for a given path filtered on folders to ignore. -}
getAllFolders :: FilePath -> IO [FilePath]
getAllFolders topPath = do
     names <- getDirectoryContents topPath `catch` \ (_ :: IOException) -> return []
     -- Map files with topPath to get full path and filter on folders to ignore
     foldersToIgnore <- getFoldersToIgnore
     let paths = filterFolder foldersToIgnore $ map ((</>) topPath) (filterFile [".", ".."] names)
     folders <- filterM doesDirectoryExist paths
     allFolders <- mapM getAllFolders folders

     return $ map normalise (concat allFolders ++ folders)

{-| Get a BkrMeta for only path and modification time that can be used with BkrMeta_ for Eq on path and modification time checksum. -}
getBkrMeta_ :: FilePath -> IO BkrMeta
getBkrMeta_ path = do
     modTime <- getModificationTime path
     return $ BkrMeta path "" (show $ getHashForString path) "" (show $ getHashForString $ show modTime)

{-| 
Get file update check type:
    checksum -> do not check .bkrmeta files
    date and smart -> pass type to getLocalMeta 
-}
getCachedBkrMetaList :: FilePath -> IO [BkrMeta]
getCachedBkrMetaList dotbkrmetaFile = do
     updateFileCheck <- getFileUpdateCheckType
     if updateFileCheck == FUCChecksum
        then return []
        else do 
          -- Check that the .bkrmeta file exist and get all cached BkrMeta objects
          bkrMetaFile <- filterM doesFileExist [dotbkrmetaFile]
          let getLocalMeta' = getLocalMeta updateFileCheck
          --cachedBkrMetaList <- mapM getLocalMeta' bkrMetaFile
          --return $ concat cachedBkrMetaList
          mapM getLocalMeta' bkrMetaFile >>= return . concat

{-| 
For given FilePath:
    - Get all BkrMeta objects from .bkrmeta (if it exists)
    - Check if the .bkrmeta objects are valid or should be updated according to file update check type (see getCachedBkrMetaList)
    - Update .bkrmeta by inserting new and updated objects and deleting objects for files that have been deleted
    - Return all BkrMeta objects for the given FilePath
-}
folderCompareAndGetBkrMeta :: FilePath -> IO [BkrMeta]
folderCompareAndGetBkrMeta path = do
     let dotbkrmetaFile = path ++ "/.bkrmeta"

     -- Get all files in folder and getBkrMeta_ them (getBkrMeta_ does not get all BkrMeta items since folderBkrMetaList is only going to be used for filtering which files are not cached in .bkrmeta)
     --filesInFolder <- getFilesInFolder path
     --folderBkrMetaList <- mapM getBkrMeta_ filesInFolder
     folderBkrMetaList <- getFilesInFolder path >>= mapM getBkrMeta_

     -- Get all cached BkrMeta objects and mind file update check type setting; checksum -> do not check .bkrmeta files (get checksum for all files every time), date (check by file change date) and smart (get the checksum randomly but always at least every tenth run).
     cachedBkrMetaList' <- getCachedBkrMetaList dotbkrmetaFile

     -- Get all cached files that has been deleted from the disk and delete them from the .bkrmeta file
     let deletedCachedFiles = filter (`notElem` folderBkrMetaList) cachedBkrMetaList'
     deleteBkrMeta dotbkrmetaFile deletedCachedFiles
     -- Get the cached files that has not been deleted from disk
     let cachedBkrMetaList = filter (`notElem` deletedCachedFiles) cachedBkrMetaList'
     
     -- Get all files that are in folderBkrMetaList but not in cachedBkrMetaList
     let notCachedBkrMetaList = filter (`notElem` cachedBkrMetaList) folderBkrMetaList
     
     -- Get "real" BkrMeta objects and update .bkrmeta file with notCachedBkrMetaList
     let notCachedPaths = map fullPath notCachedBkrMetaList
     notCachedBkrMeta <- mapM getBkrMeta notCachedPaths
     insertBkrMeta dotbkrmetaFile notCachedBkrMeta
     
     -- Return cached and not cached BkrMeta objects
     return $ notCachedBkrMeta ++ cachedBkrMetaList

{-| Get all BkrMeta objects for a list of folders using .bkrmeta cached objects according to the bkr.conf fileupdatecheck setting. -}
getBkrMetaForLocalPaths :: [FilePath] -> IO [BkrMeta]
getBkrMetaForLocalPaths paths = do
     allPaths <- mapM getAllFolders paths
     --bkrMeta <- mapM folderCompareAndGetBkrMeta (concat allPaths ++ paths)
     --return $ concat bkrMeta
     --mapM folderCompareAndGetBkrMeta (concat allPaths ++ paths) >>= return . concat
     liftM concat (mapM folderCompareAndGetBkrMeta ((++) paths (concat allPaths)))

getFileNameForBkrmFile :: FilePath -> BkrMeta -> String
getFileNameForBkrmFile path bkrObj = "bkrm." ++ (show $ getHashForString path) ++ "." ++ fileChecksum bkrObj

{-| Take a bkr conf pair and write a .bkrm file in a temporary directory. -}
writeBkrMetaFile :: FilePath -> BkrMeta -> IO FilePath
writeBkrMetaFile path bkrObj = do
     
     -- Get system tmp dir
     tmpDir <- getTemporaryDirectory
     -- Get hash of the file name
     --let fullPathHash = show $ getHashForString path
     -- Get the full file path to the .bkrm file (<full path hash:file hash.bkrm>)
     -- Note to self, do not change how the file name is built without updating deleteObject function used by synchronization mode
     --let fullPath = tmpDir ++ "/" ++ "bkrm." ++ fullPathHash ++ "." ++ checksumForFile
     let fullPath' = tmpDir ++ "/" ++ getFileNameForBkrmFile path bkrObj
     -- Get file modification time
     modTime <- getModificationTime path
     -- Open a file handle
     hndl <- openBinaryFile fullPath' WriteMode
     -- Map over a list of the lines to write to the file
     let hPutStrLnHndl = hPutStrLn hndl
     _ <- mapM hPutStrLnHndl ["[BkrMetaInfo]" 
                              , "fullpath: " ++ path
                              , "checksum: " ++ fileChecksum bkrObj
                              , "modificationtime: " ++ show modTime
                              , "fullpathchecksum: " ++ (show $ getHashForString path)
                              , "modificationtimechecksum: " ++ (show $ getHashForString $ show modTime)
                              ]
     -- Close the handle and return the file path
     hClose hndl
     return fullPath'