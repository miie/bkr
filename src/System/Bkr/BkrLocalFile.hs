
{-# LANGUAGE ScopedTypeVariables #-}

module System.Bkr.BkrLocalFile ( getBkrMetaForLocalPaths
                        --, getBkrObjects
                        --, getAllFolders
                        --, getAllFiles
                        ) where

import System.Bkr.Hasher
import System.Bkr.BkrFundare
import System.Bkr.BkrLocalMeta
import System.Bkr.BkrConfig (getFilesToIgnore, getFileExtensionsToIgnore, getFoldersToIgnore, getFileUpdateCheckType, FileUpdateCheckType(..))

import Control.Monad (filterM, liftM)
import System.Directory (doesDirectoryExist, getDirectoryContents, doesFileExist, getModificationTime)
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

{-
getBkrMeta'' :: [FilePath] -> IO [BkrMeta]
getBkrMeta'' paths = do
     -- Get all folders, combine with .bkrmeta file for full file path to the .bkrmeta files, filter to to check which .bkrmeta files exist, get BkrMeta objects and finally get BkrMeta_ objects
     allFolders <- mapM getAllFolders paths
     allBkrLocalMetaFiles <- filterM doesFileExist (map combine_ (concat allFolders))
     localMetaDB <- mapM getLocalMeta allBkrLocalMetaFiles
     let localMeta_ = map BkrMeta_ (concat localMetaDB)
     
     -- Get all files
     allFiles <- mapM getAllFiles paths
     localMetaFiles <- mapM getBkrMeta (concat allFiles)
     let localMetaFiles_ = map BkrMeta_ localMetaFiles
     let notCached = filter (`notElem` localMeta_) localMetaFiles_

     let allFiles = map fullPath (map bkrMeta notCached)

     notCachedMeta <- mapM getBkrMeta allFiles --(fullPath (BkrMeta notCached))


     --let allMeta = (concat localMetaDB) ++ notCachedMeta

     return $ (concat localMetaDB) ++ notCachedMeta
-}
{-
getAllFiles :: FilePath -> IO [FilePath]
getAllFiles topPath = do
     names <- getDirectoryContents topPath `catch` \ (ex :: IOException) -> handleIO ex topPath
     --filesToIgnore <- getFilesToIgnore
     filesToIgnore <- getFilesToFilter
     fileExtToIgnore <- getFileExtensionsToIgnore
     foldersToIgnore <- getFoldersToIgnore
     -- Filter files on extension and files to ignore
     --let properNames = (filterExt fileExtToIgnore) $ filterFile ([".", "..", ".bkrmeta"] ++ filesToIgnore) names
     let properNames = filterExt fileExtToIgnore $ filterFile filesToIgnore names
     -- Map files with topPath to get full path and filter on folders to ignore
     let allPaths = filterFolder foldersToIgnore $ map (topPath </>) properNames
     paths <- forM allPaths $ \path -> do
           isDirectory <- doesDirectoryExist path
           if isDirectory
              then getAllFiles path
              else return [normalise path]
     return $ concat paths
-}
{-
getAllFilesOld :: FilePath -> IO [FilePath]
getAllFilesOld topPath = do
     names <- getDirectoryContents topPath `catch` \ (ex :: IOException) -> handleIO ex topPath
     --filesToIgnore <- getFilesToIgnore
     filesToIgnore <- getFilesToFilter
     --let properNames = filter (`notElem` ([".", ".."] ++ filesToIgnore)) names
     let properNames = filter (`notElem` (filesToIgnore)) names
     print $ show properNames
     paths <- forM properNames $ \name -> do
           let path = topPath </> name
           isDirectory <- doesDirectoryExist path
           if isDirectory
              then getAllFiles path
              else return [normalise path]
     return $ concat paths
-}
{-
getBkrMeta :: FilePath -> IO BkrMeta
getBkrMeta path = do
     fileHash <- getFileHash path
     return $ BkrMeta path (show fileHash) (show $ getHashForString path)
-}
{-
getBkrObjects :: FilePath -> IO [BkrMeta]
getBkrObjects path = --do
     --allFiles <- getAllFiles path
     --mapM getBkrMeta allFiles
     getAllFiles path >>= mapM getBkrMeta
-}
{-
getLocalBkrObjects :: FilePath -> IO [BkrMeta]
getLocalBkrObjects path = do
     folders <- getAllFolders path
     return $ mapM getBkrMetaFromDB folders
-}
{-
getBkrMeta' :: FilePath -> IO [BkrMeta]
getBkrMeta' = do
     folders <- getAllFolders path
     return $ concat $ map getBkrMeta'' folders
-}
{-
combine_ :: FilePath -> FilePath
combine_ path = (++) path "/.bkrmeta"
-}