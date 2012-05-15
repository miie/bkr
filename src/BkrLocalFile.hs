
{-# LANGUAGE ScopedTypeVariables #-}

module BkrLocalFile ( getBkrObjects
                    , getAllFolders
                    --, getAllFiles
                    , getBkrMeta'''
                    ) where

import Control.Monad (forM, filterM)
import System.Directory (doesDirectoryExist, getDirectoryContents, doesFileExist, getModificationTime)
import System.FilePath ((</>), normalise, takeExtensions, takeDirectory)
import Prelude hiding (catch)
import Control.Exception

--import Data.Digest.Pure.MD5 (md5, MD5Digest)
--import qualified Data.ByteString.Lazy as L

import Hasher
import BkrFundare
import BkrConfig (getFilesToIgnore, getFileExtensionsToIgnore, getFoldersToIgnore, getFileUpdateCheckType, FileUpdateCheckType(..))
import BkrLocalMeta

filterExt :: [FilePath] -> [FilePath] -> [FilePath]
filterExt ignoreList = filter ((\x -> (takeExtensions x) `notElem` ignoreList))

filterFolder :: [FilePath] -> [FilePath] -> [FilePath]
filterFolder ignoreList = filter ((\x -> (takeDirectory x) `notElem` ignoreList))

filterFile :: [FilePath] -> [FilePath] -> [FilePath]
filterFile ignoreList = filter (\x -> x `notElem` ignoreList)

getFilesToFilter :: IO [FilePath]
getFilesToFilter = do
     --filesToFilter <- getFilesToIgnore
     --return $ [".", "..", ".bkrmeta"] ++ filesToIgnore
     getFilesToIgnore >>= return . (++) [".", "..", ".bkrmeta"]

getAllFiles :: FilePath -> IO [FilePath]
getAllFiles topPath = do
     names <- getDirectoryContents topPath `catch` \ (ex :: IOException) -> handleIO ex topPath
     --filesToIgnore <- getFilesToIgnore
     filesToIgnore <- getFilesToFilter
     fileExtToIgnore <- getFileExtensionsToIgnore
     foldersToIgnore <- getFoldersToIgnore
     -- Filter files on extension and files to ignore
     --let properNames = (filterExt fileExtToIgnore) $ filterFile ([".", "..", ".bkrmeta"] ++ filesToIgnore) names
     let properNames = (filterExt fileExtToIgnore) $ filterFile (filesToIgnore) names
     -- Map files with topPath to get full path and filter on folders to ignore
     let allPaths = (filterFolder foldersToIgnore) $ map (topPath </>) properNames
     paths <- forM allPaths $ \path -> do
           isDirectory <- doesDirectoryExist path
           if isDirectory
              then getAllFiles path
              else return [normalise path]
     return $ concat paths

getFilesInFolder :: FilePath -> IO [FilePath]
getFilesInFolder path = do
     names <- getDirectoryContents path `catch` \ (ex :: IOException) -> handleIO ex path
     filesToIgnore <- getFilesToFilter
     fileExtToIgnore <- getFileExtensionsToIgnore
     -- Filter files on extension and files to ignore
     let properNames = (filterExt fileExtToIgnore) $ filterFile (filesToIgnore) names
     -- Map files with topPath to get full path and filter on files
     filterM doesFileExist (map (path </>) properNames)

getAllFolders :: FilePath -> IO [FilePath]
getAllFolders topPath = do
     names <- getDirectoryContents topPath `catch` \ (ex :: IOException) -> handleIO ex topPath
     -- Map files with topPath to get full path and filter on folders to ignore
     foldersToIgnore <- getFoldersToIgnore
     let paths = (filterFolder foldersToIgnore) $ map (topPath </>) (filterFile ([".", ".."]) names)
     folders <- filterM doesDirectoryExist paths
     allFolders <- mapM getAllFolders folders
     let allF = map normalise ((concat allFolders) ++ folders)
     --return . reverse $ (concat allFolders) ++ folders
     --print $ "all: " ++ show all
     return allF
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
handleIO :: IOException -> FilePath -> IO [FilePath]
handleIO _ path = do
     pathIsFile <- doesFileExist path
     if pathIsFile
        then return [path]
        else return []
{-     
getBkrMeta :: FilePath -> IO BkrMeta
getBkrMeta path = do
     fileHash <- getFileHash path
     return $ BkrMeta path (show fileHash) (show $ getHashForString path)
-}
getBkrObjects :: FilePath -> IO [BkrMeta]
getBkrObjects path = do
     allFiles <- getAllFiles path
     mapM getBkrMeta allFiles
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
{-| Get a BkrMeta for only path and modification time that can be used with BkrMeta_ for Eq on path and modification time checksum |-}
getBkrMeta_ :: FilePath -> IO BkrMeta
getBkrMeta_ path = do
     modTime <- getModificationTime path
     return $ BkrMeta path "" (show $ getHashForString path) "" (show $ getHashForString $ show modTime)
{-
combine_ :: FilePath -> FilePath
combine_ path = (++) path "/.bkrmeta"
-}
getCachedBkrMetaList :: FilePath -> IO [BkrMeta]
getCachedBkrMetaList dotbkrmetaFile = do
     -- Get file update check type. checksum -> do not check .bkrmeta files, date and smart -> pass type to getLocalMeta
     updateFileCheck <- getFileUpdateCheckType
     if updateFileCheck == FUCChecksum
        then return []
        else do 
                -- Check that the .bkrmeta file exist and get all cached BkrMeta objects
                bkrMetaFile <- filterM doesFileExist [dotbkrmetaFile]
                let getLocalMeta' = getLocalMeta updateFileCheck
                cachedBkrMetaList <- mapM getLocalMeta' bkrMetaFile
                return $ concat cachedBkrMetaList
     
folderCompareAndGetBkrMeta :: FilePath -> IO [BkrMeta]
folderCompareAndGetBkrMeta path = do

     -- Get all files in folder and getBkrMeta_ them -> folderBkrMetaList -- DONE
     -- Get all BkrMeta objects from the .bkrmeta file -> dotbkrmetaBkrMetaList -- DONE
     -- Get all files that are in dotbkrmetaBkrMetaList but not in folderBkrMetaList and delete them from the .bkrmeta folder then get new dotbkrmetaBkrMetaList -- DONE
     -- Get all files that are in folderBkrMetaList but not in dotbkrmetaBkrMetaList -> notCachedBkrMetaList 
     -- Update .bkrmeta file with notCachedBkrMetaList
     -- Return notCachedBkrMetaList ++ dotbkrmetaBkrMetaList
     --print $ "folderCompareAndGetBkrMeta: " ++ show path
     let dotbkrmetaFile = path ++ "/.bkrmeta"

     -- Get all files in folder and getBkrMeta_ them (getBkrMeta_ does not get all BkrMeta items since folderBkrMetaList is only going to be used for filtering which files are not cached in .bkrmeta)
     filesInFolder <- getFilesInFolder path
     folderBkrMetaList <- mapM getBkrMeta_ filesInFolder

     -- Get all cached BkrMeta objects and mind file update check type setting; checksum -> do not check .bkrmeta files (get checksum for all files every time), date (check by file change date) and smart (get the checksum randomly but always at least every tenth run).
     cachedBkrMetaList' <- getCachedBkrMetaList dotbkrmetaFile

     -- Get all cached files that has been deleted from the disk and delete them from the .bkrmeta file
     let deletedCachedFiles = filter (`notElem` folderBkrMetaList) cachedBkrMetaList'
     --print $ "-: " ++ show path 
     --print $ "--: " ++ show dotbkrmetaFile 
     deleteBkrMeta dotbkrmetaFile deletedCachedFiles
     -- Get the cached files that has not been deleted from disk
     let cachedBkrMetaList = filter (`notElem` deletedCachedFiles) cachedBkrMetaList'
     --print $ "--" ++ show cachedBkrMetaList
     
     -- Get all files that are in folderBkrMetaList but not in cachedBkrMetaList
     let notCachedBkrMetaList = filter (`notElem` cachedBkrMetaList) folderBkrMetaList
     --print $ "---" ++ show notCachedBkrMetaList
     
     -- Get "real" BkrMeta objects and update .bkrmeta file with notCachedBkrMetaList
     let notCachedPaths = map fullPath notCachedBkrMetaList
     --print $ "-: " ++ show notCachedPaths
     notCachedBkrMeta <- mapM getBkrMeta notCachedPaths
     insertBkrMeta dotbkrmetaFile notCachedBkrMeta
     
     --print $ "--: " ++ show notCachedBkrMeta
     --print $ "---: " ++ show cachedBkrMetaList     
     -- Return cached and not cached BkrMeta objects
     return $ notCachedBkrMeta ++ cachedBkrMetaList

getBkrMeta''' :: [FilePath] -> IO [BkrMeta]
getBkrMeta''' paths = do
     allPaths <- mapM getAllFolders paths
     --print $ "allPaths: " ++ show ((concat allPaths) ++ paths)
     bkrMeta <- mapM folderCompareAndGetBkrMeta ((concat allPaths) ++ paths)
     return $ concat bkrMeta

{-
--not used:
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