
{-# LANGUAGE ScopedTypeVariables #-}

module BkrLocalFile ( getBkrObjects   
                    --, getAllFiles
                    --, getBkrMeta
                    ) where

import Control.Monad (forM, mapM)
import System.Directory (doesDirectoryExist, getDirectoryContents, doesFileExist)
import System.FilePath ((</>), normalise, takeExtensions, takeDirectory)
import Prelude hiding (catch)
import Control.Exception

import Data.Digest.Pure.MD5 (md5, MD5Digest)
import qualified Data.ByteString.Lazy as L

import Hasher
import BkrFundare
import BkrConfig (getFilesToIgnore, getFileExtensionsToIgnore, getFoldersToIgnore)

filterExt :: [FilePath] -> [FilePath] -> [FilePath]
filterExt ignoreList = filter ((\x -> (takeExtensions x) `notElem` ignoreList))

filterFolder :: [FilePath] -> [FilePath] -> [FilePath]
filterFolder ignoreList = filter ((\x -> (takeDirectory x) `notElem` ignoreList))

filterFile :: [FilePath] -> [FilePath] -> [FilePath]
filterFile ignoreList = filter (\x -> x `notElem` ignoreList)

getAllFiles :: FilePath -> IO [FilePath]
getAllFiles topPath = do
  names <- getDirectoryContents topPath `catch` \ (ex :: IOException) -> handleIO ex topPath
  filesToIgnore <- getFilesToIgnore
  fileExtToIgnore <- getFileExtensionsToIgnore
  foldersToIgnore <- getFoldersToIgnore
  -- Filter files on extension and files to ignore
  --let properNames = (filterExt fileExtToIgnore) $ filter (`notElem` ([".", ".."] ++ filesToIgnore)) names
  let properNames = (filterExt fileExtToIgnore) $ filterFile ([".", ".."] ++ filesToIgnore) names
  -- Map files with topPath to get full path and filter on folders to ignore
  let allPaths = (filterFolder foldersToIgnore) $ map (topPath </>) properNames
  paths <- forM allPaths $ \path -> do
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getAllFiles path
      else return [normalise path]
  return $ concat paths

getAllFilesOld :: FilePath -> IO [FilePath]
getAllFilesOld topPath = do
  names <- getDirectoryContents topPath `catch` \ (ex :: IOException) -> handleIO ex topPath
  filesToIgnore <- getFilesToIgnore
  let properNames = filter (`notElem` ([".", ".."] ++ filesToIgnore)) names
  print $ show properNames
  paths <- forM properNames $ \name -> do
    let path = topPath </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getAllFiles path
      else return [normalise path]
  return $ concat paths

handleIO :: IOException -> FilePath -> IO [FilePath]
handleIO ex path = do
         --print $ show ex
         pathIsFile <- doesFileExist path
         if pathIsFile
            then return [path]
            else return []

getBkrMeta :: FilePath -> IO BkrMeta
getBkrMeta path = do
           fileHash <- getFileHash path
           --print "Got hash"
           -- An ugly workaround for and laziness IO problem where file handles to the files to be hashed are opened by the hashing readFile function but never closed since due to laziness the files are never read and therefore does not read EOF. Printing the hash creates IO and therefore forces evaluation. This must be fixed!  
           --print $ "Hash for file: " ++ (show path) ++ ": " ++ (show fileHash)
           --return $ BkrMeta path (show fileHash)
           return $ BkrMeta path (show fileHash) (show $ getHashForString path)

getBkrObjects :: FilePath -> IO [BkrMeta]
getBkrObjects path = do
     allFiles <- getAllFiles path
     mapM getBkrMeta allFiles