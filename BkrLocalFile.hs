
{-# LANGUAGE ScopedTypeVariables #-}

module BkrLocalFile ( getBkrObjects   
                    --, getAllFiles
                    --, getBkrMeta
                    ) where

import Control.Monad (forM, mapM)
import System.Directory (doesDirectoryExist, getDirectoryContents, doesFileExist)
import System.FilePath ((</>))
import Prelude hiding (catch)
import Control.Exception

import Data.Digest.Pure.MD5 (md5, MD5Digest)
import qualified Data.ByteString.Lazy as L

import Hasher
import BkrFundare

getAllFiles :: FilePath -> IO [FilePath]
getAllFiles topPath = do
  names <- getDirectoryContents topPath `catch` \ (ex :: IOException) -> handleIO ex topPath
  let properNames = filter (`notElem` [".", "..", ".DS_Store", ".localized"]) names
  paths <- forM properNames $ \name -> do
    let path = topPath </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getAllFiles path
      else return [path]
  return (concat paths)

handleIO :: IOException -> FilePath -> IO [FilePath]
handleIO ex path = do
         --print $ show ex
         pathIsFile <- doesFileExist path
         if pathIsFile
            then return [path]
            else return []

{- Moved to Hasher
getFileHash :: FilePath -> IO MD5Digest
getFileHash path = L.readFile path >>= return . md5
-}

getBkrMeta :: FilePath -> IO BkrMeta
getBkrMeta path = do
           fileHash <- getFileHash path
           return $ BkrMeta path (show fileHash)

getBkrObjects :: FilePath -> IO [BkrMeta]
getBkrObjects path = do
    allFiles <- getAllFiles path
    mapM getBkrMeta allFiles