
module Bkr.BkrLogging ( setupLogging
                      , logDebug
                      , logNotice
                      , logCritical
                      ) where

import Bkr.BkrConfig (getLogFileLocation,  getLogFileMaximumSize)

import System.Log.Logger
import System.Log.Handler.Simple
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import Data.Maybe (fromJust)
import System.Posix.Files (getFileStatus, fileSize)
import System.Directory (removeFile)
import Control.Monad (when)

--import System.Log.Handler.Growl

setupLogging :: Priority -> IO ()
setupLogging logLevel = do
     -- Get log file location
     logFilePath <- getLogFileLocation

     -- Check that the log file hasn't reached maximum size and recycle if it has
     logFileMaxSize <- getLogFileMaximumSize
     fileStatus <- getFileStatus (fromJust logFilePath)
     let logFileSize = fileSize fileStatus
     when ((read (show logFileSize) :: Int) > logFileMaxSize) (removeFile $ fromJust logFilePath)

     -- File handler
     h <- fileHandler (fromJust logFilePath) DEBUG >>= \lh -> return $ setFormatter lh (tfLogFormatter "%F %X.%q" "$time|$loggername|$prio: $msg")
     
     -- Growl handler
     --g <- growlHandler "bkrgrow" NOTICE
     --addTarget "localhost" g
     
     updateGlobalLogger "bkrfile" (addHandler h)
     --updateGlobalLogger "bkrgrowl" (addHandler g)
     
     updateGlobalLogger "bkrfile" (setLevel logLevel)
     --updateGlobalLogger "bkrgrowl" (setLevel NOTICE)

logDebug :: String -> IO ()
logDebug = debugM "bkrfile"

logNotice :: String -> IO ()
logNotice = noticeM "bkrfile"

logCritical :: String -> IO ()
logCritical = criticalM "bkrfile"
