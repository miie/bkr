
module System.Bkr.BkrLogging ( setupLogging
                      , logDebug
                      , logNotice
                      , logCritical
                      ) where

import System.Bkr.BkrConfig (getLogFileLocation,  getLogFileMaximumSize)

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
     --print $ show logFilePath
     -- Check if we got a log file, if not set up logging to stderr otherwise set up file logging
     if logFilePath == Nothing
        then do
              putStrLn "Could not find log file, please make sure to set the log file path correctly in bkr settings. Will log to stderr."
              updateGlobalLogger "bkrfile" (setLevel logLevel)
        else do
              -- Check that the log file hasn't reached maximum size and recycle if it has
              logFileMaxSize <- getLogFileMaximumSize
              fileStatus <- getFileStatus (fromJust logFilePath)
              let logFileSize = fileSize fileStatus
              when ((read (show logFileSize) :: Int) > logFileMaxSize) (removeFile $ fromJust logFilePath)

              -- File handler
              h <- fileHandler (fromJust logFilePath) DEBUG >>= \lh -> return $ setFormatter lh (tfLogFormatter "%F %X.%q" "$time|$loggername|$prio: $msg")
        
              -- Update global logger with handler and log level
              updateGlobalLogger "bkrfile" (addHandler h)
              updateGlobalLogger "bkrfile" (setLevel logLevel)
        

logDebug :: String -> IO ()
logDebug = debugM "bkrfile"

logNotice :: String -> IO ()
logNotice = noticeM "bkrfile"

logCritical :: String -> IO ()
logCritical = criticalM "bkrfile"
