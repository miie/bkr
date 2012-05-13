
module BkrLogging ( setupLogging
                  , logDebug
                  , logNotice
                  , logCritical
                  ) where

import System.Log.Logger
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter
import System.Log.Handler.Growl

setupLogging :: Priority -> IO ()
setupLogging logLevel = do
     
     -- File handler
     h <- fileHandler "debug.log" DEBUG >>= \lh -> return $ setFormatter lh (tfLogFormatter "%F %X.%q" "$time|$loggername|$prio: $msg")
     
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
