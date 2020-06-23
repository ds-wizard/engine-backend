module Registry.Bootstrap.Config where

import System.Exit

loadConfig fileName loadFn = do
  eitherConfig <- loadFn fileName
  case eitherConfig of
    Right config -> do
      print ("Config '" ++ fileName ++ "' loaded")
      return config
    Left error -> do
      print "Config load failed"
      print ("Server can't load '" ++ fileName ++ "'. Maybe the file is missing or not well-formatted")
      print error
      exitFailure
