module Shared.Common.Bootstrap.Config where

import System.Exit

loadConfig fileName = loadConfigWith fileName fileName

loadConfigWith label source loadFn = do
  eitherConfig <- loadFn source
  case eitherConfig of
    Right config -> do
      print ("Config '" ++ label ++ "' loaded")
      return config
    Left error -> do
      print "Config load failed"
      print ("Server can't load '" ++ label ++ "'. Maybe the file is missing or not well-formatted")
      print error
      exitFailure
