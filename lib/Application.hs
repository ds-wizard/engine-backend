module Application where

import Control.Lens ((^.))
import Control.Monad.Except
import Web.Scotty

import Api.Router
import Common.Context
import Common.DSWConfig
import Database.Connection
import Database.Migration.Migration

applicationConfigFile = "config/app-config.cfg"

buildInfoFile = "config/build-info.cfg"

runServer = do
  putStrLn
    "/-------------------------------------------------------------\\\n\
  \|    _____   _____ _____     _____                            |\n\
  \|   |  __ \\ / ____|  __ \\   / ____|                           |\n\
  \|   | |  | | (___ | |__) | | (___   ___ _ ____   _____ _ __   |\n\
  \|   | |  | |\\___ \\|  ___/   \\___ \\ / _ \\ '__\\ \\ / / _ \\ '__|  |\n\
  \|   | |__| |____) | |       ____) |  __/ |   \\ V /  __/ |     |   \n\
  \|   |_____/|_____/|_|      |_____/ \\___|_|    \\_/ \\___|_|     |   \n\
  \|                                                             |\n\                                             
  \\\-------------------------------------------------------------/"
  putStrLn "SERVER: started"
  eitherDspConfig <- loadDSWConfig applicationConfigFile buildInfoFile
  case eitherDspConfig of
    Left (errorDate, reason) -> do
      putStrLn "CONFIG: load failed"
      putStrLn "Can't load app-config.cfg or build-info.cfg. Maybe the file is missing or not well-formatted"
      print errorDate
    Right dswConfig -> do
      putStrLn "CONFIG: loaded"
      createDBConn dswConfig $ \dbPool -> do
        putStrLn "DATABASE: connected"
        let context = Context {_ctxDbPool = dbPool, _ctxConfig = Config}
        runMigration context dswConfig
        let serverPort = dswConfig ^. dswcfgWebConfig ^. acwPort
        scotty serverPort (createEndpoints context dswConfig)
