module Application where

import Control.Lens ((^.))
import Control.Monad.Except
import Data.Text
import Database.Persist.MongoDB (withMongoDBConn)
import Network
import Text.Regex
import Web.Scotty

import Api.Handler.Common
import Api.Handler.Event.EventHandler
import Api.Handler.IO.IOHandler
import Api.Handler.Info.InfoHandler
import Api.Handler.KnowledgeModel.KnowledgeModelHandler
import Api.Handler.KnowledgeModelContainer.KnowledgeModelContainerHandler
import Api.Handler.Organization.OrganizationHandler
import Api.Handler.Package.PackageHandler
import Api.Handler.Token.TokenHandler
import Api.Handler.User.UserHandler
import Api.Handler.Version.VersionHandler
import Api.Middleware.Auth
import Api.Middleware.CORS
import Context
import DSPConfig
import Database.Migration.Migration

applicationConfigFile = "config/app-config.cfg"

buildInfoFile = "config/build-info.cfg"

unauthorizedEndpoints = [mkRegex "^$", mkRegex "^tokens$", mkRegex "^export/.*/.*$"]

createEndpoints :: Context -> DSPConfig -> ScottyM ()
createEndpoints context dspConfig
   --------------------
   -- MIDDLEWARES
   --------------------
 = do
  middleware corsMiddleware
  middleware (authMiddleware dspConfig unauthorizedEndpoints)
   --------------------
   -- INFO
   --------------------
  get "/" (getInfoA context dspConfig)
   --------------------
   -- TOKENS
   --------------------
  post "/tokens" (postTokenA context dspConfig)
   --------------------
   -- ORGANIZATIONS
   --------------------
  get "/organizations/current" (getOrganizationCurrentA context dspConfig)
  put "/organizations/current" (putOrganizationCurrentA context dspConfig)
   --------------------
   -- USERS
   --------------------
  get "/users" (getUsersA context dspConfig)
  post "/users/" (postUsersA context dspConfig)
  get "/users/current" (getUserCurrentA context dspConfig)
  get "/users/:userUuid" (getUserA context dspConfig)
  put "/users/current/password" (putUserCurrentPasswordA context dspConfig)
  put "/users/current" (putUserCurrentA context dspConfig)
  put "/users/:userUuid/password" (putUserPasswordA context dspConfig)
  put "/users/:userUuid" (putUserA context dspConfig)
  delete "/users/:userUuid" (deleteUserA context dspConfig)
   --------------------
   -- KNOWLEDGE MODEL
   --------------------
  get "/kmcs" (getKnowledgeModelContainersA context dspConfig)
  post "/kmcs" (postKnowledgeModelContainersA context dspConfig)
  get "/kmcs/:kmcUuid" (getKnowledgeModelContainerA context dspConfig)
  put "/kmcs/:kmcUuid" (putKnowledgeModelContainerA context dspConfig)
  delete "/kmcs/:kmcUuid" (deleteKnowledgeModelContainerA context dspConfig)
  get "/kmcs/:kmcUuid/km" (getKnowledgeModelA context dspConfig)
  get "/kmcs/:kmcUuid/events" (getEventsA context dspConfig)
  post "/kmcs/:kmcUuid/events/_bulk" (postEventsA context dspConfig)
  delete "/kmcs/:kmcUuid/events" (deleteEventsA context dspConfig)
  put "/kmcs/:kmcUuid/versions/:version" (putVersionA context dspConfig)
   --------------------
   -- PACKAGES
   --------------------
  get "/packages" (getPackagesA context dspConfig)
  get "/packages/:name" (getPackageA context dspConfig)
  delete "/packages/:name" (deletePackagesByNameA context dspConfig)
  delete "/packages/:name/versions/:version" (deletePackageA context dspConfig)
   --------------------
   -- IMPORT/EXPORT
   --------------------
  post "/import" (importA context dspConfig)
  get "/export/:name/:version" (exportA context dspConfig)
   --------------------
   -- ERROR
   --------------------
  notFound notFoundA

runApplication context dspConfig =
  let serverPort = dspConfig ^. dspcfgWebConfig ^. acwPort
  in scotty serverPort (createEndpoints context dspConfig)

--       middleware (authMiddleware dspConfig unauthorizedEndpoints)
createDBConn dspConfig afterSuccess =
  let appConfigDatabase = dspConfig ^. dspcfgDatabaseConfig
      dbHost = appConfigDatabase ^. acdbHost
      dbPort =
        PortNumber (fromInteger (appConfigDatabase ^. acdbPort) :: PortNumber) :: PortID
      dbName = pack (appConfigDatabase ^. acdbDatabaseName)
  in withMongoDBConn dbName dbHost dbPort Nothing 10100 afterSuccess

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
  eitherDspConfig <- loadDSPConfig applicationConfigFile buildInfoFile
  case eitherDspConfig of
    Left (errorDate, reason) -> do
      putStrLn "CONFIG: load failed"
      putStrLn
        "Can't load app-config.cfg or build-info.cfg. Maybe the file is missing or not well-formatted"
      print errorDate
    Right dspConfig -> do
      putStrLn "CONFIG: loaded"
      createDBConn dspConfig $ \dbPool -> do
        putStrLn "DATABASE: connected"
        let context = Context {_ctxDbPool = dbPool, _ctxConfig = Config}
        runMigration context dspConfig
        runApplication context dspConfig
