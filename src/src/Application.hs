module Application where

import Control.Lens ((^.))
import Control.Monad.Except
import Data.Text
import Database.Persist.MongoDB (withMongoDBConn)
import Network
import Text.Regex
import Web.Scotty

import Api.Handler.Common
import Api.Handler.Info.InfoHandler
import Api.Handler.Token.TokenHandler
import Api.Handler.User.UserHandler
import Api.Middleware.Auth
import Api.Middleware.CORS
import DSPConfig
import Context
import Migration

unauthorizedEndpoints = [mkRegex "^$", mkRegex "^tokens$"]

runApplication context dspConfig =
  let serverPort = dspConfig ^. dspcfgWebConfig ^. acwPort
  in scotty serverPort $ do
    middleware corsMiddleware
    middleware (authMiddleware dspConfig unauthorizedEndpoints)
    get "/" (getInfoA context dspConfig)
    post "/tokens" (postTokenA context dspConfig)
    get "/users" (getUsersA context dspConfig)
    post "/users/" (postUsersA context dspConfig)
    get "/users/:userUuid" (getUserA context dspConfig)
    put "/users/:userUuid" (putUserA context dspConfig)
    delete "/users/:userUuid" (deleteUserA context dspConfig)
    notFound notFoundA

createDBConn dspConfig afterSuccess =
  let appConfigDatabase = dspConfig ^. dspcfgDatabaseConfig
      dbHost = appConfigDatabase ^. acdbHost
      dbPort =
        PortNumber (fromInteger (appConfigDatabase ^. acdbPort) :: PortNumber) :: PortID
      dbName = pack (appConfigDatabase ^. acdbDatabaseName)
  in withMongoDBConn dbName dbHost dbPort Nothing 10100 afterSuccess

main = do
  putStrLn "SERVER: started"
  eitherDspConfig <- loadDSPConfig
  case eitherDspConfig of
    Left error ->
      putStrLn
        "Can't load app-config.cfg or build-info.cfg. Maybe the file is missing or not well-formatted"
    Right dspConfig ->
      createDBConn dspConfig $ \dbPool -> do
        putStrLn "DATABASE: connected"
        let context = Context {_ctxDbPool = dbPool, _ctxConfig = Config}
        runMigration context dspConfig
        runApplication context dspConfig
