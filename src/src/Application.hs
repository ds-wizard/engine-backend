module Application where

import Database.Persist.MongoDB (withMongoDBConn)
import Network (PortID(PortNumber))
import Text.Regex
import Web.Scotty

import Api.Handler.Common
import Api.Handler.Token.TokenHandler
import Api.Handler.User.UserHandler
import Api.Middleware.Auth
import Api.Middleware.CORS
import Context
import Migration

unauthorizedEndpoints = [mkRegex "^tokens$"]

runApplication context =
  scotty 3000 $ do
    middleware corsMiddleware
    middleware (authMiddleware "secret-key" unauthorizedEndpoints)
    post "/tokens" (postTokenA context)
    get "/users" (getUsersA context)
    post "/users/" (postUsersA context)
    get "/users/:userUuid" (getUserA context)
    put "/users/:userUuid" (putUserA context)
    delete "/users/:userUuid" (deleteUserA context)
    notFound notFoundA

main = do
  putStrLn "SERVER: started"
  withMongoDBConn "dsp-user-management" "mongo" (PortNumber 27017) Nothing 10100 $ \dbPool -> do
    putStrLn "DATABASE: connected"
    let context = Context {_ctxDbPool = dbPool, _ctxConfig = Config}
    runMigration context
    runApplication context
