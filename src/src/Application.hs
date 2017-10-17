module Application where

import Database.Persist.MongoDB (withMongoDBConn)
import Network (PortID(PortNumber))
import Web.Scotty

import Api.Handler.Common
import Api.Handler.User.UserHandler
import Context

runApplication dbPool = do
  let context = Context {_ctxDbPool = dbPool, _ctxConfig = Config}
  scotty 3000 $ do
    get "/users" (getUsersA context)
    post "/users/" (postUsersA context)
    get "/users/:userUuid" (getUserA context)
    -- put "/users/:userUuid" (putUserA context)
    -- delete "/users/:userUuid" (deleteUserA context)
    notFound notFoundA

main = do
  putStrLn "SERVER: started"
  withMongoDBConn
    "dsp-user-management-2"
    "localhost"
    (PortNumber 27017)
    Nothing
    10100 $ \dbPool -> do
    putStrLn "DATABASE: connected"
    runApplication dbPool
