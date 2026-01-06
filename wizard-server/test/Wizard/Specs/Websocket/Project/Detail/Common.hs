module Wizard.Specs.Websocket.Project.Detail.Common where

import qualified Control.Exception.Base as E
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import qualified Network.HTTP.Client as HC
import Network.WebSockets
import qualified Network.Wreq as W
import qualified Network.Wreq.Types as WT
import System.Timeout
import Test.Hspec.Expectations.Pretty

import Shared.Common.Integration.Http.Common.HttpClient (mapHeader)
import Shared.Common.Integration.Http.Common.HttpClientFactory
import Shared.Common.Model.Http.HttpRequest
import Shared.Common.Util.JSON
import Shared.Common.Util.String
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Wizard.Api.Resource.Websocket.ProjectMessageDTO
import Wizard.Api.Resource.Websocket.WebsocketActionDTO
import Wizard.Cache.ProjectWebsocketCache
import Wizard.Database.DAO.Project.ProjectDAO
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Websocket.WebsocketRecord
import Wizard.Service.Project.Collaboration.ProjectCollaborationService

import Wizard.Specs.API.Common
import Wizard.Specs.Common
import Wizard.Specs.Websocket.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertCountOfWebsocketConnection appContext expCount = do
  (Right resCount) <- runInContext countCache appContext
  resCount `shouldBe` expCount

-- --------------------------------
-- URL
-- --------------------------------
reqUrlT projectUuid mUser =
  let suffix =
        case mUser of
          Just user -> "?Authorization=Bearer%20" ++ user
          Nothing -> ""
   in f' "/wizard-api/projects/%s/websocket%s" [U.toString projectUuid, suffix]

-- --------------------------------
-- DATABASE
-- --------------------------------
insertProjectAndUsers appContext project =
  -- Prepare DB
  do
    runInContext U.runMigration appContext
    runInContextIO TML_Migration.runMigration appContext
    runInContextIO (insertPackage germanyKmPackage) appContext
    runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) appContext
    runInContextIO (insertProject project) appContext

-- --------------------------------
-- CONNECT
-- --------------------------------
connectTestWebsocketUsers appContext projectUuid =
  -- Clear websockets
  do
    clearConnections appContext projectUuid
    -- Connect 1. user
    (c1, s1) <- createConnection appContext (reqUrlT projectUuid (Just reqAuthToken))
    read_SetUserList c1 0
    -- Connect 2. user
    (c2, s2) <- createConnection appContext (reqUrlT projectUuid (Just reqNonAdminAuthToken))
    read_SetUserList c1 1
    read_SetUserList c2 1
    -- Connect 3. user
    (c3, s3) <- createConnection appContext (reqUrlT projectUuid Nothing)
    read_SetUserList c1 2
    read_SetUserList c2 2
    read_SetUserList c3 2
    return ((c1, s1), (c2, s2), (c3, s3))

read_SetUserList connection expConnectionCount = do
  resDto <- receiveData connection
  let eResult = eitherDecode resDto :: Either String (Success_ServerActionDTO ServerProjectMessageDTO)
  let (Right (Success_ServerActionDTO (SetUserList_ServerProjectMessageDTO resConnection))) = eResult
  length resConnection `shouldBe` expConnectionCount

read_Error connection expError = do
  resDto <- receiveData connection
  let eResult = eitherDecode resDto :: Either String Error_ServerActionDTO
  let (Right (Error_ServerActionDTO error)) = eResult
  error `shouldBe` expError

read_SetUserList_or_Error connection expError = do
  resDto <- receiveData connection
  let (Right result) = eitherDecode resDto :: Either String Object
  let resultType = getField "type" result return
  case resultType of
    (Right "Success_ServerAction") -> read_Error connection expError
    (Right "Error_ServerAction") -> do
      let eResult = eitherDecode resDto :: Either String Error_ServerActionDTO
      let (Right (Error_ServerActionDTO error)) = eResult
      error `shouldBe` expError
    rest -> print rest

nothingWasReceived connection = do
  maybeResDto <- timeout 1000 (receive connection)
  maybeResDto `shouldBe` Nothing

-- --------------------------------
-- DISCONNECT
-- --------------------------------
clearConnections :: AppContext -> U.UUID -> IO ()
clearConnections appContext projectUuid = do
  (Right records) <- runInContext getAllFromCache appContext
  traverse_ (clearConnection appContext projectUuid) . filter (\r -> r.entityId == U.toString projectUuid) $ records

clearConnection :: AppContext -> U.UUID -> WebsocketRecord -> IO ()
clearConnection appContext projectUuid record = do
  runInContext (deleteUser projectUuid record.connectionUuid) appContext
  return ()

-- --------------------------------
-- HTTP Client
-- --------------------------------
runSimpleRequest :: AppContext -> HttpRequest -> IO (Either E.SomeException (HC.Response BSL.ByteString))
runSimpleRequest appContext req = do
  httpClientManager <- createHttpClientManager appContext.serverConfig.logging
  let opts =
        W.defaults
          { WT.manager = Right httpClientManager
          , WT.headers = reqHeaders
          , WT.checkResponse = Just (\_ _ -> return ())
          }
  E.try . action $ opts
  where
    reqMethod = req.requestMethod
    host =
      if appContext.serverConfig.general.serverPort == 80
        then "localhost"
        else "localhost:" ++ show appContext.serverConfig.general.serverPort
    reqUrl = f' "http://%s/%s" [host, req.requestUrl]
    reqHeaders = mapHeader <$> M.toList req.requestHeaders
    action opts
      | reqMethod == "GET" = W.getWith opts reqUrl
      | otherwise = W.customPayloadMethodWith reqMethod opts reqUrl req.requestBody
