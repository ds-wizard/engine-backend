module Wizard.Specs.Websocket.KnowledgeModelEditor.Detail.Common where

import Data.Aeson
import Data.Foldable (traverse_)
import Data.Maybe (fromJust)
import qualified Data.UUID as U
import Network.WebSockets
import System.Timeout
import Test.Hspec.Expectations.Pretty

import Shared.Common.Util.JSON
import Shared.Common.Util.String
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Wizard.Api.Resource.Websocket.KnowledgeModelEditorActionDTO
import Wizard.Api.Resource.Websocket.WebsocketActionDTO
import Wizard.Cache.KnowledgeModelEditorWebsocketCache
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Wizard.Model.Websocket.WebsocketRecord
import Wizard.Service.KnowledgeModel.Editor.Collaboration.CollaborationService
import Wizard.Service.KnowledgeModel.Editor.EditorService

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
reqUrlT bUuid mUser =
  let suffix =
        case mUser of
          Just user -> "?Authorization=Bearer%20" ++ user
          Nothing -> ""
   in f' "/wizard-api/knowledge-model-editors/%s/websocket%s" [U.toString bUuid, suffix]

-- --------------------------------
-- DATABASE
-- --------------------------------
insertKnowledgeModelEditorAndUsers appContext editor =
  -- Prepare DB
  do
    runInContext U.runMigration appContext
    runInContextIO TML_Migration.runMigration appContext
    runInContextIO (insertPackage germanyKmPackage) appContext
    runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) appContext
    runInContextIO (insertKnowledgeModelEditor editor) appContext
    runInContextIO
      ( createEditorWithParams
          leidenKnowledgeModelEditor.uuid
          leidenKnowledgeModelEditor.createdAt
          (fromJust appContext.currentUser)
          leidenKnowledgeModelEditorCreate
      )
      appContext

-- --------------------------------
-- CONNECT
-- --------------------------------
connectTestWebsocketUsers appContext bUuid =
  -- Clear websockets
  do
    clearConnections appContext bUuid
    -- Connect 1. user
    (c1, s1) <- createConnection appContext (reqUrlT bUuid (Just reqAuthToken))
    read_SetUserList c1 0
    -- Connect 2. user
    (c2, s2) <- createConnection appContext (reqUrlT bUuid (Just reqNonAdminAuthToken))
    read_SetUserList c1 1
    read_SetUserList c2 1
    return ((c1, s1), (c2, s2))

read_SetUserList connection expConnectionCount = do
  resDto <- receiveData connection
  let eResult = eitherDecode resDto :: Either String (Success_ServerActionDTO ServerKnowledgeModelEditorActionDTO)
  let (Right (Success_ServerActionDTO (SetUserList_ServerKnowledgeModelEditorActionDTO resConnection))) = eResult
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
clearConnections appContext bUuid = do
  (Right records) <- runInContext getAllFromCache appContext
  traverse_ (clearConnection appContext bUuid) . filter (\r -> r.entityId == U.toString bUuid) $ records

clearConnection :: AppContext -> U.UUID -> WebsocketRecord -> IO ()
clearConnection appContext bUuid record = do
  runInContext (deleteUser bUuid record.connectionUuid) appContext
  return ()
