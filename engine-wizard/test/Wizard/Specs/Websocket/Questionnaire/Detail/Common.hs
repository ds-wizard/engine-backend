module Wizard.Specs.Websocket.Questionnaire.Detail.Common where

import Control.Lens
import Data.Aeson
import Data.Foldable (traverse_)
import qualified Data.UUID as U
import Network.WebSockets
import System.Timeout
import Test.Hspec.Expectations.Pretty

import LensesConfig hiding (request)
import Shared.Util.JSON
import Shared.Util.String
import Wizard.Api.Resource.Websocket.QuestionnaireActionDTO
import Wizard.Api.Resource.Websocket.WebsocketActionDTO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Model.Context.AppContext
import Wizard.Model.Websocket.WebsocketRecord
import Wizard.Service.Cache.QuestionnaireWebsocketCache
import Wizard.Service.Questionnaire.Collaboration.CollaborationService

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
reqUrlT qtnUuid mUser =
  let suffix =
        case mUser of
          Just user -> "?Authorization=Bearer%20" ++ user
          Nothing -> ""
   in format "/questionnaires/%s/websocket%s" [U.toString qtnUuid, suffix]

-- --------------------------------
-- DATABASE
-- --------------------------------
insertQuestionnaireAndUsers appContext qtn
  -- Prepare DB
 = do
  runInContext U.runMigration appContext
  runInContextIO (insertQuestionnaire qtn) appContext

-- --------------------------------
-- CONNECT
-- --------------------------------
connectTestWebsocketUsers appContext qtnUuid
  -- Clear websockets
 = do
  clearConnections appContext qtnUuid
  -- Connect 1. user
  (c1, s1) <- createConnection appContext (reqUrlT qtnUuid (Just reqAuthToken))
  read_SetUserList c1 0
  -- Connect 2. user
  (c2, s2) <- createConnection appContext (reqUrlT qtnUuid (Just reqNonAdminAuthToken))
  read_SetUserList c1 1
  read_SetUserList c2 1
  -- Connect 3. user
  (c3, s3) <- createConnection appContext (reqUrlT qtnUuid Nothing)
  read_SetUserList c1 2
  read_SetUserList c2 2
  read_SetUserList c3 2
  return ((c1, s1), (c2, s2), (c3, s3))

read_SetUserList connection expConnectionCount = do
  resDto <- receiveData connection
  let eResult = eitherDecode resDto :: Either String (Success_ServerActionDTO ServerQuestionnaireActionDTO)
  let (Right (Success_ServerActionDTO (SetUserList_ServerQuestionnaireActionDTO resConnection))) = eResult
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
clearConnections appContext qtnUuid = do
  (Right records) <- runInContext getAllFromCache appContext
  traverse_ (clearConnection appContext qtnUuid) . filter (\r -> r ^. entityId == U.toString qtnUuid) $ records

clearConnection :: AppContext -> U.UUID -> WebsocketRecord -> IO ()
clearConnection appContext qtnUuid record = do
  runInContext (deleteUser (U.toString qtnUuid) (record ^. connectionUuid)) appContext
  return ()
