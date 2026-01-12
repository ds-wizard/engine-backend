module Wizard.Specs.API.User.PluginSettings.Detail_PUT (
  detail_PUT,
) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Database.DAO.User.UserPluginSettingsDAO
import Wizard.Database.Migration.Development.Plugin.Data.Plugins
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Context.AppContext
import Wizard.Model.Plugin.Plugin
import Wizard.Model.User.UserPluginSettings

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Config.Common
import Wizard.Specs.API.User.PluginSettings.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/users/current/plugin-settings/{uuid}
-- ------------------------------------------------------------------------
detail_PUT :: AppContext -> SpecWith ((), Application)
detail_PUT appContext =
  describe "PUT /wizard-api/users/current/plugin-settings/{uuid}" $ do
    test_200 appContext
    test_401 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = BS.pack $ "/wizard-api/users/current/plugin-settings/" ++ U.toString plugin1.uuid

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = userAlbertPluginSettingsEdited.values

reqBody = A.encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = userAlbertPluginSettingsEdited.values
      -- AND: Run migrations
      runInContextIO (insertUserPluginSettings userAlbertPluginSettings) appContext
      runInContextIO (insertUserPluginSettings userCharlesPluginSettings) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, A.Value)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      compareDtos resBody expDto
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfUserPluginSettingsInDB appContext userAlbertPluginSettingsEdited

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody
