module Wizard.Integration.Http.GitHub.Runner where

import Control.Monad.Reader (asks)
import qualified Data.UUID as U

import Shared.Common.Integration.Http.Common.HttpClient
import Wizard.Api.Resource.User.UserDTO
import Wizard.Integration.Http.GitHub.RequestMapper
import Wizard.Integration.Http.GitHub.ResponseMapper
import Wizard.Integration.Resource.GitHub.IssueIDTO
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Service.Tenant.Config.ConfigService

getIssues :: AppContextM [IssueIDTO]
getIssues = do
  serverConfig <- asks serverConfig
  tcQuestionnaire <- getCurrentTenantConfigQuestionnaire
  runRequest (toGetIssuesRequest serverConfig.feedback tcQuestionnaire.feedback) toGetIssuesResponse

createIssue :: String -> U.UUID -> String -> String -> AppContextM IssueIDTO
createIssue pkgId questionUuid title content = do
  serverConfig <- asks serverConfig
  mCurrentUser <- asks currentUser
  tcQuestionnaire <- getCurrentTenantConfigQuestionnaire
  runRequest
    ( toCreateIssueRequest
        serverConfig.feedback
        tcQuestionnaire.feedback
        pkgId
        questionUuid
        title
        content
        (fmap (.uuid) mCurrentUser)
    )
    toCreateIssueResponse
