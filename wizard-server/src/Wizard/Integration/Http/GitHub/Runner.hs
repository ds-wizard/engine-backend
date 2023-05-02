module Wizard.Integration.Http.GitHub.Runner where

import Control.Monad.Reader (asks)
import qualified Data.UUID as U

import Wizard.Integration.Http.Common.HttpClient
import Wizard.Integration.Http.GitHub.RequestMapper
import Wizard.Integration.Http.GitHub.ResponseMapper
import Wizard.Integration.Resource.GitHub.IssueIDTO
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Service.Config.App.AppConfigService

getIssues :: AppContextM [IssueIDTO]
getIssues = do
  serverConfig <- asks serverConfig
  appConfig <- getAppConfig
  runRequest (toGetIssuesRequest serverConfig.feedback appConfig.questionnaire.feedback) toGetIssuesResponse

createIssue :: String -> U.UUID -> String -> String -> AppContextM IssueIDTO
createIssue pkgId questionUuid title content = do
  serverConfig <- asks serverConfig
  appConfig <- getAppConfig
  runRequest
    ( toCreateIssueRequest
        serverConfig.feedback
        appConfig.questionnaire.feedback
        pkgId
        questionUuid
        title
        content
    )
    toCreateIssueResponse
