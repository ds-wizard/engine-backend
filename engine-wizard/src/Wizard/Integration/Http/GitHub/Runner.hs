module Wizard.Integration.Http.GitHub.Runner where

import Control.Lens ((^.))
import Control.Monad.Reader (asks)
import qualified Data.UUID as U

import LensesConfig
import Wizard.Integration.Http.Common.HttpClient
import Wizard.Integration.Http.GitHub.RequestMapper
import Wizard.Integration.Http.GitHub.ResponseMapper
import Wizard.Integration.Resource.GitHub.IssueIDTO
import Wizard.Model.Context.AppContext

getIssues :: AppContextM [IssueIDTO]
getIssues = do
  appConfig <- asks _appContextApplicationConfig
  runRequest (toGetIssuesRequest (appConfig ^. feedback)) toGetIssuesResponse

createIssue :: String -> U.UUID -> String -> String -> AppContextM IssueIDTO
createIssue pkgId questionUuid title content = do
  appConfig <- asks _appContextApplicationConfig
  runRequest (toCreateIssueRequest (appConfig ^. feedback) pkgId questionUuid title content) toCreateIssueResponse
