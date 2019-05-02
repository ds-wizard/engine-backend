module Service.Feedback.Connector.GitHub.GitHubConnector where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.UUID as U
import qualified Data.Vector as V
import qualified GitHub as GH
import qualified GitHub.Auth as GA
import qualified GitHub.Data as GD
import qualified GitHub.Data.Issues as GI
import qualified GitHub.Data.Name as GN

import LensesConfig
import Localization
import Model.Context.AppContext
import Model.Error.Error
import Service.Feedback.Connector.Connector
import Service.Feedback.Connector.GitHub.GitHubMapper
import Util.Logger

instance Connector AppContextM where
  getIssues = do
    dswConfig <- asks _appContextAppConfig
    let fToken = fromMaybe "" $ dswConfig ^. feedback . token
    let fOwner = fromMaybe "" $ dswConfig ^. feedback . owner
    let fRepo = fromMaybe "" $ dswConfig ^. feedback . repo
    let request = GH.issuesForRepoR (packToName fOwner) (packToName fRepo) (mempty) GD.FetchAll
    eIssues <- liftIO $ GH.executeRequest (GA.OAuth . BS.pack $ fToken) request
    case eIssues of
      Right issues -> return . Right . V.toList $ toSimpleIssue <$> issues
      Left error -> do
        logError . show $ error
        return . Left . HttpClientError $ _ERROR_HTTP_CLIENT__REQUEST_FAILED "GitHub" "Get issues"
  createIssue packageId questionUuid title content = do
    dswConfig <- asks _appContextAppConfig
    let fToken = fromMaybe "" $ dswConfig ^. feedback . token
    let fOwner = fromMaybe "" $ dswConfig ^. feedback . owner
    let fRepo = fromMaybe "" $ dswConfig ^. feedback . repo
    let newIssue =
          GI.NewIssue
          { newIssueTitle = T.pack title
          , newIssueBody = Just . T.pack $ content
          , newIssueAssignees = V.empty
          , newIssueMilestone = Nothing
          , newIssueLabels = Just . V.fromList $ [(packToName packageId), (packToName . U.toString $ questionUuid)]
          }
    let request = GH.createIssueR (packToName fOwner) (packToName fRepo) newIssue
    eIssue <- liftIO $ GH.executeRequest (GA.OAuth . BS.pack $ fToken) request
    case eIssue of
      Right issue -> return . Right . GD.unIssueNumber . GI.issueNumber $ issue
      Left error -> do
        logError . show $ error
        return . Left . HttpClientError $ _ERROR_HTTP_CLIENT__REQUEST_FAILED "GitHub" "Create issue"

packToName :: String -> GN.Name a
packToName str = GN.N . T.pack $ str
