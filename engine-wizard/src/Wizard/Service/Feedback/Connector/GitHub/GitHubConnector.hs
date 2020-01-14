module Wizard.Service.Feedback.Connector.GitHub.GitHubConnector where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.UUID as U
import qualified Data.Vector as V
import qualified GitHub as GH
import qualified GitHub.Auth as GA
import qualified GitHub.Data as GD
import qualified GitHub.Data.Issues as GI
import qualified GitHub.Data.Name as GN

import LensesConfig
import Shared.Model.Error.Error
import Wizard.Localization.Messages.Internal
import Wizard.Model.Context.AppContext
import Wizard.Service.Feedback.Connector.Connector
import Wizard.Service.Feedback.Connector.GitHub.GitHubMapper
import Wizard.Util.Logger

instance Connector AppContextM where
  getIssues = do
    appConfig <- asks _appContextApplicationConfig
    let fToken = appConfig ^. feedback . token
    let fOwner = appConfig ^. feedback . owner
    let fRepo = appConfig ^. feedback . repo
    let request = GH.issuesForRepoR (packToName fOwner) (packToName fRepo) (mempty) GD.FetchAll
    eIssues <- liftIO $ GH.executeRequest (GA.OAuth . BS.pack $ fToken) request
    case eIssues of
      Right issues -> return . Right . V.toList $ toSimpleIssue <$> issues
      Left error -> do
        logError . show $ error
        return . Left . GeneralServerError $ _ERROR_SERVICE_FEEDBACK__REQUEST_FAILED "GitHub" "Get issues"
  createIssue packageId questionUuid title content = do
    appConfig <- asks _appContextApplicationConfig
    let fToken = appConfig ^. feedback . token
    let fOwner = appConfig ^. feedback . owner
    let fRepo = appConfig ^. feedback . repo
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
        return . Left . GeneralServerError $ _ERROR_SERVICE_FEEDBACK__REQUEST_FAILED "GitHub" "Create issue"

packToName :: String -> GN.Name a
packToName str = GN.N . T.pack $ str
