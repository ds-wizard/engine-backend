module Service.Feedback.Connector.GitHub.GitHubConnector where

import Control.Lens ((^.))
import Control.Monad.Logger
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.UUID as U
import qualified Data.Vector as V
import qualified GitHub as GH
import qualified GitHub.Auth as GA
import qualified GitHub.Data.Issues as GI
import qualified GitHub.Data.Name as GN

import Common.Error
import Common.Localization
import LensesConfig
import Model.Context.AppContext
import Service.Feedback.Connector.Connector

instance Connector AppContextM where
  createIssue packageId questionUuid title content = do
    dswConfig <- asks _appContextConfig
    let fToken = dswConfig ^. feedback . token
    let fOwner = dswConfig ^. feedback . owner
    let fRepo = dswConfig ^. feedback . repo
    let newIssue =
          GI.NewIssue
          { newIssueTitle = T.pack title
          , newIssueBody = Just . T.pack $ content
          , newIssueAssignee = Nothing
          , newIssueMilestone = Nothing
          , newIssueLabels = Just . V.fromList $ [(packToName packageId), (packToName . U.toString $ questionUuid)]
          }
    let request = GH.createIssueR (packToName fOwner) (packToName fRepo) newIssue
    eIssue <- liftIO $ GH.executeRequest (GA.OAuth . BS.pack $ fToken) request
    case eIssue of
      Right issue -> return . Right . GH.untagId . GI.issueId $ issue
      Left error -> do
        $(logError) . T.pack . show $ error
        return . Left . HttpClientError $ _ERROR_HTTP_CLIENT__REQUEST_FAILED "GitHub" "Create issue"

packToName :: String -> GN.Name a
packToName str = GN.N . T.pack $ str
