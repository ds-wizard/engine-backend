module Wizard.Integration.Http.GitHub.RequestMapper where

import Control.Lens ((^.))
import Data.Aeson
import Data.ByteString.Char8 as BS
import Data.ByteString.Lazy.Char8 as BSL
import Data.Map.Strict as M
import qualified Data.UUID as U
import Prelude hiding (lookup)

import LensesConfig
import Shared.Constant.Api
import Wizard.Integration.Resource.GitHub.IssueCreateIDTO
import Wizard.Integration.Resource.GitHub.IssueCreateIJM ()
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Http.HttpRequest
import Wizard.Util.Interpolation (interpolateString)

toGetIssuesRequest :: ServerConfigFeedback -> AppConfigQuestionnaireFeedback -> HttpRequest
toGetIssuesRequest serverConfig appConfig =
  let variables = M.fromList [("owner", appConfig ^. owner), ("repo", appConfig ^. repo)]
   in HttpRequest
        { _httpRequestRequestMethod = "GET"
        , _httpRequestRequestUrl =
            interpolateString variables (serverConfig ^. apiUrl ++ "/repos/${owner}/${repo}/issues")
        , _httpRequestRequestHeaders =
            M.fromList [(authorizationHeaderName, "Bearer " ++ appConfig ^. token), ("User-Agent", "Wizard Server")]
        , _httpRequestRequestBody = BS.empty
        , _httpRequestMultipartFileName = Nothing
        }

toCreateIssueRequest ::
     ServerConfigFeedback -> AppConfigQuestionnaireFeedback -> String -> U.UUID -> String -> String -> HttpRequest
toCreateIssueRequest serverConfig appConfig pkgId questionUuid title content =
  let variables = M.fromList [("owner", appConfig ^. owner), ("repo", appConfig ^. repo)]
   in HttpRequest
        { _httpRequestRequestMethod = "POST"
        , _httpRequestRequestUrl =
            interpolateString variables (serverConfig ^. apiUrl ++ "/repos/${owner}/${repo}/issues")
        , _httpRequestRequestHeaders =
            M.fromList [(authorizationHeaderName, "Bearer " ++ appConfig ^. token), ("User-Agent", "Wizard Server")]
        , _httpRequestRequestBody =
            BSL.toStrict . encode $
            IssueCreateIDTO
              { _issueCreateIDTOTitle = title
              , _issueCreateIDTOBody = content
              , _issueCreateIDTOAssignees = []
              , _issueCreateIDTOMilestone = Nothing
              , _issueCreateIDTOLabels = [pkgId, U.toString questionUuid]
              }
        , _httpRequestMultipartFileName = Nothing
        }
