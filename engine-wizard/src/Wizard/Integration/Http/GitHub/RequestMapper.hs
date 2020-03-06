module Wizard.Integration.Http.GitHub.RequestMapper where

import Control.Lens ((^.))
import Data.Aeson
import Data.ByteString.Lazy.Char8 as BSL
import Data.Map.Strict as M
import qualified Data.UUID as U
import Prelude hiding (lookup)

import LensesConfig
import Shared.Constant.Api
import Wizard.Integration.Resource.GitHub.IssueCreateIDTO
import Wizard.Integration.Resource.GitHub.IssueCreateIJM ()
import Wizard.Model.Config.AppConfig
import Wizard.Model.Http.HttpRequest
import Wizard.Util.Interpolation (interpolateString)

toGetIssuesRequest :: AppConfigFeedback -> HttpRequest
toGetIssuesRequest feedbackConfig =
  let variables = M.fromList [("owner", feedbackConfig ^. owner), ("repo", feedbackConfig ^. repo)]
   in HttpRequest
        { _httpRequestRequestMethod = "GET"
        , _httpRequestRequestUrl =
            interpolateString variables (feedbackConfig ^. apiUrl ++ "/repos/${owner}/${repo}/issues")
        , _httpRequestRequestHeaders =
            M.fromList
              [(authorizationHeaderName, "Bearer " ++ feedbackConfig ^. token), ("User-Agent", "Wizard Server")]
        , _httpRequestRequestBody = ""
        }

toCreateIssueRequest :: AppConfigFeedback -> String -> U.UUID -> String -> String -> HttpRequest
toCreateIssueRequest feedbackConfig pkgId questionUuid title content =
  let variables = M.fromList [("owner", feedbackConfig ^. owner), ("repo", feedbackConfig ^. repo)]
   in HttpRequest
        { _httpRequestRequestMethod = "POST"
        , _httpRequestRequestUrl =
            interpolateString variables (feedbackConfig ^. apiUrl ++ "/repos/${owner}/${repo}/issues")
        , _httpRequestRequestHeaders =
            M.fromList
              [(authorizationHeaderName, "Bearer " ++ feedbackConfig ^. token), ("User-Agent", "Wizard Server")]
        , _httpRequestRequestBody =
            BSL.unpack . encode $
            IssueCreateIDTO
              { _issueCreateIDTOTitle = title
              , _issueCreateIDTOBody = content
              , _issueCreateIDTOAssignees = []
              , _issueCreateIDTOMilestone = Nothing
              , _issueCreateIDTOLabels = [pkgId, U.toString questionUuid]
              }
        }
