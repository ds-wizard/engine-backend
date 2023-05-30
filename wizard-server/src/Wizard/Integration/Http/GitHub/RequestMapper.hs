module Wizard.Integration.Http.GitHub.RequestMapper where

import Data.Aeson
import Data.ByteString.Char8 as BS
import Data.ByteString.Lazy.Char8 as BSL
import Data.Map.Strict as M
import qualified Data.UUID as U
import Prelude hiding (lookup)

import Shared.Common.Constant.Api
import Shared.Common.Model.Http.HttpRequest
import Wizard.Integration.Resource.GitHub.IssueCreateIDTO
import Wizard.Integration.Resource.GitHub.IssueCreateIJM ()
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.ServerConfig
import Wizard.Util.Interpolation (interpolateString)

toGetIssuesRequest :: ServerConfigFeedback -> AppConfigQuestionnaireFeedback -> HttpRequest
toGetIssuesRequest serverConfig appConfig =
  let variables = M.fromList [("owner", appConfig.owner), ("repo", appConfig.repo)]
   in HttpRequest
        { requestMethod = "GET"
        , requestUrl =
            interpolateString variables (serverConfig.apiUrl ++ "/repos/${owner}/${repo}/issues")
        , requestHeaders =
            M.fromList [(authorizationHeaderName, "Bearer " ++ appConfig.token), ("User-Agent", "Wizard Server")]
        , requestBody = BS.empty
        , multipart = Nothing
        }

toCreateIssueRequest
  :: ServerConfigFeedback -> AppConfigQuestionnaireFeedback -> String -> U.UUID -> String -> String -> HttpRequest
toCreateIssueRequest serverConfig appConfig pkgId questionUuid title content =
  let variables = M.fromList [("owner", appConfig.owner), ("repo", appConfig.repo)]
   in HttpRequest
        { requestMethod = "POST"
        , requestUrl =
            interpolateString variables (serverConfig.apiUrl ++ "/repos/${owner}/${repo}/issues")
        , requestHeaders =
            M.fromList [(authorizationHeaderName, "Bearer " ++ appConfig.token), ("User-Agent", "Wizard Server")]
        , requestBody =
            BSL.toStrict . encode $
              IssueCreateIDTO
                { title = title
                , body = content
                , assignees = []
                , milestone = Nothing
                , labels = [pkgId, U.toString questionUuid]
                }
        , multipart = Nothing
        }
