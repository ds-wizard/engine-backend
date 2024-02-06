module Wizard.Integration.Http.GitHub.RequestMapper where

import Data.Aeson
import Data.ByteString.Char8 as BS
import Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Common.Constant.Api
import Shared.Common.Model.Http.HttpRequest
import Shared.Common.Util.String
import Wizard.Integration.Resource.GitHub.IssueCreateIDTO
import Wizard.Integration.Resource.GitHub.IssueCreateIJM ()
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Util.Interpolation (interpolateString)

toGetIssuesRequest :: ServerConfigFeedback -> TenantConfigQuestionnaireFeedback -> HttpRequest
toGetIssuesRequest serverConfig tenantConfig =
  let variables = M.fromList [("owner", tenantConfig.owner), ("repo", tenantConfig.repo)]
   in HttpRequest
        { requestMethod = "GET"
        , requestUrl =
            interpolateString variables (serverConfig.apiUrl ++ "/repos/${owner}/${repo}/issues")
        , requestHeaders =
            M.fromList [(authorizationHeaderName, "Bearer " ++ tenantConfig.token), ("User-Agent", "Wizard Server")]
        , requestBody = BS.empty
        , multipart = Nothing
        }

toCreateIssueRequest :: ServerConfigFeedback -> TenantConfigQuestionnaireFeedback -> String -> U.UUID -> String -> String -> Maybe U.UUID -> HttpRequest
toCreateIssueRequest serverConfig tenantConfig pkgId questionUuid title content mCreatedBy =
  let variables = M.fromList [("owner", tenantConfig.owner), ("repo", tenantConfig.repo)]
   in HttpRequest
        { requestMethod = "POST"
        , requestUrl =
            interpolateString variables (serverConfig.apiUrl ++ "/repos/${owner}/${repo}/issues")
        , requestHeaders =
            M.fromList [(authorizationHeaderName, "Bearer " ++ tenantConfig.token), ("User-Agent", "Wizard Server")]
        , requestBody =
            BSL.toStrict . encode $
              IssueCreateIDTO
                { title = title
                , body = f' "%s\n\n**created by:** %s" [content, maybe "anonymous" U.toString mCreatedBy]
                , assignees = []
                , milestone = Nothing
                , labels = [pkgId, U.toString questionUuid]
                }
        , multipart = Nothing
        }
