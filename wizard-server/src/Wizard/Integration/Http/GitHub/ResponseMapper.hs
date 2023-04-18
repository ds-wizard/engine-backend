module Wizard.Integration.Http.GitHub.ResponseMapper where

import qualified Data.ByteString.Lazy as BSL
import Network.Wreq (Response)
import Prelude hiding (lookup)

import Shared.Common.Model.Error.Error
import Wizard.Integration.Http.Common.ResponseMapper
import Wizard.Integration.Resource.GitHub.IssueIDTO
import Wizard.Integration.Resource.GitHub.IssueIJM ()

toGetIssuesResponse :: Response BSL.ByteString -> Either AppError [IssueIDTO]
toGetIssuesResponse = extractResponseBody

toCreateIssueResponse :: Response BSL.ByteString -> Either AppError IssueIDTO
toCreateIssueResponse = extractResponseBody
