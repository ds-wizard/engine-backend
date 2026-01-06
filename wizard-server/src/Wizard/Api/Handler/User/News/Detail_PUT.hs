module Wizard.Api.Handler.User.News.Detail_PUT where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common hiding (getCurrentUser)
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Context.BaseContext
import Wizard.Service.User.News.NewsService

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> "users"
    :> "current"
    :> "news"
    :> Capture "lastSeenNewsId" String
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

detail_PUT :: Maybe String -> Maybe String -> String -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
detail_PUT mTokenHeader mServerUrl lastSeenNewsId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< do
        user <- getCurrentUser
        updateNews user.uuid lastSeenNewsId
        return NoContent
