module Wizard.Api.Handler.User.Tour.Detail_PUT where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common hiding (getCurrentUser)
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Context.BaseContext
import WizardLib.Public.Service.User.Tour.TourService

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> "users"
    :> "current"
    :> "tours"
    :> Capture "tourId" String
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

detail_PUT :: Maybe String -> Maybe String -> String -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
detail_PUT mTokenHeader mServerUrl tourId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< do
        user <- getCurrentUser
        createOrUpdateTour user.uuid tourId
        return NoContent
