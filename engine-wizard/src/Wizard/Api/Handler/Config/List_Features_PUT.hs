module Wizard.Api.Handler.Config.List_Features_PUT where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Config.AppConfigDTO
import Wizard.Api.Resource.Config.AppConfigJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Config.AppConfigService

type List_Features_PUT
   = Header "Authorization" String
     :> ReqBody '[ SafeJSON] AppConfigFeaturesDTO
     :> "configs"
     :> "features"
     :> Put '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] AppConfigFeaturesDTO)

list_features_PUT ::
     Maybe String
  -> AppConfigFeaturesDTO
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] AppConfigFeaturesDTO)
list_features_PUT mTokenHeader reqDto =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      checkPermission mTokenHeader "CFG_PERM"
      modifyAppConfigFeatures reqDto
