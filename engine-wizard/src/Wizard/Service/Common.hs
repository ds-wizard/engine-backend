module Wizard.Service.Common where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)

import Shared.Model.Error.Error
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Service.Config.App.AppConfigService

checkIfAppFeatureIsEnabled featureName accessor = do
  appConfig <- getAppConfig
  if accessor appConfig
    then return ()
    else throwError $ UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ featureName

checkIfServerFeatureIsEnabled featureName accessor = do
  serverConfig <- asks serverConfig
  if accessor serverConfig
    then return ()
    else throwError $ UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ featureName
