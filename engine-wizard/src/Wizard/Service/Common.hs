module Wizard.Service.Common
  ( heCheckIfFeatureIsEnabled
  , hmCheckIfFeatureIsEnabled
  ) where

import Control.Lens ((^.))
import Control.Monad.Reader (asks)

import Shared.Model.Error.Error
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext

heCheckIfFeatureIsEnabled featureName accessor callback = do
  appConfig <- asks _appContextApplicationConfig
  if appConfig ^. accessor
    then callback
    else return . Left . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ featureName

hmCheckIfFeatureIsEnabled featureName accessor callback = do
  appConfig <- asks _appContextApplicationConfig
  if appConfig ^. accessor
    then callback
    else return . Just . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ featureName
