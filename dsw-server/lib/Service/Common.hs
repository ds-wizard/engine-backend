module Service.Common
  ( heCheckIfFeatureIsEnabled
  , hmCheckIfFeatureIsEnabled
  ) where

import Control.Lens ((^.))
import Control.Monad.Reader (asks)

import Localization
import Model.Context.AppContext
import Model.Error.ErrorHelpers

heCheckIfFeatureIsEnabled featureName accessor callback = do
  dswConfig <- asks _appContextAppConfig
  if dswConfig ^. accessor
    then callback
    else return . Left . createErrorWithErrorMessage . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ featureName

hmCheckIfFeatureIsEnabled featureName accessor callback = do
  dswConfig <- asks _appContextAppConfig
  if dswConfig ^. accessor
    then callback
    else return . Just . createErrorWithErrorMessage . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ featureName
