module Service.Common
  ( heCheckIfFeatureIsEnabled
  , hmCheckIfFeatureIsEnabled
  ) where

import Control.Lens ((^.))
import Control.Monad.Reader (asks)

import Localization.Messages.Public
import Model.Context.AppContext
import Model.Error.Error

heCheckIfFeatureIsEnabled featureName accessor callback = do
  dswConfig <- asks _appContextAppConfig
  if dswConfig ^. accessor
    then callback
    else return . Left . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ featureName

hmCheckIfFeatureIsEnabled featureName accessor callback = do
  dswConfig <- asks _appContextAppConfig
  if dswConfig ^. accessor
    then callback
    else return . Just . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ featureName
