module Registry.Service.Common where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)

import Registry.Model.Context.AppContext
import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error

checkIfServerFeatureIsEnabled featureName accessor = do
  serverConfig <- asks serverConfig
  if accessor serverConfig
    then return ()
    else throwError $ UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ featureName
