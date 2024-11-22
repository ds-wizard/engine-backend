module Shared.Common.Service.Tenant.Limit.LimitService where

import Control.Monad (when)
import Control.Monad.Except (throwError)

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Context.AppContext
import Shared.Common.Model.Error.Error

checkLimit :: (AppContextC s sc m, Show number, Ord number) => String -> number -> number -> m ()
checkLimit name count maxCount =
  when (count >= maxCount) (throwError . UserError $ _ERROR_SERVICE_TENANT__LIMIT_EXCEEDED name count maxCount)
