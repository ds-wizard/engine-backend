module Registry.Service.Package.PackageValidation
  ( validateIsVersionHigher
  ) where

import Registry.Service.Package.PackageUtil
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error

validateIsVersionHigher :: String -> String -> Maybe AppError
validateIsVersionHigher newVersion oldVersion =
  if compareVersion newVersion oldVersion == GT
    then Nothing
    else Just . UserError $ _ERROR_SERVICE_PKG__HIGHER_NUMBER_IN_NEW_VERSION
