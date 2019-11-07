module Service.Package.PackageValidation
  ( validateIsVersionHigher
  ) where

import Localization
import Model.Error.Error
import Model.Error.ErrorHelpers
import Service.Package.PackageUtils

validateIsVersionHigher :: String -> String -> Maybe AppError
validateIsVersionHigher newVersion oldVersion =
  if compareVersion newVersion oldVersion == GT
    then Nothing
    else Just . createErrorWithErrorMessage $ _ERROR_SERVICE_PKG__HIGHER_NUMBER_IN_NEW_VERSION
