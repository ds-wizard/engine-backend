module Registry.Service.Package.PackageValidation (
  validateIsVersionHigher,
) where

import Shared.Common.Model.Error.Error
import WizardLib.Common.Util.Coordinate
import WizardLib.KnowledgeModel.Localization.Messages.Public

validateIsVersionHigher :: String -> String -> Maybe AppError
validateIsVersionHigher newVersion oldVersion =
  if compareVersion newVersion oldVersion == GT
    then Nothing
    else Just . UserError $ _ERROR_SERVICE_PKG__HIGHER_NUMBER_IN_NEW_VERSION
