module Registry.Service.KnowledgeModel.Package.KnowledgeModelPackageValidation (
  validateIsVersionHigher,
) where

import Shared.Common.Model.Error.Error
import Shared.Coordinate.Util.Coordinate
import Shared.KnowledgeModel.Localization.Messages.Public

validateIsVersionHigher :: String -> String -> Maybe AppError
validateIsVersionHigher newVersion oldVersion =
  if compareVersion newVersion oldVersion == GT
    then Nothing
    else Just . UserError $ _ERROR_SERVICE_PKG__HIGHER_NUMBER_IN_NEW_VERSION
