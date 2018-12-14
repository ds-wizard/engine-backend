module Service.Branch.BranchValidation where

import Data.Maybe
import Text.Regex

import Localization
import Model.Error.Error
import Model.Error.ErrorHelpers

isValidKmId :: String -> Maybe AppError
isValidKmId kmId =
  if isJust $ matchRegex validationRegex kmId
    then Nothing
    else Just $ createErrorWithFieldError ("kmId", _ERROR_VALIDATION__INVALID_KM_ID_FORMAT)
  where
    validationRegex = mkRegex "^[a-zA-Z0-9][a-zA-Z0-9-]*[a-zA-Z0-9]$"
