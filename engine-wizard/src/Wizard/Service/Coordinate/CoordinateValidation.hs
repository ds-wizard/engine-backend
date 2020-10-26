module Wizard.Service.Coordinate.CoordinateValidation where

import Control.Monad (forM_)
import Control.Monad.Except (throwError)
import Data.Maybe
import Text.Regex

import Shared.Model.Error.Error
import Shared.Util.Coordinate
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext

validateCoordinateFormat :: String -> AppContextM ()
validateCoordinateFormat coordinate =
  let coordinateSplit = splitCoordinate coordinate
   in if length coordinateSplit /= 3 || null (head coordinateSplit) || null (coordinateSplit !! 1)
        then throwError . UserError $ _ERROR_VALIDATION__INVALID_COORDINATE_FORMAT
        else validateVersionFormat (coordinateSplit !! 2)

validateCoordinateFormat' :: Maybe String -> AppContextM ()
validateCoordinateFormat' mCoordinate = forM_ mCoordinate validateCoordinateFormat

validateVersionFormat :: String -> AppContextM ()
validateVersionFormat version =
  if isJust $ matchRegex validationRegex version
    then return ()
    else throwError . UserError $ _ERROR_VALIDATION__INVALID_COORDINATE_VERSION_FORMAT
  where
    validationRegex = mkRegex "^[0-9]+\\.[0-9]+\\.[0-9]+$"

validateCoordinateWithParams :: String -> String -> String -> String -> AppContextM ()
validateCoordinateWithParams coordinate organizationId kmOrTemplateId version =
  if coordinate == buildCoordinate organizationId kmOrTemplateId version
    then return ()
    else throwError . UserError $ _ERROR_VALIDATION__COORDINATE_MISMATCH coordinate
