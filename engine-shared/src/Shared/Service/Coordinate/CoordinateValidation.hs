module Shared.Service.Coordinate.CoordinateValidation where

import Control.Monad (forM_)
import Control.Monad.Except (MonadError, throwError)
import Data.Maybe
import Text.Regex

import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Util.Coordinate

validateCoordinateFormat :: MonadError AppError m => Bool -> String -> m ()
validateCoordinateFormat allowLatest coordinate =
  let coordinateSplit = splitCoordinate coordinate
   in if length coordinateSplit /= 3 || null (head coordinateSplit) || null (coordinateSplit !! 1)
        then throwError . UserError $ _ERROR_VALIDATION__INVALID_COORDINATE_FORMAT
        else validateVersionFormat allowLatest (coordinateSplit !! 2)

validateCoordinateFormat' :: MonadError AppError m => Bool -> Maybe String -> m ()
validateCoordinateFormat' allowLatest mCoordinate = forM_ mCoordinate (validateCoordinateFormat allowLatest)

validateVersionFormat :: MonadError AppError m => Bool -> String -> m ()
validateVersionFormat allowLatest version
  | allowLatest && version == "latest" = return ()
  | isJust $ matchRegex validationRegex version = return ()
  | otherwise = throwError . UserError $ _ERROR_VALIDATION__INVALID_COORDINATE_VERSION_FORMAT
  where
    validationRegex = mkRegex "^[0-9]+\\.[0-9]+\\.[0-9]+$"

validateCoordinateWithParams :: MonadError AppError m => String -> String -> String -> String -> m ()
validateCoordinateWithParams coordinate organizationId kmOrTemplateId version =
  if coordinate == buildCoordinate organizationId kmOrTemplateId version
    then return ()
    else throwError . UserError $ _ERROR_VALIDATION__COORDINATE_MISMATCH coordinate
