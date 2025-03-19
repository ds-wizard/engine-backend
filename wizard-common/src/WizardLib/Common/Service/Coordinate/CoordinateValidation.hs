module WizardLib.Common.Service.Coordinate.CoordinateValidation where

import Control.Applicative ((<|>))
import Control.Monad (forM_)
import Control.Monad.Except (MonadError, throwError)
import qualified Data.Map.Strict as M
import Data.Maybe
import Text.Regex

import Shared.Common.Model.Error.Error
import WizardLib.Common.Localization.Messages.Public
import WizardLib.Common.Util.Coordinate

validateCoordinateFormat :: MonadError AppError m => Bool -> String -> String -> m ()
validateCoordinateFormat allowLatest entityIdName coordinate = forM_ (isValidCoordinateFormat allowLatest entityIdName coordinate) throwError

isValidCoordinateFormat :: Bool -> String -> String -> Maybe AppError
isValidCoordinateFormat allowLatest entityIdName coordinate =
  let coordinateSplit = splitCoordinate coordinate
   in if length coordinateSplit /= 3 || null (head coordinateSplit) || null (coordinateSplit !! 1)
        then Just . UserError $ _ERROR_VALIDATION__INVALID_COORDINATE_FORMAT
        else
          isValidCoordinatePartFormat "organizationId" (head coordinateSplit)
            <|> isValidCoordinatePartFormat entityIdName (coordinateSplit !! 1)
            <|> isValidVersionFormat allowLatest (coordinateSplit !! 2)

validateCoordinateFormat' :: MonadError AppError m => Bool -> String -> Maybe String -> m ()
validateCoordinateFormat' allowLatest entityIdName mCoordinate = forM_ mCoordinate (validateCoordinateFormat allowLatest entityIdName)

validateCoordinatePartFormat :: MonadError AppError m => String -> String -> m ()
validateCoordinatePartFormat coordinatePartName coordinatePart = forM_ (isValidCoordinatePartFormat coordinatePartName coordinatePart) throwError

isValidCoordinatePartFormat :: String -> String -> Maybe AppError
isValidCoordinatePartFormat coordinatePartName coordinatePart =
  if isJust $ matchRegex validationRegex coordinatePart
    then Nothing
    else Just $ ValidationError [] (M.singleton coordinatePartName [_ERROR_VALIDATION__INVALID_COORDINATE_PART_FORMAT coordinatePartName coordinatePart])
  where
    validationRegex = mkRegex "^[a-zA-Z0-9_.-]+$"

validateVersionFormat :: MonadError AppError m => Bool -> String -> m ()
validateVersionFormat allowLatest version = forM_ (isValidVersionFormat allowLatest version) throwError

isValidVersionFormat :: Bool -> String -> Maybe AppError
isValidVersionFormat allowLatest version
  | allowLatest && version == "latest" = Nothing
  | isJust $ matchRegex validationRegex version = Nothing
  | otherwise = Just . UserError $ _ERROR_VALIDATION__INVALID_COORDINATE_VERSION_FORMAT
  where
    validationRegex = mkRegex "^[0-9]+\\.[0-9]+\\.[0-9]+$"

validateCoordinateWithParams :: MonadError AppError m => String -> String -> String -> String -> m ()
validateCoordinateWithParams coordinate organizationId kmOrTemplateId version = forM_ (isValidCoordinateWithParams coordinate organizationId kmOrTemplateId version) throwError

isValidCoordinateWithParams :: String -> String -> String -> String -> Maybe AppError
isValidCoordinateWithParams coordinate organizationId kmOrTemplateId version =
  if coordinate == buildCoordinate organizationId kmOrTemplateId version
    then Nothing
    else Just . UserError $ _ERROR_VALIDATION__COORDINATE_MISMATCH coordinate
