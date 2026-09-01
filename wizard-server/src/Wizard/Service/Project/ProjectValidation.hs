module Wizard.Service.Project.ProjectValidation where

import Control.Monad.Except (throwError)
import Data.Foldable (forM_, traverse_)
import qualified Data.Map.Strict as M
import Text.Regex.TDFA

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Wizard.Api.Resource.Project.ProjectSettingsChangeDTO
import Wizard.Model.Context.AppContext

validateProjectSettingsChangeDTO :: ProjectSettingsChangeDTO -> AppContextM ()
validateProjectSettingsChangeDTO reqDto = validateProjectTags reqDto.projectTags

validateProjectTags :: [String] -> AppContextM ()
validateProjectTags = traverse_ validateProjectTag

validateProjectTag :: String -> AppContextM ()
validateProjectTag tag = forM_ (isValidProjectTag tag) throwError

isValidProjectTag :: String -> Maybe AppError
isValidProjectTag tag =
  if tag =~ "^[^,]+$"
    then Nothing
    else Just $ ValidationError [] (M.singleton "tags" [_ERROR_VALIDATION__FORBIDDEN_CHARACTERS tag])
