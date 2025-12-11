module Wizard.Service.Project.ProjectValidation where

import Control.Monad.Except (throwError)
import Data.Foldable (forM_, traverse_)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Text.Regex.TDFA

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Wizard.Api.Resource.Project.ProjectSettingsChangeDTO
import Wizard.Database.DAO.Project.ProjectMigrationDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext

validateProjectSettingsChangeDTO :: ProjectSettingsChangeDTO -> AppContextM ()
validateProjectSettingsChangeDTO reqDto = validateProjectTags reqDto.projectTags

validateProjectDeletion :: U.UUID -> AppContextM ()
validateProjectDeletion = validateUsageByProjectMigration

validateUsageByProjectMigration :: U.UUID -> AppContextM ()
validateUsageByProjectMigration projectUuid = do
  result <- findProjectMigrationsByOldProjectUuid projectUuid
  case result of
    [] -> return ()
    _ -> throwError . UserError $ _ERROR_SERVICE_PROJECT__PROJECT_CANT_BE_DELETED_BECAUSE_IT_IS_USED_IN_MIGRATION

validateProjectTags :: [String] -> AppContextM ()
validateProjectTags = traverse_ validateProjectTag

validateProjectTag :: String -> AppContextM ()
validateProjectTag tag = forM_ (isValidProjectTag tag) throwError

isValidProjectTag :: String -> Maybe AppError
isValidProjectTag tag =
  if tag =~ "^[^,]+$"
    then Nothing
    else Just $ ValidationError [] (M.singleton "tags" [_ERROR_VALIDATION__FORBIDDEN_CHARACTERS tag])
