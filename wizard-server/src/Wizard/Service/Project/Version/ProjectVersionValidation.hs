module Wizard.Service.Project.Version.ProjectVersionValidation where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Data.Maybe (isJust)
import qualified Data.UUID as U

import Shared.Common.Model.Error.Error
import Wizard.Api.Resource.Project.Version.ProjectVersionChangeDTO
import Wizard.Database.DAO.Project.ProjectEventDAO
import Wizard.Database.DAO.Project.ProjectVersionDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext

validateProjectVersionCreate :: U.UUID -> ProjectVersionChangeDTO -> AppContextM ()
validateProjectVersionCreate projectUuid reqDto = do
  validateProjectVersionEventExistence reqDto
  validateProjectVersionUniqueness projectUuid reqDto

validateProjectVersionUpdate :: ProjectVersionChangeDTO -> AppContextM ()
validateProjectVersionUpdate = validateProjectVersionEventExistence

validateProjectVersionUniqueness :: U.UUID -> ProjectVersionChangeDTO -> AppContextM ()
validateProjectVersionUniqueness projectUuid reqDto = do
  mProjectVersion <- findProjectVersionByEventUuid' projectUuid reqDto.eventUuid
  when
    (isJust mProjectVersion)
    (throwError . UserError $ _ERROR_SERVICE_PROJECT_VERSION__VERSION_UNIQUENESS (U.toString $ reqDto.eventUuid))

validateProjectVersionEventExistence :: ProjectVersionChangeDTO -> AppContextM ()
validateProjectVersionEventExistence reqDto =
  findProjectEventByUuid' reqDto.eventUuid >>= \case
    Just _ -> return ()
    Nothing -> throwError . UserError $ _ERROR_SERVICE_PROJECT_VERSION__NON_EXISTENT_EVENT_UUID (U.toString $ reqDto.eventUuid)
