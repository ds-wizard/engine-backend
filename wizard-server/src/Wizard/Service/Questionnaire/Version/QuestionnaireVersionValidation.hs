module Wizard.Service.Questionnaire.Version.QuestionnaireVersionValidation where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Data.Maybe (isJust)
import qualified Data.UUID as U

import Shared.Common.Model.Error.Error
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionChangeDTO
import Wizard.Database.DAO.Questionnaire.QuestionnaireEventDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireVersionDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext

validateQuestionnaireVersionCreate :: U.UUID -> QuestionnaireVersionChangeDTO -> AppContextM ()
validateQuestionnaireVersionCreate qtnUuid reqDto = do
  validateQuestionnaireVersionEventExistence reqDto
  validateQuestionnaireVersionUniqueness qtnUuid reqDto

validateQuestionnaireVersionUpdate :: QuestionnaireVersionChangeDTO -> AppContextM ()
validateQuestionnaireVersionUpdate = validateQuestionnaireVersionEventExistence

validateQuestionnaireVersionUniqueness :: U.UUID -> QuestionnaireVersionChangeDTO -> AppContextM ()
validateQuestionnaireVersionUniqueness qtnUuid reqDto = do
  mQtnVersion <- findQuestionnaireVersionByEventUuid' qtnUuid reqDto.eventUuid
  when
    (isJust mQtnVersion)
    (throwError . UserError $ _ERROR_SERVICE_QTN_VERSION__VERSION_UNIQUENESS (U.toString $ reqDto.eventUuid))

validateQuestionnaireVersionEventExistence :: QuestionnaireVersionChangeDTO -> AppContextM ()
validateQuestionnaireVersionEventExistence reqDto =
  findQuestionnaireEventByUuid' reqDto.eventUuid >>= \case
    Just _ -> return ()
    Nothing -> throwError . UserError $ _ERROR_SERVICE_QTN_VERSION__NON_EXISTENT_EVENT_UUID (U.toString $ reqDto.eventUuid)
