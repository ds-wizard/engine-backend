module Wizard.Service.Questionnaire.Version.QuestionnaireVersionValidation where

import Control.Monad.Except (throwError)
import qualified Data.List as L
import qualified Data.UUID as U

import Shared.Common.Model.Common.Lens
import Shared.Common.Model.Error.Error
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionChangeDTO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireEventLenses ()
import Wizard.Model.Questionnaire.QuestionnaireVersion

validateQuestionnaireVersion :: QuestionnaireVersionChangeDTO -> Questionnaire -> AppContextM ()
validateQuestionnaireVersion reqDto qtn = do
  validateQuestionnaireVersionEventExistence reqDto qtn
  validateQuestionnaireVersionUniqueness reqDto qtn

validateQuestionnaireVersionUniqueness :: QuestionnaireVersionChangeDTO -> Questionnaire -> AppContextM ()
validateQuestionnaireVersionUniqueness reqDto qtn =
  case L.find (\v -> v.eventUuid == reqDto.eventUuid) qtn.versions of
    Just _ -> throwError . UserError $ _ERROR_SERVICE_QTN_VERSION__VERSION_UNIQUENESS (U.toString $ reqDto.eventUuid)
    Nothing -> return ()

validateQuestionnaireVersionEventExistence :: QuestionnaireVersionChangeDTO -> Questionnaire -> AppContextM ()
validateQuestionnaireVersionEventExistence reqDto qtn =
  case L.find (\e -> getUuid e == reqDto.eventUuid) qtn.events of
    Just _ -> return ()
    Nothing ->
      throwError . UserError $ _ERROR_SERVICE_QTN_VERSION__NON_EXISTENT_EVENT_UUID (U.toString $ reqDto.eventUuid)
