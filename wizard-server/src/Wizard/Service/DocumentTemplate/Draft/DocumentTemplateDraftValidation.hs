module Wizard.Service.DocumentTemplate.Draft.DocumentTemplateDraftValidation where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import qualified Data.UUID as U

import Shared.Common.Model.Error.Error
import Shared.Coordinate.Model.Coordinate.Coordinate
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeDTO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Service.DocumentTemplate.DocumentTemplateValidation (validateDocumentTemplateIdUniqueness)

validateChangeDto :: DocumentTemplateDraftChangeDTO -> DocumentTemplate -> AppContextM ()
validateChangeDto reqDto dt = do
  let newCoordinate = Coordinate dt.organizationId reqDto.templateId reqDto.version
  when
    (reqDto.templateId /= dt.templateId || reqDto.version /= dt.version)
    (validateDocumentTemplateIdUniqueness newCoordinate)
  validatePhase dt.uuid reqDto.phase

validatePhase :: U.UUID -> DocumentTemplatePhase -> AppContextM ()
validatePhase dtUuid newPhase = do
  when
    (newPhase == DeprecatedDocumentTemplatePhase)
    (throwError . UserError $ _ERROR_VALIDATION__DOC_TML_UNSUPPORTED_STATE (U.toString dtUuid) (show newPhase))
