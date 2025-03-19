module Wizard.Service.DocumentTemplate.Draft.DocumentTemplateDraftValidation where

import Control.Monad (when)
import Control.Monad.Except (throwError)

import Shared.Common.Model.Error.Error
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeDTO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Service.DocumentTemplate.DocumentTemplateValidation (validateDocumentTemplateIdUniqueness)
import WizardLib.Common.Service.Coordinate.CoordinateValidation
import WizardLib.Common.Util.Coordinate
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

validateChangeDto :: DocumentTemplateDraftChangeDTO -> DocumentTemplate -> AppContextM ()
validateChangeDto reqDto tml = do
  let tmlId = buildCoordinate tml.organizationId reqDto.templateId reqDto.version
  validateCoordinateFormat False "templateId" tmlId
  validateCoordinateWithParams tml.tId tml.organizationId tml.templateId tml.version
  when
    (reqDto.templateId /= tml.templateId || reqDto.version /= tml.version)
    (validateDocumentTemplateIdUniqueness tmlId)
  validatePhase tmlId reqDto.phase

validatePhase :: String -> DocumentTemplatePhase -> AppContextM ()
validatePhase tmlId newPhase = do
  when
    (newPhase == DeprecatedDocumentTemplatePhase)
    (throwError . UserError $ _ERROR_VALIDATION__DOC_TML_UNSUPPORTED_STATE tmlId (show newPhase))
