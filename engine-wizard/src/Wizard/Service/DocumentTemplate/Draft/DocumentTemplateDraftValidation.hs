module Wizard.Service.DocumentTemplate.Draft.DocumentTemplateDraftValidation where

import Control.Monad (when)
import Control.Monad.Except (throwError)

import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.Error.Error
import Shared.Service.Coordinate.CoordinateValidation
import Shared.Util.Coordinate
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeDTO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Service.DocumentTemplate.DocumentTemplateValidation (validateDocumentTemplateIdUniqueness)

validateChangeDto :: DocumentTemplateDraftChangeDTO -> DocumentTemplate -> AppContextM ()
validateChangeDto reqDto tml = do
  let tmlId = buildCoordinate tml.organizationId reqDto.templateId reqDto.version
  validateCoordinateFormat False tmlId
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
