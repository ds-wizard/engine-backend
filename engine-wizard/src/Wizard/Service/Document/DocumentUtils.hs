module Wizard.Service.Document.DocumentUtils where

import Control.Lens ((^.))
import Control.Monad.Except (catchError)
import qualified Data.UUID as U

import LensesConfig hiding (hash)
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Model.Context.AppContext
import Wizard.Model.Document.Document
import Wizard.Service.Document.DocumentMapper
import Wizard.Service.Questionnaire.QuestionnaireService
import Wizard.Service.Template.TemplateService

enhanceDocument :: Document -> AppContextM DocumentDTO
enhanceDocument doc = do
  tml <- getTemplateByUuidAndPackageId (doc ^. templateId) Nothing
  mQtn <- catchError (getQuestionnaireById' (U.toString $ doc ^. questionnaireUuid)) (\_ -> return Nothing)
  return $ toDTO doc mQtn tml
