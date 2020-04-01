module Wizard.Service.PublicQuestionnaire.PublicQuestionnaireService where

import Control.Lens ((^.))
import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO)
import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Error.Error
import Shared.Service.Package.PackageMapper
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Wizard.Database.DAO.PublicPackage.PublicPackageDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireState
import Wizard.Service.Common
import Wizard.Service.KnowledgeModel.KnowledgeModelMapper
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Package.PackageMapper

getPublicQuestionnaire :: AppContextM QuestionnaireDetailDTO
getPublicQuestionnaire = do
  checkIfPublicQuestionnaireIsEnabled
  mPkg <- findPublicPackage'
  case mPkg of
    Just package -> do
      knowledgeModel <- compileKnowledgeModel (package ^. events) Nothing []
      now <- liftIO getCurrentTime
      return
        QuestionnaireDetailDTO
          { _questionnaireDetailDTOUuid = fromJust . U.fromString $ "a870d5c7-0e0a-4110-95ae-932cb65c6a6a"
          , _questionnaireDetailDTOName = "Public Questionnaire"
          , _questionnaireDetailDTOLevel = 2
          , _questionnaireDetailDTOAccessibility = PublicQuestionnaire
          , _questionnaireDetailDTOState = QSDefault
          , _questionnaireDetailDTOPackage = toSimpleDTO . toPackage $ package
          , _questionnaireDetailDTOSelectedTagUuids = []
          , _questionnaireDetailDTOTemplateUuid = Nothing
          , _questionnaireDetailDTOFormatUuid = Nothing
          , _questionnaireDetailDTOKnowledgeModel = toKnowledgeModelDTO knowledgeModel
          , _questionnaireDetailDTOReplies = []
          , _questionnaireDetailDTOLabels = []
          , _questionnaireDetailDTOOwnerUuid = Nothing
          , _questionnaireDetailDTOCreatedAt = now
          , _questionnaireDetailDTOUpdatedAt = now
          }
    Nothing -> throwError . UserError $ _ERROR_SERVICE_PUBLIC_QTN__PUBLIC_QTN_NOT_FOUND_IN_DB

-- --------------------------------
-- PRIVATE
-- --------------------------------
checkIfPublicQuestionnaireIsEnabled =
  checkIfAppFeatureIsEnabled "PublicQuestionnaire" (questionnaire . publicQuestionnaire . enabled)
