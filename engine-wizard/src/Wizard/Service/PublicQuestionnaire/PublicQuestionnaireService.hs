module Wizard.Service.PublicQuestionnaire.PublicQuestionnaireService where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U
import Shared.Model.Error.Error
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Wizard.Database.DAO.PublicPackage.PublicPackageDAO
import Wizard.LensesConfig
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireState
import Wizard.Service.Common
import Wizard.Service.KnowledgeModel.KnowledgeModelMapper
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Package.PackageMapper

getPublicQuestionnaire :: AppContextM (Either AppError QuestionnaireDetailDTO)
getPublicQuestionnaire =
  heCheckIfPublicQuestionnaireIsEnabled $ do
    eitherPackage <- findPublicPackage
    case eitherPackage of
      Right package ->
        heCompileKnowledgeModel (package ^. events) Nothing [] $ \knowledgeModel -> do
          now <- liftIO getCurrentTime
          return . Right $
            QuestionnaireDetailDTO
              { _questionnaireDetailDTOUuid = fromJust . U.fromString $ "a870d5c7-0e0a-4110-95ae-932cb65c6a6a"
              , _questionnaireDetailDTOName = "Public Questionnaire"
              , _questionnaireDetailDTOLevel = 2
              , _questionnaireDetailDTOAccessibility = PublicQuestionnaire
              , _questionnaireDetailDTOState = QSDefault
              , _questionnaireDetailDTOPackage = toSimpleDTO . toPackage $ package
              , _questionnaireDetailDTOSelectedTagUuids = []
              , _questionnaireDetailDTOKnowledgeModel = toKnowledgeModelDTO knowledgeModel
              , _questionnaireDetailDTOReplies = []
              , _questionnaireDetailDTOLabels = []
              , _questionnaireDetailDTOOwnerUuid = Nothing
              , _questionnaireDetailDTOCreatedAt = now
              , _questionnaireDetailDTOUpdatedAt = now
              }
      Left (NotExistsError _) ->
        return . Left . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Public Questionnaire"
      Left error -> return . Left $ error

-- --------------------------------
-- PRIVATE
-- --------------------------------
heCheckIfPublicQuestionnaireIsEnabled =
  heCheckIfFeatureIsEnabled "PublicQuestionnaire" (general . publicQuestionnaireEnabled)
