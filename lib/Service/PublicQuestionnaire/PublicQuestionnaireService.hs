module Service.PublicQuestionnaire.PublicQuestionnaireService where

import Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U
import Database.DAO.PublicPackage.PublicPackageDAO
import LensesConfig
import Localization
import Model.Context.AppContext
import Model.Error.Error
import Model.Questionnaire.Questionnaire
import Model.Questionnaire.QuestionnaireState
import Service.Common
import Service.KnowledgeModel.KnowledgeModelMapper
import Service.KnowledgeModel.KnowledgeModelService
import Service.Package.PackageMapper

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
            , _questionnaireDetailDTOOwnerUuid = Nothing
            , _questionnaireDetailDTOCreatedAt = now
            , _questionnaireDetailDTOUpdatedAt = now
            }
      Left (NotExistsError _) -> return . Left . NotExistsError $ _ERROR_SERVICE_PQ__NOT_SET_UP
      Left error -> return . Left $ error

-- --------------------------------
-- PRIVATE
-- --------------------------------
heCheckIfPublicQuestionnaireIsEnabled =
  heCheckIfFeatureIsEnabled "PublicQuestionnaire" (general . publicQuestionnaireEnabled)
