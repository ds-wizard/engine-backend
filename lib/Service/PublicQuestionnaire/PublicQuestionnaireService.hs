module Service.PublicQuestionnaire.PublicQuestionnaireService where

import Control.Lens ((^.))

import Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Database.DAO.Package.PackageDAO
import Database.DAO.PublicQuestionnaire.PublicQuestionnaireDAO
import LensesConfig
import Localization
import Model.Context.AppContext
import Model.Error.Error
import Service.Questionnaire.QuestionnaireMapper

getPublicQuestionnaire :: AppContextM (Either AppError QuestionnaireDetailDTO)
getPublicQuestionnaire = do
  eitherQuestionnaire <- findPublicQuestionnaire
  case eitherQuestionnaire of
    Right pubQtn ->
      heFindPackageWithEventsById (pubQtn ^. packageId) $ \package -> return . Right $ toDetailDTO pubQtn package
    Left (NotExistsError _) -> return . Left . NotExistsError $ _ERROR_SERVICE_PQ__NOT_SET_UP
    Left error -> return . Left $ error
