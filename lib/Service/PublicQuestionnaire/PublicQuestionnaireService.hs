module Service.PublicQuestionnaire.PublicQuestionnaireService where

import Control.Lens ((^.))

import Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Database.DAO.Package.PackageDAO
import Database.DAO.PublicQuestionnaire.PublicQuestionnaireDAO
import LensesConfig
import Model.Context.AppContext
import Model.Error.Error
import Service.Questionnaire.QuestionnaireMapper

getPublicQuestionnaire :: AppContextM (Either AppError QuestionnaireDetailDTO)
getPublicQuestionnaire =
  heFindPublicQuestionnaire $ \pubQtn ->
    heFindPackageWithEventsById (pubQtn ^. packageId) $ \package -> return . Right $ toDetailDTO pubQtn package
