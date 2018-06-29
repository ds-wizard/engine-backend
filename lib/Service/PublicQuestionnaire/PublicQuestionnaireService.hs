module Service.PublicQuestionnaire.PublicQuestionnaireService where

import Control.Lens ((^.))

import Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Common.Error
import Database.DAO.Package.PackageDAO
import Database.DAO.PublicQuestionnaire.PublicQuestionnaireDAO
import LensesConfig
import Model.Context.AppContext
import Service.Questionnaire.QuestionnaireMapper

getPublicQuestionnaire :: AppContextM (Either AppError QuestionnaireDetailDTO)
getPublicQuestionnaire =
  heFindPublicQuestionnaire $ \pubQtn ->
    heFindPackageWithEventsById (pubQtn ^. packageId) $ \package -> return . Right $ toDetailDTO pubQtn package
