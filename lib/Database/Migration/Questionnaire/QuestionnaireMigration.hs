module Database.Migration.Questionnaire.QuestionnaireMigration where

import Control.Monad.Logger (logInfo)
import Data.Maybe (fromJust)
import qualified Data.UUID as U

import Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Database.DAO.Questionnaire.QuestionnaireDAO
import Service.Questionnaire.QuestionnaireService

runMigration = do
  $(logInfo) "MIGRATION (Questionnaire/Questionnaire): started"
  deleteQuestionnaires
  createQuestionnaireWithGivenUuid
    (fromJust (U.fromString "d3408c77-d0a0-4349-b93d-929249811d5d"))
    QuestionnaireCreateDTO
    {_questionnaireCreateDTOName = "Isaac", _questionnaireCreateDTOPackageId = "elixir.nl:core-nl:2.0.0"}
  $(logInfo) "MIGRATION (Questionnaire/Questionnaire): ended"
