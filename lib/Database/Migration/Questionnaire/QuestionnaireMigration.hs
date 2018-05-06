module Database.Migration.Questionnaire.QuestionnaireMigration where

import Control.Lens ((&), (.~), (^.))
import Control.Monad.Logger (logInfo)
import Control.Monad.Reader (liftIO)
import Data.Maybe (fromJust)
import qualified Data.UUID as U

import Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Database.DAO.Questionnaire.QuestionnaireDAO
import LensesConfig
import Model.Context.AppContext
import Model.Questionnaire.Questionnaire
import Service.Questionnaire.QuestionnaireService

runMigration appContext = do
  $(logInfo) "MIGRATION (Questionnaire/Questionnaire): started"
  let context = appContext ^. oldContext
  let dswConfig = appContext ^. config
  liftIO $ deleteQuestionnaires context
  liftIO $
    createQuestionnaireWithGivenUuid
      context
      dswConfig
      (fromJust (U.fromString "d3408c77-d0a0-4349-b93d-929249811d5d"))
      QuestionnaireCreateDTO
      {_questionnaireCreateDTOName = "Isaac", _questionnaireCreateDTOPackageId = "elixir.nl:core-nl:2.0.0"}
  $(logInfo) "MIGRATION (Questionnaire/Questionnaire): ended"
