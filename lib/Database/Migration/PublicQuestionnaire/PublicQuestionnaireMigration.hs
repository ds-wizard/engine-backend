module Database.Migration.PublicQuestionnaire.PublicQuestionnaireMigration where

import Control.Monad.Logger (logInfo)

import Database.DAO.PublicQuestionnaire.PublicQuestionnaireDAO
import Database.Migration.PublicQuestionnaire.Data.PublicQuestionnaires

runMigration = do
  $(logInfo) "MIGRATION (PublicQuestionnaire/PublicQuestionnaire): started"
  deletePublicQuestionnaires
  insertPublicQuestionnaire publicQuestionnaire
  $(logInfo) "MIGRATION (PublicQuestionnaire/PublicQuestionnaire): ended"
