module Database.Migration.Development.PublicQuestionnaire.PublicQuestionnaireMigration where

import Database.DAO.PublicQuestionnaire.PublicQuestionnaireDAO
import Database.Migration.Development.PublicQuestionnaire.Data.PublicQuestionnaires
import Util.Logger

runMigration = do
  logInfo "MIGRATION (PublicQuestionnaire/PublicQuestionnaire): started"
  deletePublicQuestionnaires
  insertPublicQuestionnaire publicQuestionnaire
  logInfo "MIGRATION (PublicQuestionnaire/PublicQuestionnaire): ended"
