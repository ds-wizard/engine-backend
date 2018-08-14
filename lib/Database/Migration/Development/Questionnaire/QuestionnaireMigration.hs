module Database.Migration.Development.Questionnaire.QuestionnaireMigration where

import Database.DAO.Package.PackageDAO
import Database.DAO.Questionnaire.QuestionnaireDAO
import Database.Migration.Development.Package.Data.Packages
import Database.Migration.Development.Questionnaire.Data.Questionnaires
import Util.Logger

runMigration = do
  logInfo "MIGRATION (Questionnaire/Questionnaire): started"
  deleteQuestionnaires
  insertPackage elixirCzPackage2Dto
  insertQuestionnaire questionnaire1
  logInfo "MIGRATION (Questionnaire/Questionnaire): ended"
