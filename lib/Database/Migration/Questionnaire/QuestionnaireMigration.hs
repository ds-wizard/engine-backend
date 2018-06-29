module Database.Migration.Questionnaire.QuestionnaireMigration where

import Control.Monad.Logger (logInfo)

import Database.DAO.Package.PackageDAO
import Database.DAO.Questionnaire.QuestionnaireDAO
import Database.Migration.Package.Data.Packages
import Database.Migration.Questionnaire.Data.Questionnaires

runMigration = do
  $(logInfo) "MIGRATION (Questionnaire/Questionnaire): started"
  deleteQuestionnaires
  insertPackage elixirCzPackage2Dto
  insertQuestionnaire questionnaire1
  $(logInfo) "MIGRATION (Questionnaire/Questionnaire): ended"
