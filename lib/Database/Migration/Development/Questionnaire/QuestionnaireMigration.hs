module Database.Migration.Development.Questionnaire.QuestionnaireMigration where

import Constant.Component
import Database.DAO.Package.PackageDAO
import Database.DAO.Questionnaire.QuestionnaireDAO
import Database.Migration.Development.Package.Data.Packages
import Database.Migration.Development.Questionnaire.Data.Questionnaires
import Util.Logger

runMigration = do
  logInfo $ msg _CMP_MIGRATION "(Questionnaire/Questionnaire) started"
  deleteQuestionnaires
  insertPackage germanyPackage
  insertQuestionnaire questionnaire1
  insertQuestionnaire questionnaire2
  insertQuestionnaire questionnaire3
  logInfo $ msg _CMP_MIGRATION "(Questionnaire/Questionnaire) ended"
