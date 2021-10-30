module Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration where

import Shared.Constant.Component
import Shared.Database.DAO.Package.PackageDAO
import Shared.Database.Migration.Development.Package.Data.Packages
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "(Questionnaire/Questionnaire) started"
  deleteQuestionnaires
  insertPackage germanyPackage
  insertQuestionnaire questionnaire1
  insertQuestionnaire questionnaire2
  insertQuestionnaire questionnaire3
  insertQuestionnaire differentQuestionnaire
  logInfo _CMP_MIGRATION "(Questionnaire/Questionnaire) ended"
