module Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration where

import Shared.Constant.Component
import Shared.Database.DAO.Package.PackageDAO
import Shared.Database.Migration.Development.Package.Data.Packages
import Wizard.Database.DAO.Questionnaire.QuestionnaireCommentDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireCommentThreadDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireComments
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "(Questionnaire/Questionnaire) started"
  deleteQuestionnaireComments
  deleteQuestionnaireCommentThreads
  deleteQuestionnaires
  insertPackage germanyPackage
  insertQuestionnaire questionnaire1
  insertQuestionnaire questionnaire2
  insertQuestionnaire questionnaire3
  insertQuestionnaire differentQuestionnaire
  insertQuestionnaireCommentThread cmtQ1_t1
  insertQuestionnaireComment cmtQ1_t1_1
  insertQuestionnaireComment cmtQ1_t1_2
  insertQuestionnaireCommentThread cmtQ2_t1
  insertQuestionnaireComment cmtQ2_t1_1
  logInfo _CMP_MIGRATION "(Questionnaire/Questionnaire) ended"
