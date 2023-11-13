module Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration where

import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Wizard.Database.DAO.Questionnaire.QuestionnaireCommentDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireCommentThreadDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.Questionnaire.QuestionnairePermDAO
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireComments
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import WizardLib.KnowledgeModel.Database.DAO.Package.PackageDAO
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages

runMigration = do
  logInfo _CMP_MIGRATION "(Questionnaire/Questionnaire) started"
  deleteQuestionnaireComments
  deleteQuestionnaireCommentThreads
  deleteQuestionnairePerms
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
