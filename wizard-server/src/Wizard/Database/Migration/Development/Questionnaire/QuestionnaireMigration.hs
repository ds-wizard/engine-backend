module Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration where

import Data.Foldable (traverse_)

import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Wizard.Database.DAO.Questionnaire.QuestionnaireCommentDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireCommentThreadDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireEventDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireFileDAO
import Wizard.Database.DAO.Questionnaire.QuestionnairePermDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireVersionDAO
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireComments
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireEvents
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.S3.Questionnaire.QuestionnaireFileS3
import WizardLib.KnowledgeModel.Database.DAO.Package.PackageDAO
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages

runMigration = do
  logInfo _CMP_MIGRATION "(Questionnaire/Questionnaire) started"
  deleteQuestionnaireFiles
  purgeBucket
  deleteQuestionnaireComments
  deleteQuestionnaireCommentThreads
  deleteQuestionnairePerms
  deleteQuestionnaireEvents
  deleteQuestionnaires
  insertPackage germanyPackage
  insertQuestionnaire questionnaire1
  insertQuestionnaireEvents (fEvents questionnaire1Uuid)
  traverse_ insertQuestionnaireVersion questionnaire1Versions
  insertQuestionnaire questionnaire2
  insertQuestionnaireEvents (fEvents questionnaire2Uuid)
  traverse_ insertQuestionnaireVersion questionnaire2Versions
  insertQuestionnaire questionnaire3
  insertQuestionnaireEvents (fEvents questionnaire3Uuid)
  traverse_ insertQuestionnaireVersion questionnaire3Versions
  insertQuestionnaire differentQuestionnaire
  insertQuestionnaireCommentThread cmtQ1_t1
  insertQuestionnaireComment cmtQ1_t1_1
  insertQuestionnaireComment cmtQ1_t1_2
  insertQuestionnaireCommentThread cmtQ2_t1
  insertQuestionnaireComment cmtQ2_t1_1
  logInfo _CMP_MIGRATION "(Questionnaire/Questionnaire) ended"
