module Database.Migration.Development.PublicQuestionnaire.PublicQuestionnaireMigration where

import Constant.Component
import Database.DAO.PublicQuestionnaire.PublicQuestionnaireDAO
import Database.Migration.Development.PublicQuestionnaire.Data.PublicQuestionnaires
import Util.Logger

runMigration = do
  logInfo $ msg _CMP_MIGRATION "(PublicQuestionnaire/PublicQuestionnaire) started"
  deletePublicQuestionnaires
  insertPublicQuestionnaire publicQuestionnaire
  logInfo $ msg _CMP_MIGRATION "(PublicQuestionnaire/PublicQuestionnaire) ended"
