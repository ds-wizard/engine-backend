module Wizard.Database.Migration.Development.QuestionnaireAction.QuestionnaireActionMigration where

import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Wizard.Database.DAO.QuestionnaireAction.QuestionnaireActionDAO
import Wizard.Database.Migration.Development.QuestionnaireAction.Data.QuestionnaireActions
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(QuestionnaireAction/QuestionnaireAction) started"
  deleteQuestionnaireActions
  insertQuestionnaireAction questionnaireActionFtp1
  insertQuestionnaireAction questionnaireActionFtp2
  insertQuestionnaireAction questionnaireActionFtp3
  insertQuestionnaireAction questionnaireActionMail1
  insertQuestionnaireAction questionnaireActionScp1
  logInfo _CMP_MIGRATION "(QuestionnaireAction/QuestionnaireAction) ended"
