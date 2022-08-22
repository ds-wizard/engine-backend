module Wizard.Database.Migration.Development.QuestionnaireImporter.QuestionnaireImporterMigration where

import Shared.Constant.Component
import Wizard.Database.DAO.QuestionnaireImporter.QuestionnaireImporterDAO
import Wizard.Database.Migration.Development.QuestionnaireImporter.Data.QuestionnaireImporters
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Util.Logger

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(QuestionnaireImporter/QuestionnaireImporter) started"
  deleteQuestionnaireImporters
  insertQuestionnaireImporter questionnaireImporterBio1
  insertQuestionnaireImporter questionnaireImporterBio2
  insertQuestionnaireImporter questionnaireImporterBio3
  insertQuestionnaireImporter questionnaireExtImporter1
  insertQuestionnaireImporter questionnaireOntoImporter1
  logInfo _CMP_MIGRATION "(QuestionnaireImporter/QuestionnaireImporter) ended"
