module Wizard.Database.Migration.Development.Template.TemplateMigration where

import qualified Data.UUID as U

import Shared.Constant.Component
import Shared.Database.DAO.Template.TemplateAssetDAO
import Shared.Database.DAO.Template.TemplateDAO
import Shared.Database.DAO.Template.TemplateFileDAO
import Shared.Database.Migration.Development.Template.Data.Templates
import Shared.Model.Template.Template
import Wizard.Database.Migration.Development.Template.Data.Templates
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.S3.Template.TemplateS3
import Wizard.Util.Logger

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Template/Template) started"
  deleteTemplates
  insertTemplate commonWizardTemplate
  _ <- insertTemplateFile templateFileDefaultHtml
  _ <- insertTemplateFile templateFileDefaultCss
  _ <- insertTemplateAsset templateAssetLogo
  insertTemplate differentTemplate
  _ <- insertTemplateFile differentFileHtml
  logInfo _CMP_MIGRATION "(Template/Template) ended"

runS3Migration :: AppContextM ()
runS3Migration = do
  purgeBucket
  _ <- putAsset commonWizardTemplate.tId (U.toString templateAssetLogo.uuid) templateAssetLogoContent
  return ()
