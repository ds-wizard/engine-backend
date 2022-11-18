module Registry.Database.Migration.Development.Template.TemplateMigration where

import qualified Data.UUID as U

import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Registry.S3.Template.TemplateS3
import Registry.Util.Logger
import Shared.Database.DAO.Template.TemplateAssetDAO
import Shared.Database.DAO.Template.TemplateDAO
import Shared.Database.DAO.Template.TemplateFileDAO
import Shared.Database.Migration.Development.Template.Data.Templates
import Shared.Model.Template.Template

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Fixtures/Template) started"
  deleteTemplates
  purgeBucket
  insertTemplate commonWizardTemplate
  insertTemplateFile templateFileDefaultHtml
  insertTemplateFile templateFileDefaultCss
  insertTemplateAsset templateAssetLogo
  putAsset commonWizardTemplate.tId (U.toString templateAssetLogo.uuid) templateAssetLogoContent
  logInfo _CMP_MIGRATION "(Fixtures/Template) ended"
