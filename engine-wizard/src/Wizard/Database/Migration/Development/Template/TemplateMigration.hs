module Wizard.Database.Migration.Development.Template.TemplateMigration where

import Control.Lens ((^.))
import qualified Data.UUID as U

import LensesConfig
import Shared.Constant.Component
import Shared.Database.DAO.Template.TemplateAssetDAO
import Shared.Database.DAO.Template.TemplateDAO
import Shared.Database.DAO.Template.TemplateFileDAO
import Shared.Database.Migration.Development.Template.Data.Templates
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
  insertTemplateFile templateFileDefaultHtml
  insertTemplateFile templateFileDefaultCss
  insertTemplateAsset templateAssetLogo
  insertTemplate differentTemplate
  insertTemplateFile differentFileHtml
  logInfo _CMP_MIGRATION "(Template/Template) ended"

runS3Migration :: AppContextM ()
runS3Migration = do
  purgeBucket
  putAsset (commonWizardTemplate ^. tId) (U.toString $ templateAssetLogo ^. uuid) templateAssetLogoContent
