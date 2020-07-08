module Wizard.Database.Migration.Development.Template.TemplateMigration where

import Control.Lens ((^.))
import qualified Data.UUID as U

import LensesConfig
import Shared.Constant.Component
import Shared.Database.DAO.Template.TemplateDAO
import Shared.Database.Migration.Development.Template.Data.Templates
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Util.Logger

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Template/Template) started"
  deleteTemplates
  deleteTemplateAssetContents
  insertTemplate commonWizardTemplate
  insertTemplateAssetContent (U.toString $ templateAssetLogo ^. uuid) templateAssetLogoContent
  logInfo _CMP_MIGRATION "(Template/Template) ended"
