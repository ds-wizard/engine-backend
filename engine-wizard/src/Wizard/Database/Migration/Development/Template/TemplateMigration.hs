module Wizard.Database.Migration.Development.Template.TemplateMigration where

import Control.Lens ((^.))
import qualified Data.UUID as U

import LensesConfig
import Shared.Database.Migration.Development.Template.Data.Templates
import Wizard.Constant.Component
import Wizard.Database.DAO.Template.TemplateDAO
import Wizard.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "(Template/Template) started"
  deleteTemplates
  deleteTemplateAssetContents
  insertTemplate commonWizardTemplate
  insertTemplateAssetContent (U.toString $ templateAssetLogo ^. uuid) templateAssetLogoContent
  logInfo _CMP_MIGRATION "(Template/Template) ended"
