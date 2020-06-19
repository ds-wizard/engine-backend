module Registry.Database.Migration.Development.Template.TemplateMigration where

import Control.Lens ((^.))
import qualified Data.UUID as U

import LensesConfig
import Registry.Constant.Component
import Registry.Database.DAO.Template.TemplateDAO
import Registry.Util.Logger
import Shared.Database.Migration.Development.Template.Data.Templates

runMigration = do
  logInfo _CMP_MIGRATION "(Template/Template) started"
  deleteTemplates
  deleteTemplateAssetContents
  insertTemplate commonWizardTemplate
  insertTemplateAssetContent (U.toString $ templateAssetLogo ^. uuid) templateAssetLogoContent
  logInfo _CMP_MIGRATION "(Template/Template) ended"
