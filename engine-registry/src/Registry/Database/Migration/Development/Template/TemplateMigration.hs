module Registry.Database.Migration.Development.Template.TemplateMigration where

import Control.Lens ((^.))
import qualified Data.UUID as U

import LensesConfig
import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Registry.S3.Template.TemplateS3
import Registry.Util.Logger
import Shared.Database.DAO.Template.TemplateSqlDAO
import Shared.Database.Migration.Development.Template.Data.Templates

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Fixtures/Template) started"
  deleteTemplates
  purgeBucket
  insertTemplate commonWizardTemplate
  putAsset (commonWizardTemplate ^. tId) (U.toString $ templateAssetLogo ^. uuid) templateAssetLogoContent
  logInfo _CMP_MIGRATION "(Fixtures/Template) ended"
