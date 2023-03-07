module Registry.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration where

import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Registry.S3.DocumentTemplate.DocumentTemplateS3
import Registry.Util.Logger
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateAssets
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFiles
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Model.DocumentTemplate.DocumentTemplate

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Fixtures/DocumentTemplate) started"
  deleteDocumentTemplates
  purgeBucket
  insertDocumentTemplate wizardDocumentTemplate
  insertFile fileDefaultHtml
  insertFile fileDefaultCss
  insertAsset assetLogo
  putAsset wizardDocumentTemplate.tId assetLogo.uuid assetLogo.contentType assetLogoContent
  logInfo _CMP_MIGRATION "(Fixtures/DocumentTemplate) ended"
