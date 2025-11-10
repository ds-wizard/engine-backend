module Registry.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration where

import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Registry.S3.DocumentTemplate.DocumentTemplateS3
import Shared.Common.Util.Logger
import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO
import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateFormatDAO
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateAssets
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFiles
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Fixtures/DocumentTemplate) started"
  deleteDocumentTemplates
  purgeBucket
  insertDocumentTemplate wizardDocumentTemplate
  insertDocumentTemplateFormat formatJson
  insertDocumentTemplateFormat formatHtml
  insertDocumentTemplateFormat formatPdf
  insertDocumentTemplateFormat formatLatex
  insertDocumentTemplateFormat formatDocx
  insertDocumentTemplateFormat formatOdt
  insertDocumentTemplateFormat formatMarkdown
  insertFile fileDefaultHtml
  insertFile fileDefaultCss
  insertAsset assetLogo
  putAsset wizardDocumentTemplate.tId assetLogo.uuid assetLogo.contentType assetLogoContent
  logInfo _CMP_MIGRATION "(Fixtures/DocumentTemplate) ended"
