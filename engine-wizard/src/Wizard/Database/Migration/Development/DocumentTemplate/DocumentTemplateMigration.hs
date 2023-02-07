module Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration where

import Shared.Constant.Component
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateAssets
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFiles
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Model.DocumentTemplate.DocumentTemplate
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDataDAO
import Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateDrafts
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.S3.DocumentTemplate.DocumentTemplateS3
import Wizard.Util.Logger

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(DocumentTemplate/DocumentTemplate) started"
  -- deleteDraftDatas
  deleteDocumentTemplates
  insertDocumentTemplate wizardDocumentTemplate
  insertDocumentTemplate wizardDocumentTemplateDraft
  _ <- insertFile fileDefaultHtml
  _ <- insertFile fileDefaultCss
  _ <- insertAsset assetLogo
  insertDocumentTemplate differentDocumentTemplate
  _ <- insertFile differentFileHtml
  insertDraftData wizardDocumentTemplateDraftData
  logInfo _CMP_MIGRATION "(DocumentTemplate/DocumentTemplate) ended"

runS3Migration :: AppContextM ()
runS3Migration = do
  purgeBucket
  _ <- putAsset wizardDocumentTemplate.tId assetLogo.uuid assetLogo.contentType assetLogoContent
  return ()
