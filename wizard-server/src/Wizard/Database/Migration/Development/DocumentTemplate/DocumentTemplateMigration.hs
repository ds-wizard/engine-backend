module Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration where

import Data.Foldable (traverse_)

import Shared.Common.Constant.Component
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
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDataDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.S3.DocumentTemplate.DocumentTemplateS3

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(DocumentTemplate/DocumentTemplate) started"
  deleteDraftData
  deleteDocumentTemplates
  insertDocumentTemplate wizardDocumentTemplate
  traverse_ insertDocumentTemplateFormat wizardDocumentTemplateFormats
  insertDocumentTemplate wizardDocumentTemplateDraft
  traverse_ insertDocumentTemplateFormat wizardDocumentTemplateDraftFormats
  _ <- insertFile fileDefaultHtml
  _ <- insertFile fileDefaultCss
  _ <- insertAsset assetLogo
  insertDocumentTemplate differentDocumentTemplate
  traverse_ insertDocumentTemplateFormat differentDocumentTemplateFormats
  _ <- insertFile differentFileHtml
  logInfo _CMP_MIGRATION "(DocumentTemplate/DocumentTemplate) ended"

runS3Migration :: AppContextM ()
runS3Migration = do
  purgeBucket
  _ <- putAsset wizardDocumentTemplate.tId assetLogo.uuid assetLogo.contentType assetLogoContent
  return ()
