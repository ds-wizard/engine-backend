module Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration where

import Data.Foldable (traverse_)

import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDataDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.S3.DocumentTemplate.DocumentTemplateS3
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateFormatDAO
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateAssets
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFiles
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

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
