module Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration where

import Shared.Common.Constant.Component
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDataDAO
import Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateDrafts
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.S3.DocumentTemplate.DocumentTemplateS3
import Wizard.Util.Logger
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateAssets
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFiles
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

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
