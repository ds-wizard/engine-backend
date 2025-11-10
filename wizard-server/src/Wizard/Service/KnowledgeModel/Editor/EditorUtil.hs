module Wizard.Service.KnowledgeModel.Editor.EditorUtil where

import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelMigrationDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigOrganizationDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorState
import Wizard.Model.KnowledgeModel.Migration.KnowledgeModelMigration
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageService

getEditorPreviousPackage :: KnowledgeModelEditor -> AppContextM (Maybe KnowledgeModelPackage)
getEditorPreviousPackage editor =
  case editor.previousPackageId of
    Just pkgId -> do
      pkg <- getPackageById pkgId
      return . Just $ pkg
    Nothing -> return Nothing

getEditorForkOfPackageId :: KnowledgeModelEditor -> AppContextM (Maybe String)
getEditorForkOfPackageId editor = do
  mPreviousPkg <- getEditorPreviousPackage editor
  case mPreviousPkg of
    Just previousPkg -> do
      tcOrganization <- findTenantConfigOrganization
      if (previousPkg.organizationId == tcOrganization.organizationId) && (previousPkg.kmId == editor.kmId)
        then return $ previousPkg.forkOfPackageId
        else return . Just $ previousPkg.pId
    Nothing -> return Nothing

getEditorMergeCheckpointPackageId :: KnowledgeModelEditor -> AppContextM (Maybe String)
getEditorMergeCheckpointPackageId editor = do
  mPreviousPkg <- getEditorPreviousPackage editor
  case mPreviousPkg of
    Just previousPkg -> do
      tcOrganization <- findTenantConfigOrganization
      if (previousPkg.organizationId == tcOrganization.organizationId) && (previousPkg.kmId == editor.kmId)
        then return $ previousPkg.mergeCheckpointPackageId
        else return . Just $ previousPkg.pId
    Nothing -> return Nothing

getEditorState :: KnowledgeModelEditor -> Int -> Maybe String -> AppContextM KnowledgeModelEditorState
getEditorState editor eventSize mForkOfPackageId = do
  mMs <- findMigratorStateByEditorUuid' editor.uuid
  isMigrating mMs $ isEditing $ isMigrated mMs $ isOutdated isDefault
  where
    isMigrating mMs continue =
      case mMs of
        Just ms ->
          if ms.state == CompletedKnowledgeModelMigrationState
            then continue
            else return MigratingKnowledgeModelEditorState
        Nothing -> continue
    isEditing continue =
      if eventSize > 0
        then return EditedKnowledgeModelEditorState
        else continue
    isMigrated mMs continue =
      case mMs of
        Just ms ->
          if ms.state == CompletedKnowledgeModelMigrationState
            then return MigratedKnowledgeModelEditorState
            else continue
        Nothing -> continue
    isOutdated continue =
      case mForkOfPackageId of
        Just forkOfPackageId -> do
          newerPackages <- getNewerPackages forkOfPackageId False
          if not . null $ newerPackages
            then return OutdatedKnowledgeModelEditorState
            else continue
        Nothing -> continue
    isDefault = return DefaultKnowledgeModelEditorState
