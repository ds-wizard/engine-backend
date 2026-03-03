module Wizard.Service.KnowledgeModel.Editor.EditorUtil where

import Shared.Coordinate.Model.Coordinate.Coordinate
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelMigrationDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigOrganizationDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorState
import Wizard.Model.KnowledgeModel.Migration.KnowledgeModelMigration
import Wizard.Model.Tenant.Config.TenantConfig

getEditorPreviousPackage :: KnowledgeModelEditor -> AppContextM (Maybe KnowledgeModelPackage)
getEditorPreviousPackage editor =
  case editor.previousPackageUuid of
    Just pkgId -> do
      pkg <- findPackageByUuid pkgId
      return . Just $ pkg
    Nothing -> return Nothing

getEditorForkOfPackageId :: KnowledgeModelEditor -> AppContextM (Maybe Coordinate)
getEditorForkOfPackageId editor = do
  mPreviousPkg <- getEditorPreviousPackage editor
  case mPreviousPkg of
    Just previousPkg -> do
      tcOrganization <- findTenantConfigOrganization
      if (previousPkg.organizationId == tcOrganization.organizationId) && (previousPkg.kmId == editor.kmId)
        then return $ previousPkg.forkOfPackageId
        else return . Just . createCoordinate $ previousPkg
    Nothing -> return Nothing

getEditorMergeCheckpointPackageId :: KnowledgeModelEditor -> AppContextM (Maybe Coordinate)
getEditorMergeCheckpointPackageId editor = do
  mPreviousPkg <- getEditorPreviousPackage editor
  case mPreviousPkg of
    Just previousPkg -> do
      tcOrganization <- findTenantConfigOrganization
      if (previousPkg.organizationId == tcOrganization.organizationId) && (previousPkg.kmId == editor.kmId)
        then return $ previousPkg.mergeCheckpointPackageId
        else return . Just . createCoordinate $ previousPkg
    Nothing -> return Nothing

getEditorState :: KnowledgeModelEditor -> Int -> Maybe Coordinate -> AppContextM KnowledgeModelEditorState
getEditorState editor eventSize mForkOfPackageId = do
  mMs <- findKnowledgeModelMigrationByEditorUuid' editor.uuid
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
          mLatestPkg <- findLatestPackageByOrganizationIdAndKmId' forkOfPackageId.organizationId forkOfPackageId.entityId Nothing
          case mLatestPkg of
            Just latestPkg ->
              if createCoordinate latestPkg /= forkOfPackageId
                then return OutdatedKnowledgeModelEditorState
                else continue
            Nothing -> continue
        Nothing -> continue
    isDefault = return DefaultKnowledgeModelEditorState
