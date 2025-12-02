module Wizard.Service.KnowledgeModel.Publish.KnowledgeModelPublishService (
  publishPackageFromKnowledgeModelEditor,
  publishPackageFromMigration,
) where

import Control.Monad.Reader (liftIO)
import Data.Time

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.KnowledgeModel.Service.KnowledgeModel.Package.KnowledgeModelPackageUtil
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Wizard.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishEditorDTO
import Wizard.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishMigrationDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelMigrationDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigOrganizationDAO
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Wizard.Model.KnowledgeModel.Migration.KnowledgeModelMigration
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Service.KnowledgeModel.Editor.Collaboration.CollaborationService
import Wizard.Service.KnowledgeModel.Editor.EditorAudit
import Wizard.Service.KnowledgeModel.Editor.EditorMapper
import Wizard.Service.KnowledgeModel.Editor.EditorUtil
import Wizard.Service.KnowledgeModel.Migration.MigrationAudit
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageService
import Wizard.Service.KnowledgeModel.Publish.KnowledgeModelPublishMapper
import Wizard.Service.KnowledgeModel.Publish.KnowledgeModelPublishValidation
import Wizard.Service.KnowledgeModel.Squash.Squasher

publishPackageFromKnowledgeModelEditor :: PackagePublishEditorDTO -> AppContextM KnowledgeModelPackageSimpleDTO
publishPackageFromKnowledgeModelEditor reqDto = do
  runInTransaction $ do
    checkPermission _KM_PUBLISH_PERM
    validateMigrationExistence reqDto.editorUuid
    kmEditor <- findKnowledgeModelEditorByUuid reqDto.editorUuid
    kmEditorEvents <- findKnowledgeModelEventsByEditorUuid reqDto.editorUuid
    let kmEvents = fmap toKnowledgeModelEvent kmEditorEvents
    mForkOfPkgId <- getEditorForkOfPackageId kmEditor
    mMergeCheckpointPkgId <- getEditorMergeCheckpointPackageId kmEditor
    auditKnowledgeModelEditorPublish kmEditor kmEditorEvents mForkOfPkgId
    doPublishPackage
      kmEditor.version
      kmEditor
      kmEvents
      kmEditor.description
      kmEditor.readme
      mForkOfPkgId
      mMergeCheckpointPkgId

publishPackageFromMigration :: PackagePublishMigrationDTO -> AppContextM KnowledgeModelPackageSimpleDTO
publishPackageFromMigration reqDto = do
  runInTransaction $ do
    checkPermission _KM_PUBLISH_PERM
    kmEditor <- findKnowledgeModelEditorByUuid reqDto.editorUuid
    ms <- findMigratorStateByEditorUuid reqDto.editorUuid
    deleteMigratorStateByEditorUuid reqDto.editorUuid
    auditKmMigrationFinish reqDto.editorUuid
    doPublishPackage
      reqDto.version
      kmEditor
      ms.resultEvents
      reqDto.description
      reqDto.readme
      (Just ms.targetPackageId)
      (Just $ upgradePackageVersion ms.editorPreviousPackageId reqDto.version)

-- --------------------------------
-- PRIVATE
-- --------------------------------
doPublishPackage
  :: String
  -> KnowledgeModelEditor
  -> [KnowledgeModelEvent]
  -> String
  -> String
  -> Maybe String
  -> Maybe String
  -> AppContextM KnowledgeModelPackageSimpleDTO
doPublishPackage version kmEditor kmEvents description readme mForkOfPkgId mMergeCheckpointPkgId = do
  let squashedKmEvents = squash kmEvents
  tcOrganization <- findTenantConfigOrganization
  validateNewPackageVersion version kmEditor tcOrganization
  now <- liftIO getCurrentTime
  let (pkg, pkgEvents) = fromPackage kmEditor mForkOfPkgId mMergeCheckpointPkgId tcOrganization version description readme squashedKmEvents now
  createdPkg <- createPackage (pkg, pkgEvents)
  let updatedKmEditor = kmEditor {previousPackageId = Just pkg.pId, updatedAt = now} :: KnowledgeModelEditor
  updateKnowledgeModelEditorByUuid updatedKmEditor
  deleteKnowledgeModelEventsByEditorUuid kmEditor.uuid
  logOutOnlineUsersWhenKnowledgeModelEditorDramaticallyChanged kmEditor.uuid
  return createdPkg
