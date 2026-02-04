module Wizard.Service.KnowledgeModel.Publish.KnowledgeModelPublishService (
  publishPackageFromKnowledgeModelEditor,
  publishPackageFromMigration,
) where

import Control.Monad.Reader (liftIO)
import Data.Time

import Shared.Common.Util.Uuid
import Shared.Coordinate.Model.Coordinate.Coordinate
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
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
import Wizard.Service.KnowledgeModel.Migration.KnowledgeModelMigrationAudit
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
    ms <- findKnowledgeModelMigrationByEditorUuid reqDto.editorUuid
    deleteKnowledgeModelMigrationByEditorUuid reqDto.editorUuid
    auditKmMigrationFinish reqDto.editorUuid
    mForkOfPkg <- findPackageByUuid ms.targetPackageUuid
    let mForkOfPkgId = Just $ createCoordinate mForkOfPkg
    editorPreviousPackage <- findPackageByUuid ms.editorPreviousPackageUuid
    let mMergeCheckpointPkgId = Just $ Coordinate {organizationId = editorPreviousPackage.organizationId, entityId = editorPreviousPackage.kmId, version = reqDto.version}
    doPublishPackage
      reqDto.version
      kmEditor
      ms.resultEvents
      reqDto.description
      reqDto.readme
      mForkOfPkgId
      mMergeCheckpointPkgId

-- --------------------------------
-- PRIVATE
-- --------------------------------
doPublishPackage
  :: String
  -> KnowledgeModelEditor
  -> [KnowledgeModelEvent]
  -> String
  -> String
  -> Maybe Coordinate
  -> Maybe Coordinate
  -> AppContextM KnowledgeModelPackageSimpleDTO
doPublishPackage version kmEditor kmEvents description readme mForkOfPkgId mMergeCheckpointPkgId = do
  let squashedKmEvents = squash kmEvents
  tcOrganization <- findTenantConfigOrganization
  validateNewPackageVersion version kmEditor tcOrganization
  uuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  let (pkg, pkgEvents) = fromPackage kmEditor uuid mForkOfPkgId mMergeCheckpointPkgId tcOrganization version description readme squashedKmEvents now
  createdPkg <- createPackage (pkg, pkgEvents)
  let updatedKmEditor = kmEditor {previousPackageUuid = Just pkg.uuid, updatedAt = now} :: KnowledgeModelEditor
  updateKnowledgeModelEditorByUuid updatedKmEditor
  deleteKnowledgeModelEventsByEditorUuid kmEditor.uuid
  logOutOnlineUsersWhenKnowledgeModelEditorDramaticallyChanged kmEditor.uuid
  return createdPkg
