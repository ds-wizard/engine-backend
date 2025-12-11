module Wizard.Specs.Service.Project.ProjectAclSpec where

import Data.Foldable (traverse_)
import Test.Hspec

import Shared.Common.Util.Uuid
import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Wizard.Database.DAO.Project.ProjectDAO
import Wizard.Database.DAO.Project.ProjectPermDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Database.Migration.Development.Project.Data.Projects
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Context.AppContext
import Wizard.Model.Project.Acl.ProjectPerm
import Wizard.Model.Project.Project
import Wizard.Model.User.User
import Wizard.Service.Project.ProjectAcl
import Wizard.Service.Project.ProjectMapper
import qualified Wizard.Service.User.UserMapper as U_Mapper
import WizardLib.Public.Database.DAO.User.UserGroupDAO
import WizardLib.Public.Database.DAO.User.UserGroupMembershipDAO
import WizardLib.Public.Database.Migration.Development.User.Data.UserGroups
import WizardLib.Public.Model.User.UserGroup

import Wizard.Specs.Common

projectAclSpec appContext =
  describe "Project ACL" $ do
    let permissions =
          [ toUserProjectPerm
              (u' "808d4770-0d38-45b0-a028-0a3ffaafc617")
              userNikola.uuid
              ownerPermissions
              userNikola.tenantUuid
          , toUserProjectPerm
              (u' "52e74b8b-ca73-4d6a-a7e1-5c0f34a1819a")
              userNicolaus.uuid
              editorPermissions
              userNicolaus.tenantUuid
          , toUserProjectPerm
              (u' "3d60a813-5e54-4fd2-8fe8-5cf3c076bd50")
              userGalileo.uuid
              viewerPermissions
              userGalileo.tenantUuid
          , toUserGroupProjectPerm
              (u' "b90b17f4-06a2-40dc-b364-88d8f195c8a0")
              bioGroup.uuid
              ownerPermissions
              bioGroup.tenantUuid
          , toUserGroupProjectPerm
              (u' "fcead22d-e453-47a3-84bc-5c29698ab990")
              plantGroup.uuid
              editorPermissions
              plantGroup.tenantUuid
          , toUserGroupProjectPerm
              (u' "4e0765e8-f25c-4091-8531-aed2eef161f6")
              animalGroup.uuid
              viewerPermissions
              animalGroup.tenantUuid
          ]
    let makeContext user = appContext {currentUser = Just . U_Mapper.toDTO $ user}
    let adminCtx = makeContext userAlbert
    let ownerCtx = makeContext userNikola
    let editorCtx = makeContext userNicolaus
    let viewerCtx = makeContext userGalileo
    let userInOwnerGroupCtx = makeContext userIsaac
    let userInEditorGroupCtx = makeContext userIsaac
    let userInViewerGroupCtx = makeContext userIsaac
    let userWithoutPermCtx = makeContext userIsaac
    let anonymousCtx = appContext {currentUser = Nothing}
    it "checkViewPermissionToProject" $ do
      runLocalTestMigration appContext
      let fn1 memberships = do
            deleteUserGroupMemberships
            traverse_ insertUserGroupMembership memberships
            checkViewPermissionToProject PrivateProjectVisibility RestrictedProjectSharing permissions
      shouldSucceed adminCtx (fn1 [])
      shouldSucceed ownerCtx (fn1 [])
      shouldSucceed editorCtx (fn1 [])
      shouldSucceed viewerCtx (fn1 [])
      shouldSucceed userInOwnerGroupCtx (fn1 [userIsaacBioGroupMembership, userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldSucceed userInEditorGroupCtx (fn1 [userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldSucceed userInViewerGroupCtx (fn1 [userIsaacAnimalGroupMembership])
      shouldFailed userWithoutPermCtx (fn1 [])
      shouldFailed anonymousCtx (fn1 [])
      let fn2 memberships = do
            deleteUserGroupMemberships
            traverse_ insertUserGroupMembership memberships
            checkViewPermissionToProject VisibleViewProjectVisibility RestrictedProjectSharing permissions
      shouldSucceed adminCtx (fn2 [])
      shouldSucceed ownerCtx (fn2 [])
      shouldSucceed editorCtx (fn2 [])
      shouldSucceed viewerCtx (fn2 [])
      shouldSucceed userInOwnerGroupCtx (fn2 [userIsaacBioGroupMembership, userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldSucceed userInEditorGroupCtx (fn2 [userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldSucceed userInViewerGroupCtx (fn2 [userIsaacAnimalGroupMembership])
      shouldSucceed userWithoutPermCtx (fn2 [])
      shouldFailed anonymousCtx (fn2 [])
      let fn3 memberships = do
            deleteUserGroupMemberships
            traverse_ insertUserGroupMembership memberships
            checkViewPermissionToProject VisibleEditProjectVisibility RestrictedProjectSharing permissions
      shouldSucceed adminCtx (fn3 [])
      shouldSucceed ownerCtx (fn3 [])
      shouldSucceed editorCtx (fn3 [])
      shouldSucceed viewerCtx (fn3 [])
      shouldSucceed userInOwnerGroupCtx (fn3 [userIsaacBioGroupMembership, userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldSucceed userInEditorGroupCtx (fn3 [userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldSucceed userInViewerGroupCtx (fn3 [userIsaacAnimalGroupMembership])
      shouldSucceed userWithoutPermCtx (fn3 [])
      shouldFailed anonymousCtx (fn3 [])
      -- --------------------
      let fn4 memberships = do
            deleteUserGroupMemberships
            traverse_ insertUserGroupMembership memberships
            checkViewPermissionToProject PrivateProjectVisibility AnyoneWithLinkViewProjectSharing permissions
      shouldSucceed adminCtx (fn4 [])
      shouldSucceed ownerCtx (fn4 [])
      shouldSucceed editorCtx (fn4 [])
      shouldSucceed viewerCtx (fn4 [])
      shouldSucceed userInOwnerGroupCtx (fn4 [userIsaacBioGroupMembership, userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldSucceed userInEditorGroupCtx (fn4 [userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldSucceed userInViewerGroupCtx (fn4 [userIsaacAnimalGroupMembership])
      shouldSucceed userWithoutPermCtx (fn4 [])
      shouldSucceed anonymousCtx (fn4 [])
      let fn5 memberships = do
            deleteUserGroupMemberships
            traverse_ insertUserGroupMembership memberships
            checkViewPermissionToProject VisibleViewProjectVisibility AnyoneWithLinkViewProjectSharing permissions
      shouldSucceed adminCtx (fn5 [])
      shouldSucceed ownerCtx (fn5 [])
      shouldSucceed editorCtx (fn5 [])
      shouldSucceed viewerCtx (fn5 [])
      shouldSucceed userInOwnerGroupCtx (fn5 [userIsaacBioGroupMembership, userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldSucceed userInEditorGroupCtx (fn5 [userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldSucceed userInViewerGroupCtx (fn5 [userIsaacAnimalGroupMembership])
      shouldSucceed userWithoutPermCtx (fn5 [])
      shouldSucceed anonymousCtx (fn5 [])
      let fn6 memberships = do
            deleteUserGroupMemberships
            traverse_ insertUserGroupMembership memberships
            checkViewPermissionToProject VisibleEditProjectVisibility AnyoneWithLinkViewProjectSharing permissions
      shouldSucceed adminCtx (fn6 [])
      shouldSucceed ownerCtx (fn6 [])
      shouldSucceed editorCtx (fn6 [])
      shouldSucceed viewerCtx (fn6 [])
      shouldSucceed userInOwnerGroupCtx (fn6 [userIsaacBioGroupMembership, userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldSucceed userInEditorGroupCtx (fn6 [userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldSucceed userInViewerGroupCtx (fn6 [userIsaacAnimalGroupMembership])
      shouldSucceed userWithoutPermCtx (fn6 [])
      shouldSucceed anonymousCtx (fn6 [])
      -- --------------------
      let fn7 memberships = do
            deleteUserGroupMemberships
            traverse_ insertUserGroupMembership memberships
            checkViewPermissionToProject PrivateProjectVisibility AnyoneWithLinkEditProjectSharing permissions
      shouldSucceed adminCtx (fn7 [])
      shouldSucceed ownerCtx (fn7 [])
      shouldSucceed editorCtx (fn7 [])
      shouldSucceed viewerCtx (fn7 [])
      shouldSucceed userInOwnerGroupCtx (fn7 [userIsaacBioGroupMembership, userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldSucceed userInEditorGroupCtx (fn7 [userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldSucceed userInViewerGroupCtx (fn7 [userIsaacAnimalGroupMembership])
      shouldSucceed userWithoutPermCtx (fn7 [])
      shouldSucceed anonymousCtx (fn7 [])
      let fn8 memberships = do
            deleteUserGroupMemberships
            traverse_ insertUserGroupMembership memberships
            checkViewPermissionToProject VisibleViewProjectVisibility AnyoneWithLinkEditProjectSharing permissions
      shouldSucceed adminCtx (fn8 [])
      shouldSucceed ownerCtx (fn8 [])
      shouldSucceed editorCtx (fn8 [])
      shouldSucceed viewerCtx (fn8 [])
      shouldSucceed userInOwnerGroupCtx (fn8 [userIsaacBioGroupMembership, userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldSucceed userInEditorGroupCtx (fn8 [userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldSucceed userInViewerGroupCtx (fn8 [userIsaacAnimalGroupMembership])
      shouldSucceed userWithoutPermCtx (fn8 [])
      shouldSucceed anonymousCtx (fn8 [])
      let fn9 memberships = do
            deleteUserGroupMemberships
            traverse_ insertUserGroupMembership memberships
            checkViewPermissionToProject VisibleEditProjectVisibility AnyoneWithLinkEditProjectSharing permissions
      shouldSucceed adminCtx (fn9 [])
      shouldSucceed ownerCtx (fn9 [])
      shouldSucceed editorCtx (fn9 [])
      shouldSucceed viewerCtx (fn9 [])
      shouldSucceed userInOwnerGroupCtx (fn9 [userIsaacBioGroupMembership, userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldSucceed userInEditorGroupCtx (fn9 [userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldSucceed userInViewerGroupCtx (fn9 [userIsaacAnimalGroupMembership])
      shouldSucceed userWithoutPermCtx (fn9 [])
      shouldSucceed anonymousCtx (fn9 [])
    it "checkOwnerPermissionToProject" $ do
      runLocalTestMigration appContext
      let fn1 memberships = do
            deleteUserGroupMemberships
            traverse_ insertUserGroupMembership memberships
            checkOwnerPermissionToProject PrivateProjectVisibility permissions
      shouldSucceed adminCtx (fn1 [])
      shouldSucceed ownerCtx (fn1 [])
      shouldFailed editorCtx (fn1 [])
      shouldFailed viewerCtx (fn1 [])
      shouldSucceed userInOwnerGroupCtx (fn1 [userIsaacBioGroupMembership, userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldFailed userInEditorGroupCtx (fn1 [userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldFailed userInViewerGroupCtx (fn1 [userIsaacAnimalGroupMembership])
      shouldFailed userWithoutPermCtx (fn1 [])
      shouldFailed anonymousCtx (fn1 [])
      let fn2 memberships = do
            deleteUserGroupMemberships
            traverse_ insertUserGroupMembership memberships
            checkOwnerPermissionToProject VisibleViewProjectVisibility permissions
      shouldSucceed adminCtx (fn2 [])
      shouldSucceed ownerCtx (fn2 [])
      shouldFailed editorCtx (fn2 [])
      shouldFailed viewerCtx (fn2 [])
      shouldSucceed userInOwnerGroupCtx (fn2 [userIsaacBioGroupMembership, userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldFailed userInEditorGroupCtx (fn2 [userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldFailed userInViewerGroupCtx (fn2 [userIsaacAnimalGroupMembership])
      shouldFailed userWithoutPermCtx (fn2 [])
      shouldFailed anonymousCtx (fn2 [])
      let fn3 memberships = do
            deleteUserGroupMemberships
            traverse_ insertUserGroupMembership memberships
            checkOwnerPermissionToProject VisibleEditProjectVisibility permissions
      shouldSucceed adminCtx (fn3 [])
      shouldSucceed ownerCtx (fn3 [])
      shouldFailed editorCtx (fn3 [])
      shouldFailed viewerCtx (fn3 [])
      shouldSucceed userInOwnerGroupCtx (fn3 [userIsaacBioGroupMembership, userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldFailed userInEditorGroupCtx (fn3 [userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldFailed userInViewerGroupCtx (fn3 [userIsaacAnimalGroupMembership])
      shouldFailed userWithoutPermCtx (fn3 [])
      shouldFailed anonymousCtx (fn3 [])
    it "checkEditPermissionToProject" $ do
      runLocalTestMigration appContext
      let fn1 memberships = do
            deleteUserGroupMemberships
            traverse_ insertUserGroupMembership memberships
            checkEditPermissionToProject PrivateProjectVisibility RestrictedProjectSharing permissions
      shouldSucceed adminCtx (fn1 [])
      shouldSucceed ownerCtx (fn1 [])
      shouldSucceed editorCtx (fn1 [])
      shouldFailed viewerCtx (fn1 [])
      shouldSucceed userInOwnerGroupCtx (fn1 [userIsaacBioGroupMembership, userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldSucceed userInEditorGroupCtx (fn1 [userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldFailed userInViewerGroupCtx (fn1 [userIsaacAnimalGroupMembership])
      shouldFailed userWithoutPermCtx (fn1 [])
      shouldFailed anonymousCtx (fn1 [])
      let fn2 memberships = do
            deleteUserGroupMemberships
            traverse_ insertUserGroupMembership memberships
            checkEditPermissionToProject VisibleViewProjectVisibility RestrictedProjectSharing permissions
      shouldSucceed adminCtx (fn2 [])
      shouldSucceed ownerCtx (fn2 [])
      shouldSucceed editorCtx (fn2 [])
      shouldFailed viewerCtx (fn2 [])
      shouldSucceed userInOwnerGroupCtx (fn2 [userIsaacBioGroupMembership, userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldSucceed userInEditorGroupCtx (fn2 [userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldFailed userInViewerGroupCtx (fn2 [userIsaacAnimalGroupMembership])
      shouldFailed userWithoutPermCtx (fn2 [])
      shouldFailed anonymousCtx (fn2 [])
      let fn3 memberships = do
            deleteUserGroupMemberships
            traverse_ insertUserGroupMembership memberships
            checkEditPermissionToProject VisibleEditProjectVisibility RestrictedProjectSharing permissions
      shouldSucceed adminCtx (fn3 [])
      shouldSucceed ownerCtx (fn3 [])
      shouldSucceed editorCtx (fn3 [])
      shouldSucceed viewerCtx (fn3 [])
      shouldSucceed userInOwnerGroupCtx (fn3 [userIsaacBioGroupMembership, userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldSucceed userInEditorGroupCtx (fn3 [userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldSucceed userInViewerGroupCtx (fn3 [userIsaacAnimalGroupMembership])
      shouldSucceed userWithoutPermCtx (fn3 [])
      shouldFailed anonymousCtx (fn3 [])
      -- --------------------
      let fn4 memberships = do
            deleteUserGroupMemberships
            traverse_ insertUserGroupMembership memberships
            checkEditPermissionToProject PrivateProjectVisibility AnyoneWithLinkViewProjectSharing permissions
      shouldSucceed adminCtx (fn4 [])
      shouldSucceed ownerCtx (fn4 [])
      shouldSucceed editorCtx (fn4 [])
      shouldFailed viewerCtx (fn4 [])
      shouldSucceed userInOwnerGroupCtx (fn4 [userIsaacBioGroupMembership, userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldSucceed userInEditorGroupCtx (fn4 [userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldFailed userInViewerGroupCtx (fn4 [userIsaacAnimalGroupMembership])
      shouldFailed userWithoutPermCtx (fn4 [])
      shouldFailed anonymousCtx (fn4 [])
      let fn5 memberships = do
            deleteUserGroupMemberships
            traverse_ insertUserGroupMembership memberships
            checkEditPermissionToProject VisibleViewProjectVisibility AnyoneWithLinkViewProjectSharing permissions
      shouldSucceed adminCtx (fn5 [])
      shouldSucceed ownerCtx (fn5 [])
      shouldSucceed editorCtx (fn5 [])
      shouldFailed viewerCtx (fn5 [])
      shouldSucceed userInOwnerGroupCtx (fn5 [userIsaacBioGroupMembership, userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldSucceed userInEditorGroupCtx (fn5 [userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldFailed userInViewerGroupCtx (fn5 [userIsaacAnimalGroupMembership])
      shouldFailed userWithoutPermCtx (fn5 [])
      shouldFailed anonymousCtx (fn5 [])
      let fn6 memberships = do
            deleteUserGroupMemberships
            traverse_ insertUserGroupMembership memberships
            checkEditPermissionToProject VisibleEditProjectVisibility AnyoneWithLinkViewProjectSharing permissions
      shouldSucceed adminCtx (fn6 [])
      shouldSucceed ownerCtx (fn6 [])
      shouldSucceed editorCtx (fn6 [])
      shouldSucceed viewerCtx (fn6 [])
      shouldSucceed userInOwnerGroupCtx (fn6 [userIsaacBioGroupMembership, userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldSucceed userInEditorGroupCtx (fn6 [userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldSucceed userInViewerGroupCtx (fn6 [userIsaacAnimalGroupMembership])
      shouldSucceed userWithoutPermCtx (fn6 [])
      shouldFailed anonymousCtx (fn6 [])
      -- --------------------
      let fn7 memberships = do
            deleteUserGroupMemberships
            traverse_ insertUserGroupMembership memberships
            checkEditPermissionToProject PrivateProjectVisibility AnyoneWithLinkEditProjectSharing permissions
      shouldSucceed adminCtx (fn7 [])
      shouldSucceed ownerCtx (fn7 [])
      shouldSucceed editorCtx (fn7 [])
      shouldSucceed viewerCtx (fn7 [])
      shouldSucceed userInOwnerGroupCtx (fn7 [userIsaacBioGroupMembership, userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldSucceed userInEditorGroupCtx (fn7 [userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldSucceed userInViewerGroupCtx (fn7 [userIsaacAnimalGroupMembership])
      shouldSucceed userWithoutPermCtx (fn7 [])
      shouldSucceed anonymousCtx (fn7 [])
      let fn8 memberships = do
            deleteUserGroupMemberships
            traverse_ insertUserGroupMembership memberships
            checkEditPermissionToProject VisibleViewProjectVisibility AnyoneWithLinkEditProjectSharing permissions
      shouldSucceed adminCtx (fn8 [])
      shouldSucceed ownerCtx (fn8 [])
      shouldSucceed editorCtx (fn8 [])
      shouldSucceed viewerCtx (fn8 [])
      shouldSucceed userInOwnerGroupCtx (fn8 [userIsaacBioGroupMembership, userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldSucceed userInEditorGroupCtx (fn8 [userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldSucceed userInViewerGroupCtx (fn8 [userIsaacAnimalGroupMembership])
      shouldSucceed userWithoutPermCtx (fn8 [])
      shouldSucceed anonymousCtx (fn8 [])
      let fn9 memberships = do
            deleteUserGroupMemberships
            traverse_ insertUserGroupMembership memberships
            checkEditPermissionToProject VisibleEditProjectVisibility AnyoneWithLinkEditProjectSharing permissions
      shouldSucceed adminCtx (fn9 [])
      shouldSucceed ownerCtx (fn9 [])
      shouldSucceed editorCtx (fn9 [])
      shouldSucceed viewerCtx (fn9 [])
      shouldSucceed userInOwnerGroupCtx (fn9 [userIsaacBioGroupMembership, userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldSucceed userInEditorGroupCtx (fn9 [userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldSucceed userInViewerGroupCtx (fn9 [userIsaacAnimalGroupMembership])
      shouldSucceed userWithoutPermCtx (fn9 [])
      shouldSucceed anonymousCtx (fn9 [])
    it "checkMigrationPermissionToProject" $ do
      runLocalTestMigration appContext
      let fn1 memberships = do
            deleteUserGroupMemberships
            traverse_ insertUserGroupMembership memberships
            checkMigrationPermissionToProject PrivateProjectVisibility permissions
      shouldSucceed adminCtx (fn1 [])
      shouldSucceed ownerCtx (fn1 [])
      shouldSucceed editorCtx (fn1 [])
      shouldSucceed userInOwnerGroupCtx (fn1 [userIsaacBioGroupMembership, userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldSucceed userInEditorGroupCtx (fn1 [userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldFailed userInViewerGroupCtx (fn1 [userIsaacAnimalGroupMembership])
      shouldFailed viewerCtx (fn1 [])
      shouldFailed userWithoutPermCtx (fn1 [])
      shouldFailed anonymousCtx (fn1 [])
      let fn2 memberships = do
            deleteUserGroupMemberships
            traverse_ insertUserGroupMembership memberships
            checkMigrationPermissionToProject VisibleViewProjectVisibility permissions
      shouldSucceed adminCtx (fn2 [])
      shouldSucceed ownerCtx (fn2 [])
      shouldSucceed editorCtx (fn2 [])
      shouldFailed viewerCtx (fn2 [])
      shouldSucceed userInOwnerGroupCtx (fn2 [userIsaacBioGroupMembership, userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldSucceed userInEditorGroupCtx (fn2 [userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldFailed userInViewerGroupCtx (fn2 [userIsaacAnimalGroupMembership])
      shouldFailed userWithoutPermCtx (fn2 [])
      shouldFailed anonymousCtx (fn2 [])
      let fn3 memberships = do
            deleteUserGroupMemberships
            traverse_ insertUserGroupMembership memberships
            checkMigrationPermissionToProject VisibleEditProjectVisibility permissions
      shouldSucceed adminCtx (fn3 [])
      shouldSucceed ownerCtx (fn3 [])
      shouldSucceed editorCtx (fn3 [])
      shouldSucceed viewerCtx (fn3 [])
      shouldSucceed userInOwnerGroupCtx (fn3 [userIsaacBioGroupMembership, userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldSucceed userInEditorGroupCtx (fn3 [userIsaacPlantGroupMembership, userIsaacAnimalGroupMembership])
      shouldSucceed userInViewerGroupCtx (fn3 [userIsaacAnimalGroupMembership])
      shouldSucceed userWithoutPermCtx (fn3 [])
      shouldFailed anonymousCtx (fn3 [])

runLocalTestMigration appContext = do
  runInContext deleteProjectPerms appContext
  runInContext deleteProjects appContext
  runInContext (insertPackage germanyKmPackage) appContext
  runInContext (insertDocumentTemplate wizardDocumentTemplate) appContext
  runInContext (insertProject project1) appContext
  runInContext (insertUser userIsaac) appContext
  runInContext (insertUserGroup bioGroup) appContext
  runInContext (insertUserGroup plantGroup) appContext
  runInContext (insertUserGroup animalGroup) appContext
