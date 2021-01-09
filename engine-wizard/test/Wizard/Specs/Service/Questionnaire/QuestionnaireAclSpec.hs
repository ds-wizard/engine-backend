module Wizard.Specs.Service.Questionnaire.QuestionnaireAclSpec where

import Control.Lens ((&), (.~), (?~), (^.))
import LensesConfig
import Test.Hspec

import Wizard.Database.Migration.Development.Acl.Data.Groups
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireAcl
import Wizard.Service.Questionnaire.QuestionnaireAcl
import Wizard.Service.Questionnaire.QuestionnaireMapper
import qualified Wizard.Service.User.UserMapper as U_Mapper
import Wizard.Specs.Common

-- TODO Add test for groups
questionnaireAclSpec appContext =
  describe "Questionnaire ACL" $ do
    let permissions =
          [ toUserPermRecord (userNikola ^. uuid) ownerPermissions
          , toUserPermRecord (userNicolaus ^. uuid) editorPermissions
          , toUserPermRecord (userGalileo ^. uuid) viewerPermissions
          , toGroupPermRecord (bioGroup ^. gId) ownerPermissions
          , toGroupPermRecord (plantGroup ^. gId) editorPermissions
          , toGroupPermRecord (animalGroup ^. gId) viewerPermissions
          ]
    let makeContext user = appContext & currentUser ?~ U_Mapper.toDTO user
    let adminCtx = makeContext userAlbert
    let ownerCtx = makeContext userNikola
    let editorCtx = makeContext userNicolaus
    let viewerCtx = makeContext userGalileo
    let userInOwnerGroupCtx = makeContext $ userIsaac & groups .~ [memberBioGroup, memberPlantGroup, memberAnimalGroup]
    let userInEditorGroupCtx = makeContext $ userIsaac & groups .~ [memberPlantGroup, memberAnimalGroup]
    let userInViewerGroupCtx = makeContext $ userIsaac & groups .~ [memberAnimalGroup]
    let userWithoutPermCtx = makeContext userIsaac
    let anonymousCtx = appContext & currentUser .~ Nothing
    it "checkViewPermissionToQtn" $ do
      let fn1 = checkViewPermissionToQtn PrivateQuestionnaire RestrictedQuestionnaire permissions
      shouldSucceed adminCtx fn1
      shouldSucceed ownerCtx fn1
      shouldSucceed editorCtx fn1
      shouldSucceed viewerCtx fn1
      shouldSucceed userInOwnerGroupCtx fn1
      shouldSucceed userInEditorGroupCtx fn1
      shouldSucceed userInViewerGroupCtx fn1
      shouldFailed userWithoutPermCtx fn1
      shouldFailed anonymousCtx fn1
      let fn2 = checkViewPermissionToQtn VisibleViewQuestionnaire RestrictedQuestionnaire permissions
      shouldSucceed adminCtx fn2
      shouldSucceed ownerCtx fn2
      shouldSucceed editorCtx fn2
      shouldSucceed viewerCtx fn2
      shouldSucceed userInOwnerGroupCtx fn2
      shouldSucceed userInEditorGroupCtx fn2
      shouldSucceed userInViewerGroupCtx fn2
      shouldSucceed userWithoutPermCtx fn2
      shouldFailed anonymousCtx fn2
      let fn3 = checkViewPermissionToQtn VisibleEditQuestionnaire RestrictedQuestionnaire permissions
      shouldSucceed adminCtx fn3
      shouldSucceed ownerCtx fn3
      shouldSucceed editorCtx fn3
      shouldSucceed viewerCtx fn3
      shouldSucceed userInOwnerGroupCtx fn3
      shouldSucceed userInEditorGroupCtx fn3
      shouldSucceed userInViewerGroupCtx fn3
      shouldSucceed userWithoutPermCtx fn3
      shouldFailed anonymousCtx fn3
      -- --------------------
      let fn4 = checkViewPermissionToQtn PrivateQuestionnaire AnyoneWithLinkViewQuestionnaire permissions
      shouldSucceed adminCtx fn4
      shouldSucceed ownerCtx fn4
      shouldSucceed editorCtx fn4
      shouldSucceed viewerCtx fn4
      shouldSucceed userInOwnerGroupCtx fn4
      shouldSucceed userInEditorGroupCtx fn4
      shouldSucceed userInViewerGroupCtx fn4
      shouldSucceed userWithoutPermCtx fn4
      shouldSucceed anonymousCtx fn4
      let fn5 = checkViewPermissionToQtn VisibleViewQuestionnaire AnyoneWithLinkViewQuestionnaire permissions
      shouldSucceed adminCtx fn5
      shouldSucceed ownerCtx fn5
      shouldSucceed editorCtx fn5
      shouldSucceed viewerCtx fn5
      shouldSucceed userInOwnerGroupCtx fn5
      shouldSucceed userInEditorGroupCtx fn5
      shouldSucceed userInViewerGroupCtx fn5
      shouldSucceed userWithoutPermCtx fn5
      shouldSucceed anonymousCtx fn5
      let fn6 = checkViewPermissionToQtn VisibleEditQuestionnaire AnyoneWithLinkViewQuestionnaire permissions
      shouldSucceed adminCtx fn6
      shouldSucceed ownerCtx fn6
      shouldSucceed editorCtx fn6
      shouldSucceed viewerCtx fn6
      shouldSucceed userInOwnerGroupCtx fn6
      shouldSucceed userInEditorGroupCtx fn6
      shouldSucceed userInViewerGroupCtx fn6
      shouldSucceed userWithoutPermCtx fn6
      shouldSucceed anonymousCtx fn6
      -- --------------------
      let fn7 = checkViewPermissionToQtn PrivateQuestionnaire AnyoneWithLinkEditQuestionnaire permissions
      shouldSucceed adminCtx fn7
      shouldSucceed ownerCtx fn7
      shouldSucceed editorCtx fn7
      shouldSucceed viewerCtx fn7
      shouldSucceed userInOwnerGroupCtx fn7
      shouldSucceed userInEditorGroupCtx fn7
      shouldSucceed userInViewerGroupCtx fn7
      shouldSucceed userWithoutPermCtx fn7
      shouldSucceed anonymousCtx fn7
      let fn8 = checkViewPermissionToQtn VisibleViewQuestionnaire AnyoneWithLinkEditQuestionnaire permissions
      shouldSucceed adminCtx fn8
      shouldSucceed ownerCtx fn8
      shouldSucceed editorCtx fn8
      shouldSucceed viewerCtx fn8
      shouldSucceed userInOwnerGroupCtx fn8
      shouldSucceed userInEditorGroupCtx fn8
      shouldSucceed userInViewerGroupCtx fn8
      shouldSucceed userWithoutPermCtx fn8
      shouldSucceed anonymousCtx fn8
      let fn9 = checkViewPermissionToQtn VisibleEditQuestionnaire AnyoneWithLinkEditQuestionnaire permissions
      shouldSucceed adminCtx fn9
      shouldSucceed ownerCtx fn9
      shouldSucceed editorCtx fn9
      shouldSucceed viewerCtx fn9
      shouldSucceed userInOwnerGroupCtx fn9
      shouldSucceed userInEditorGroupCtx fn9
      shouldSucceed userInViewerGroupCtx fn9
      shouldSucceed userWithoutPermCtx fn9
      shouldSucceed anonymousCtx fn9
    it "checkOwnerPermissionToQtn" $ do
      let fn1 = checkOwnerPermissionToQtn PrivateQuestionnaire permissions
      shouldSucceed adminCtx fn1
      shouldSucceed ownerCtx fn1
      shouldFailed editorCtx fn1
      shouldFailed viewerCtx fn1
      shouldSucceed userInOwnerGroupCtx fn1
      shouldFailed userInEditorGroupCtx fn1
      shouldFailed userInViewerGroupCtx fn1
      shouldFailed userWithoutPermCtx fn1
      shouldFailed anonymousCtx fn1
      let fn2 = checkOwnerPermissionToQtn VisibleViewQuestionnaire permissions
      shouldSucceed adminCtx fn2
      shouldSucceed ownerCtx fn2
      shouldFailed editorCtx fn2
      shouldFailed viewerCtx fn2
      shouldSucceed userInOwnerGroupCtx fn2
      shouldFailed userInEditorGroupCtx fn2
      shouldFailed userInViewerGroupCtx fn2
      shouldFailed userWithoutPermCtx fn2
      shouldFailed anonymousCtx fn2
      let fn3 = checkOwnerPermissionToQtn VisibleEditQuestionnaire permissions
      shouldSucceed adminCtx fn3
      shouldSucceed ownerCtx fn3
      shouldFailed editorCtx fn3
      shouldFailed viewerCtx fn3
      shouldSucceed userInOwnerGroupCtx fn3
      shouldFailed userInEditorGroupCtx fn3
      shouldFailed userInViewerGroupCtx fn3
      shouldFailed userWithoutPermCtx fn3
      shouldFailed anonymousCtx fn3
    it "checkEditPermissionToQtn" $ do
      let fn1 = checkEditPermissionToQtn PrivateQuestionnaire RestrictedQuestionnaire permissions
      shouldSucceed adminCtx fn1
      shouldSucceed ownerCtx fn1
      shouldSucceed editorCtx fn1
      shouldFailed viewerCtx fn1
      shouldSucceed userInOwnerGroupCtx fn1
      shouldSucceed userInEditorGroupCtx fn1
      shouldFailed userInViewerGroupCtx fn1
      shouldFailed userWithoutPermCtx fn1
      shouldFailed anonymousCtx fn1
      let fn2 = checkEditPermissionToQtn VisibleViewQuestionnaire RestrictedQuestionnaire permissions
      shouldSucceed adminCtx fn2
      shouldSucceed ownerCtx fn2
      shouldSucceed editorCtx fn2
      shouldFailed viewerCtx fn2
      shouldSucceed userInOwnerGroupCtx fn2
      shouldSucceed userInEditorGroupCtx fn2
      shouldFailed userInViewerGroupCtx fn2
      shouldFailed userWithoutPermCtx fn2
      shouldFailed anonymousCtx fn2
      let fn3 = checkEditPermissionToQtn VisibleEditQuestionnaire RestrictedQuestionnaire permissions
      shouldSucceed adminCtx fn3
      shouldSucceed ownerCtx fn3
      shouldSucceed editorCtx fn3
      shouldSucceed viewerCtx fn3
      shouldSucceed userInOwnerGroupCtx fn3
      shouldSucceed userInEditorGroupCtx fn3
      shouldSucceed userInViewerGroupCtx fn3
      shouldSucceed userWithoutPermCtx fn3
      shouldFailed anonymousCtx fn3
      -- --------------------
      let fn4 = checkEditPermissionToQtn PrivateQuestionnaire AnyoneWithLinkViewQuestionnaire permissions
      shouldSucceed adminCtx fn4
      shouldSucceed ownerCtx fn4
      shouldSucceed editorCtx fn4
      shouldFailed viewerCtx fn4
      shouldSucceed userInOwnerGroupCtx fn4
      shouldSucceed userInEditorGroupCtx fn4
      shouldFailed userInViewerGroupCtx fn4
      shouldFailed userWithoutPermCtx fn4
      shouldFailed anonymousCtx fn4
      let fn5 = checkEditPermissionToQtn VisibleViewQuestionnaire AnyoneWithLinkViewQuestionnaire permissions
      shouldSucceed adminCtx fn5
      shouldSucceed ownerCtx fn5
      shouldSucceed editorCtx fn5
      shouldFailed viewerCtx fn5
      shouldSucceed userInOwnerGroupCtx fn5
      shouldSucceed userInEditorGroupCtx fn5
      shouldFailed userInViewerGroupCtx fn5
      shouldFailed userWithoutPermCtx fn5
      shouldFailed anonymousCtx fn5
      let fn6 = checkEditPermissionToQtn VisibleEditQuestionnaire AnyoneWithLinkViewQuestionnaire permissions
      shouldSucceed adminCtx fn6
      shouldSucceed ownerCtx fn6
      shouldSucceed editorCtx fn6
      shouldSucceed viewerCtx fn6
      shouldSucceed userInOwnerGroupCtx fn6
      shouldSucceed userInEditorGroupCtx fn6
      shouldSucceed userInViewerGroupCtx fn6
      shouldSucceed userWithoutPermCtx fn6
      shouldFailed anonymousCtx fn6
      -- --------------------
      let fn7 = checkEditPermissionToQtn PrivateQuestionnaire AnyoneWithLinkEditQuestionnaire permissions
      shouldSucceed adminCtx fn7
      shouldSucceed ownerCtx fn7
      shouldSucceed editorCtx fn7
      shouldSucceed viewerCtx fn7
      shouldSucceed userInOwnerGroupCtx fn7
      shouldSucceed userInEditorGroupCtx fn7
      shouldSucceed userInViewerGroupCtx fn7
      shouldSucceed userWithoutPermCtx fn7
      shouldSucceed anonymousCtx fn7
      let fn8 = checkEditPermissionToQtn VisibleViewQuestionnaire AnyoneWithLinkEditQuestionnaire permissions
      shouldSucceed adminCtx fn8
      shouldSucceed ownerCtx fn8
      shouldSucceed editorCtx fn8
      shouldSucceed viewerCtx fn8
      shouldSucceed userInOwnerGroupCtx fn8
      shouldSucceed userInEditorGroupCtx fn8
      shouldSucceed userInViewerGroupCtx fn8
      shouldSucceed userWithoutPermCtx fn8
      shouldSucceed anonymousCtx fn8
      let fn9 = checkEditPermissionToQtn VisibleEditQuestionnaire AnyoneWithLinkEditQuestionnaire permissions
      shouldSucceed adminCtx fn9
      shouldSucceed ownerCtx fn9
      shouldSucceed editorCtx fn9
      shouldSucceed viewerCtx fn9
      shouldSucceed userInOwnerGroupCtx fn9
      shouldSucceed userInEditorGroupCtx fn9
      shouldSucceed userInViewerGroupCtx fn9
      shouldSucceed userWithoutPermCtx fn9
      shouldSucceed anonymousCtx fn9
    it "checkMigrationPermissionToQtn" $ do
      let fn1 = checkMigrationPermissionToQtn PrivateQuestionnaire permissions
      shouldSucceed adminCtx fn1
      shouldSucceed ownerCtx fn1
      shouldSucceed editorCtx fn1
      shouldSucceed userInOwnerGroupCtx fn1
      shouldSucceed userInEditorGroupCtx fn1
      shouldFailed userInViewerGroupCtx fn1
      shouldFailed viewerCtx fn1
      shouldFailed userWithoutPermCtx fn1
      shouldFailed anonymousCtx fn1
      let fn2 = checkMigrationPermissionToQtn VisibleViewQuestionnaire permissions
      shouldSucceed adminCtx fn2
      shouldSucceed ownerCtx fn2
      shouldSucceed editorCtx fn2
      shouldFailed viewerCtx fn2
      shouldSucceed userInOwnerGroupCtx fn2
      shouldSucceed userInEditorGroupCtx fn2
      shouldFailed userInViewerGroupCtx fn2
      shouldFailed userWithoutPermCtx fn2
      shouldFailed anonymousCtx fn2
      let fn3 = checkMigrationPermissionToQtn VisibleEditQuestionnaire permissions
      shouldSucceed adminCtx fn3
      shouldSucceed ownerCtx fn3
      shouldSucceed editorCtx fn3
      shouldSucceed viewerCtx fn3
      shouldSucceed userInOwnerGroupCtx fn3
      shouldSucceed userInEditorGroupCtx fn3
      shouldSucceed userInViewerGroupCtx fn3
      shouldSucceed userWithoutPermCtx fn3
      shouldFailed anonymousCtx fn3
