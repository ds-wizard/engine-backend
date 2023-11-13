module Wizard.Specs.Service.Questionnaire.Collaboration.CollaborationAclSpec where

import Test.Hspec

import Shared.Common.Util.Uuid
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnairePerm
import Wizard.Model.User.User
import Wizard.Model.Websocket.WebsocketRecord
import Wizard.Service.Questionnaire.Collaboration.CollaborationAcl
import Wizard.Service.Questionnaire.QuestionnaireMapper
import WizardLib.Public.Database.Migration.Development.User.Data.UserGroups
import WizardLib.Public.Model.User.UserGroup
import WizardLib.Public.Model.User.UserGroupMembership

import Wizard.Specs.Common

questionnaireCollaborationAclSpec appContext =
  describe "Questionnaire Collaboration ACL" $ do
    let permissions =
          [ toUserQuestionnairePerm
              (u' "808d4770-0d38-45b0-a028-0a3ffaafc617")
              userNikola.uuid
              ownerPermissions
              userNikola.tenantUuid
          , toUserQuestionnairePerm
              (u' "52e74b8b-ca73-4d6a-a7e1-5c0f34a1819a")
              userNicolaus.uuid
              editorPermissions
              userNicolaus.tenantUuid
          , toUserQuestionnairePerm
              (u' "3d60a813-5e54-4fd2-8fe8-5cf3c076bd50")
              userGalileo.uuid
              viewerPermissions
              userGalileo.tenantUuid
          , toUserGroupQuestionnairePerm
              (u' "b90b17f4-06a2-40dc-b364-88d8f195c8a0")
              bioGroup.uuid
              ownerPermissions
              bioGroup.tenantUuid
          , toUserGroupQuestionnairePerm
              (u' "fcead22d-e453-47a3-84bc-5c29698ab990")
              plantGroup.uuid
              editorPermissions
              plantGroup.tenantUuid
          , toUserGroupQuestionnairePerm
              (u' "4e0765e8-f25c-4091-8531-aed2eef161f6")
              animalGroup.uuid
              viewerPermissions
              animalGroup.tenantUuid
          ]
    let (admin, adminRole, adminGroups) = (Just userAlbert.uuid, Just _USER_ROLE_ADMIN, [])
    let (owner, ownerRole, ownerGroups) = (Just userNikola.uuid, Just _USER_ROLE_RESEARCHER, [])
    let (editor, editorRole, editorGroups) = (Just userNicolaus.uuid, Just _USER_ROLE_RESEARCHER, [])
    let (viewer, viewerRole, viewerGroups) = (Just userGalileo.uuid, Just _USER_ROLE_RESEARCHER, [])
    let (userInOwnerGroup, userInOwnerGroupRole, userInOwnerGroupGroups) =
          ( Just userIsaac.uuid
          , Just _USER_ROLE_RESEARCHER
          , [userIsaacBioGroupMembership.userGroupUuid, userIsaacPlantGroupMembership.userGroupUuid, userIsaacAnimalGroupMembership.userGroupUuid]
          )
    let (userInEditorGroup, userInEditorGroupRole, userInEditorGroupGroups) =
          ( Just userIsaac.uuid
          , Just _USER_ROLE_RESEARCHER
          , [userIsaacPlantGroupMembership.userGroupUuid, userIsaacAnimalGroupMembership.userGroupUuid]
          )
    let (userInViewerGroup, userInViewerGroupRole, userInViewerGroupGroups) =
          ( Just userIsaac.uuid
          , Just _USER_ROLE_RESEARCHER
          , [userIsaacAnimalGroupMembership.userGroupUuid]
          )
    let (userWithoutPerm, userWithoutPermRole, userWithoutPermGroups) =
          (Just userIsaac.uuid, Just _USER_ROLE_RESEARCHER, [])
    let (anonymous, anonymousRole, anonymousGroups) = (Nothing, Nothing, [])
    describe "getPermission" $ do
      it "PrivateQuestionnaire RestrictedQuestionnaire" $ do
        let fn = getPermission PrivateQuestionnaire RestrictedQuestionnaire permissions
        fn admin adminRole adminGroups `shouldBe` EditorWebsocketPerm
        fn owner ownerRole ownerGroups `shouldBe` EditorWebsocketPerm
        fn editor editorRole editorGroups `shouldBe` EditorWebsocketPerm
        fn viewer viewerRole viewerGroups `shouldBe` ViewerWebsocketPerm
        fn userInOwnerGroup userInOwnerGroupRole userInOwnerGroupGroups `shouldBe` EditorWebsocketPerm
        fn userInEditorGroup userInEditorGroupRole userInEditorGroupGroups `shouldBe` EditorWebsocketPerm
        fn userInViewerGroup userInViewerGroupRole userInViewerGroupGroups `shouldBe` ViewerWebsocketPerm
        fn userWithoutPerm userWithoutPermRole userWithoutPermGroups `shouldBe` NoWebsocketPerm
        fn anonymous anonymousRole anonymousGroups `shouldBe` NoWebsocketPerm
      it "VisibleViewQuestionnaire RestrictedQuestionnaire" $ do
        let fn = getPermission VisibleViewQuestionnaire RestrictedQuestionnaire permissions
        fn admin adminRole adminGroups `shouldBe` EditorWebsocketPerm
        fn owner ownerRole ownerGroups `shouldBe` EditorWebsocketPerm
        fn editor editorRole editorGroups `shouldBe` EditorWebsocketPerm
        fn viewer viewerRole viewerGroups `shouldBe` ViewerWebsocketPerm
        fn userInOwnerGroup userInOwnerGroupRole userInOwnerGroupGroups `shouldBe` EditorWebsocketPerm
        fn userInEditorGroup userInEditorGroupRole userInEditorGroupGroups `shouldBe` EditorWebsocketPerm
        fn userInViewerGroup userInViewerGroupRole userInViewerGroupGroups `shouldBe` ViewerWebsocketPerm
        fn userWithoutPerm userWithoutPermRole userWithoutPermGroups `shouldBe` ViewerWebsocketPerm
        fn anonymous anonymousRole anonymousGroups `shouldBe` NoWebsocketPerm
      it "VisibleEditQuestionnaire AnyoneWithLinkViewQuestionnaire" $ do
        let fn = getPermission VisibleEditQuestionnaire AnyoneWithLinkViewQuestionnaire permissions
        fn admin adminRole adminGroups `shouldBe` EditorWebsocketPerm
        fn owner ownerRole ownerGroups `shouldBe` EditorWebsocketPerm
        fn editor editorRole editorGroups `shouldBe` EditorWebsocketPerm
        fn viewer viewerRole viewerGroups `shouldBe` EditorWebsocketPerm
        fn userWithoutPerm userWithoutPermRole userWithoutPermGroups `shouldBe` EditorWebsocketPerm
        fn anonymous anonymousRole anonymousGroups `shouldBe` ViewerWebsocketPerm
      -- --------------------
      it "PrivateQuestionnaire AnyoneWithLinkViewQuestionnaire" $ do
        let fn = getPermission PrivateQuestionnaire AnyoneWithLinkViewQuestionnaire permissions
        fn admin adminRole adminGroups `shouldBe` EditorWebsocketPerm
        fn owner ownerRole ownerGroups `shouldBe` EditorWebsocketPerm
        fn editor editorRole editorGroups `shouldBe` EditorWebsocketPerm
        fn viewer viewerRole viewerGroups `shouldBe` ViewerWebsocketPerm
        fn userInOwnerGroup userInOwnerGroupRole userInOwnerGroupGroups `shouldBe` EditorWebsocketPerm
        fn userInEditorGroup userInEditorGroupRole userInEditorGroupGroups `shouldBe` EditorWebsocketPerm
        fn userInViewerGroup userInViewerGroupRole userInViewerGroupGroups `shouldBe` ViewerWebsocketPerm
        fn userWithoutPerm userWithoutPermRole userWithoutPermGroups `shouldBe` ViewerWebsocketPerm
        fn anonymous anonymousRole anonymousGroups `shouldBe` ViewerWebsocketPerm
      it "VisibleViewQuestionnaire AnyoneWithLinkViewQuestionnaire" $ do
        let fn = getPermission VisibleViewQuestionnaire AnyoneWithLinkViewQuestionnaire permissions
        fn admin adminRole adminGroups `shouldBe` EditorWebsocketPerm
        fn owner ownerRole ownerGroups `shouldBe` EditorWebsocketPerm
        fn editor editorRole editorGroups `shouldBe` EditorWebsocketPerm
        fn viewer viewerRole viewerGroups `shouldBe` ViewerWebsocketPerm
        fn userInOwnerGroup userInOwnerGroupRole userInOwnerGroupGroups `shouldBe` EditorWebsocketPerm
        fn userInEditorGroup userInEditorGroupRole userInEditorGroupGroups `shouldBe` EditorWebsocketPerm
        fn userInViewerGroup userInViewerGroupRole userInViewerGroupGroups `shouldBe` ViewerWebsocketPerm
        fn userWithoutPerm userWithoutPermRole userWithoutPermGroups `shouldBe` ViewerWebsocketPerm
        fn anonymous anonymousRole anonymousGroups `shouldBe` ViewerWebsocketPerm
      it "VisibleEditQuestionnaire AnyoneWithLinkViewQuestionnaire" $ do
        let fn = getPermission VisibleEditQuestionnaire AnyoneWithLinkViewQuestionnaire permissions
        fn admin adminRole adminGroups `shouldBe` EditorWebsocketPerm
        fn owner ownerRole ownerGroups `shouldBe` EditorWebsocketPerm
        fn editor editorRole editorGroups `shouldBe` EditorWebsocketPerm
        fn viewer viewerRole viewerGroups `shouldBe` EditorWebsocketPerm
        fn userInOwnerGroup userInOwnerGroupRole userInOwnerGroupGroups `shouldBe` EditorWebsocketPerm
        fn userInEditorGroup userInEditorGroupRole userInEditorGroupGroups `shouldBe` EditorWebsocketPerm
        fn userInViewerGroup userInViewerGroupRole userInViewerGroupGroups `shouldBe` EditorWebsocketPerm
        fn userWithoutPerm userWithoutPermRole userWithoutPermGroups `shouldBe` EditorWebsocketPerm
        fn anonymous anonymousRole anonymousGroups `shouldBe` ViewerWebsocketPerm
      -- --------------------
      it "PrivateQuestionnaire AnyoneWithLinkEditQuestionnaire" $ do
        let fn = getPermission PrivateQuestionnaire AnyoneWithLinkEditQuestionnaire permissions
        fn admin adminRole adminGroups `shouldBe` EditorWebsocketPerm
        fn owner ownerRole ownerGroups `shouldBe` EditorWebsocketPerm
        fn editor editorRole editorGroups `shouldBe` EditorWebsocketPerm
        fn viewer viewerRole viewerGroups `shouldBe` EditorWebsocketPerm
        fn userInOwnerGroup userInOwnerGroupRole userInOwnerGroupGroups `shouldBe` EditorWebsocketPerm
        fn userInEditorGroup userInEditorGroupRole userInEditorGroupGroups `shouldBe` EditorWebsocketPerm
        fn userInViewerGroup userInViewerGroupRole userInViewerGroupGroups `shouldBe` EditorWebsocketPerm
        fn userWithoutPerm userWithoutPermRole userWithoutPermGroups `shouldBe` EditorWebsocketPerm
        fn anonymous anonymousRole anonymousGroups `shouldBe` EditorWebsocketPerm
      it "VisibleViewQuestionnaire AnyoneWithLinkEditQuestionnaire" $ do
        let fn = getPermission VisibleViewQuestionnaire AnyoneWithLinkEditQuestionnaire permissions
        fn admin adminRole adminGroups `shouldBe` EditorWebsocketPerm
        fn owner ownerRole ownerGroups `shouldBe` EditorWebsocketPerm
        fn editor editorRole editorGroups `shouldBe` EditorWebsocketPerm
        fn viewer viewerRole viewerGroups `shouldBe` EditorWebsocketPerm
        fn userInOwnerGroup userInOwnerGroupRole userInOwnerGroupGroups `shouldBe` EditorWebsocketPerm
        fn userInEditorGroup userInEditorGroupRole userInEditorGroupGroups `shouldBe` EditorWebsocketPerm
        fn userInViewerGroup userInViewerGroupRole userInViewerGroupGroups `shouldBe` EditorWebsocketPerm
        fn userWithoutPerm userWithoutPermRole userWithoutPermGroups `shouldBe` EditorWebsocketPerm
        fn anonymous anonymousRole anonymousGroups `shouldBe` EditorWebsocketPerm
      it "VisibleEditQuestionnaire AnyoneWithLinkEditQuestionnaire" $ do
        let fn = getPermission VisibleEditQuestionnaire AnyoneWithLinkEditQuestionnaire permissions
        fn admin adminRole adminGroups `shouldBe` EditorWebsocketPerm
        fn owner ownerRole ownerGroups `shouldBe` EditorWebsocketPerm
        fn editor editorRole editorGroups `shouldBe` EditorWebsocketPerm
        fn viewer viewerRole viewerGroups `shouldBe` EditorWebsocketPerm
        fn userInOwnerGroup userInOwnerGroupRole userInOwnerGroupGroups `shouldBe` EditorWebsocketPerm
        fn userInEditorGroup userInEditorGroupRole userInEditorGroupGroups `shouldBe` EditorWebsocketPerm
        fn userInViewerGroup userInViewerGroupRole userInViewerGroupGroups `shouldBe` EditorWebsocketPerm
        fn userWithoutPerm userWithoutPermRole userWithoutPermGroups `shouldBe` EditorWebsocketPerm
        fn anonymous anonymousRole anonymousGroups `shouldBe` EditorWebsocketPerm
    describe "check permissions" $ do
      let record perm =
            WebsocketRecord
              { connectionUuid = undefined
              , connection = undefined
              , entityId = undefined
              , entityPerm = perm
              , user = undefined
              }
      it "checkViewPermission" $ do
        shouldSucceed appContext (checkViewPermission (record EditorWebsocketPerm))
        shouldSucceed appContext (checkViewPermission (record ViewerWebsocketPerm))
        shouldFailed appContext (checkViewPermission (record NoWebsocketPerm))
      it "checkEditPermission" $ do
        shouldSucceed appContext (checkEditPermission (record EditorWebsocketPerm))
        shouldFailed appContext (checkEditPermission (record ViewerWebsocketPerm))
        shouldFailed appContext (checkEditPermission (record NoWebsocketPerm))
