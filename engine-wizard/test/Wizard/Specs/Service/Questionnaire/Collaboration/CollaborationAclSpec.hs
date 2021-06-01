module Wizard.Specs.Service.Questionnaire.Collaboration.CollaborationAclSpec where

import Control.Lens ((^.))
import Test.Hspec

import LensesConfig
import Shared.Util.Uuid
import Wizard.Database.Migration.Development.Acl.Data.Groups
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireAcl
import Wizard.Model.User.User
import Wizard.Model.Websocket.WebsocketRecord
import Wizard.Service.Questionnaire.Collaboration.CollaborationAcl
import Wizard.Service.Questionnaire.QuestionnaireMapper

import Wizard.Specs.Common

questionnaireCollaborationAclSpec appContext =
  describe "Questionnaire Collaboration ACL" $ do
    let permissions =
          [ toUserPermRecord
              (u' "4ccb62e9-cae6-48c7-81a1-0af7b46a8743")
              (u' "808d4770-0d38-45b0-a028-0a3ffaafc617")
              (userNikola ^. uuid)
              ownerPermissions
          , toUserPermRecord
              (u' "3776a51f-c052-403e-ab7d-6b69e9ed0550")
              (u' "52e74b8b-ca73-4d6a-a7e1-5c0f34a1819a")
              (userNicolaus ^. uuid)
              editorPermissions
          , toUserPermRecord
              (u' "8f5aeee2-3f88-44e3-8369-b84a1ce88cac")
              (u' "3d60a813-5e54-4fd2-8fe8-5cf3c076bd50")
              (userGalileo ^. uuid)
              viewerPermissions
          , toGroupPermRecord
              (u' "91bdee5f-7a4c-43f8-986d-6ecd6d392d0d")
              (u' "b90b17f4-06a2-40dc-b364-88d8f195c8a0")
              (bioGroup ^. gId)
              ownerPermissions
          , toGroupPermRecord
              (u' "e8010761-34d7-4fe8-a482-fdfa04685d61")
              (u' "fcead22d-e453-47a3-84bc-5c29698ab990")
              (plantGroup ^. gId)
              editorPermissions
          , toGroupPermRecord
              (u' "e41e9550-429b-41c3-a364-f6e7f26f2d13")
              (u' "4e0765e8-f25c-4091-8531-aed2eef161f6")
              (animalGroup ^. gId)
              viewerPermissions
          ]
    let (admin, adminRole, adminGroups) = (Just $ userAlbert ^. uuid, Just _USER_ROLE_ADMIN, Just [])
    let (owner, ownerRole, ownerGroups) = (Just $ userNikola ^. uuid, Just _USER_ROLE_RESEARCHER, Just [])
    let (editor, editorRole, editorGroups) = (Just $ userNicolaus ^. uuid, Just _USER_ROLE_RESEARCHER, Just [])
    let (viewer, viewerRole, viewerGroups) = (Just $ userGalileo ^. uuid, Just _USER_ROLE_RESEARCHER, Just [])
    let (userInOwnerGroup, userInOwnerGroupRole, userInOwnerGroupGroups) =
          ( Just $ userIsaac ^. uuid
          , Just _USER_ROLE_RESEARCHER
          , Just [memberBioGroup, memberPlantGroup, memberAnimalGroup])
    let (userInEditorGroup, userInEditorGroupRole, userInEditorGroupGroups) =
          (Just $ userIsaac ^. uuid, Just _USER_ROLE_RESEARCHER, Just [memberPlantGroup, memberAnimalGroup])
    let (userInViewerGroup, userInViewerGroupRole, userInViewerGroupGroups) =
          (Just $ userIsaac ^. uuid, Just _USER_ROLE_RESEARCHER, Just [memberAnimalGroup])
    let (userWithoutPerm, userWithoutPermRole, userWithoutPermGroups) =
          (Just $ userIsaac ^. uuid, Just _USER_ROLE_RESEARCHER, Just [])
    let (anonymous, anonymousRole, anonymousGroups) = (Nothing, Nothing, Nothing)
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
              { _websocketRecordConnectionUuid = undefined
              , _websocketRecordConnection = undefined
              , _websocketRecordEntityId = undefined
              , _websocketRecordEntityPerm = perm
              , _websocketRecordUser = undefined
              }
      it "checkViewPermission" $ do
        shouldSucceed appContext (checkViewPermission (record EditorWebsocketPerm))
        shouldSucceed appContext (checkViewPermission (record ViewerWebsocketPerm))
        shouldFailed appContext (checkViewPermission (record NoWebsocketPerm))
      it "checkEditPermission" $ do
        shouldSucceed appContext (checkEditPermission (record EditorWebsocketPerm))
        shouldFailed appContext (checkEditPermission (record ViewerWebsocketPerm))
        shouldFailed appContext (checkEditPermission (record NoWebsocketPerm))
