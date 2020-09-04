module Wizard.Specs.Service.Questionnaire.Collaboration.CollaborationACLSpec where

import Control.Lens ((^.))
import Test.Hspec

import LensesConfig
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.User.User
import Wizard.Model.Websocket.WebsocketRecord
import Wizard.Service.Questionnaire.Collaboration.CollaborationACL

import Wizard.Specs.Common

questionnaireCollaborationACLSpec appContext =
  describe "Questionnaire Collaboration ACL" $ do
    let (admin, adminRole) = (Just $ userAlbert ^. uuid, Just _USER_ROLE_ADMIN)
    let (owner, ownerRole) = (Just $ userNikola ^. uuid, Just _USER_ROLE_DATA_STEWARD)
    let (nonOwner, nonOwnerRole) = (Just $ userIsaac ^. uuid, Just _USER_ROLE_RESEARCHER)
    let (anonymous, anonymousRole) = (Nothing, Nothing)
    describe "getPermission" $ do
      it "PrivateQuestionnaire RestrictedQuestionnaire owner" $ do
        let fn = getPermission PrivateQuestionnaire RestrictedQuestionnaire owner
        fn admin adminRole `shouldBe` EditEntityPerm
        fn owner ownerRole `shouldBe` EditEntityPerm
        fn nonOwner nonOwnerRole `shouldBe` NoEntityPerm
        fn anonymous anonymousRole `shouldBe` NoEntityPerm
      it "VisibleViewQuestionnaire RestrictedQuestionnaire owner" $ do
        let fn = getPermission VisibleViewQuestionnaire RestrictedQuestionnaire owner
        fn admin adminRole `shouldBe` EditEntityPerm
        fn owner ownerRole `shouldBe` EditEntityPerm
        fn nonOwner nonOwnerRole `shouldBe` ViewEntityPerm
        fn anonymous anonymousRole `shouldBe` NoEntityPerm
      it "VisibleEditQuestionnaire AnyoneWithLinkViewQuestionnaire owner" $ do
        let fn = getPermission VisibleEditQuestionnaire AnyoneWithLinkViewQuestionnaire owner
        fn admin adminRole `shouldBe` EditEntityPerm
        fn owner ownerRole `shouldBe` EditEntityPerm
        fn nonOwner nonOwnerRole `shouldBe` EditEntityPerm
        fn anonymous anonymousRole `shouldBe` ViewEntityPerm
      -- --------------------
      it "PrivateQuestionnaire AnyoneWithLinkViewQuestionnaire owner" $ do
        let fn = getPermission PrivateQuestionnaire AnyoneWithLinkViewQuestionnaire owner
        fn admin adminRole `shouldBe` EditEntityPerm
        fn owner ownerRole `shouldBe` EditEntityPerm
        fn nonOwner nonOwnerRole `shouldBe` ViewEntityPerm
        fn anonymous anonymousRole `shouldBe` ViewEntityPerm
      it "VisibleViewQuestionnaire AnyoneWithLinkViewQuestionnaire owner" $ do
        let fn = getPermission VisibleViewQuestionnaire AnyoneWithLinkViewQuestionnaire owner
        fn admin adminRole `shouldBe` EditEntityPerm
        fn owner ownerRole `shouldBe` EditEntityPerm
        fn nonOwner nonOwnerRole `shouldBe` ViewEntityPerm
        fn anonymous anonymousRole `shouldBe` ViewEntityPerm
      it "VisibleEditQuestionnaire AnyoneWithLinkViewQuestionnaire owner" $ do
        let fn = getPermission VisibleEditQuestionnaire AnyoneWithLinkViewQuestionnaire owner
        fn admin adminRole `shouldBe` EditEntityPerm
        fn owner ownerRole `shouldBe` EditEntityPerm
        fn nonOwner nonOwnerRole `shouldBe` EditEntityPerm
        fn anonymous anonymousRole `shouldBe` ViewEntityPerm
      -- --------------------
      it "PrivateQuestionnaire AnyoneWithLinkEditQuestionnaire owner" $ do
        let fn = getPermission PrivateQuestionnaire AnyoneWithLinkEditQuestionnaire owner
        fn admin adminRole `shouldBe` EditEntityPerm
        fn owner ownerRole `shouldBe` EditEntityPerm
        fn nonOwner nonOwnerRole `shouldBe` EditEntityPerm
        fn anonymous anonymousRole `shouldBe` EditEntityPerm
      it "VisibleViewQuestionnaire AnyoneWithLinkEditQuestionnaire owner" $ do
        let fn = getPermission VisibleViewQuestionnaire AnyoneWithLinkEditQuestionnaire owner
        fn admin adminRole `shouldBe` EditEntityPerm
        fn owner ownerRole `shouldBe` EditEntityPerm
        fn nonOwner nonOwnerRole `shouldBe` EditEntityPerm
        fn anonymous anonymousRole `shouldBe` EditEntityPerm
      it "VisibleEditQuestionnaire AnyoneWithLinkEditQuestionnaire owner" $ do
        let fn = getPermission VisibleEditQuestionnaire AnyoneWithLinkEditQuestionnaire owner
        fn admin adminRole `shouldBe` EditEntityPerm
        fn owner ownerRole `shouldBe` EditEntityPerm
        fn nonOwner nonOwnerRole `shouldBe` EditEntityPerm
        fn anonymous anonymousRole `shouldBe` EditEntityPerm
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
        shouldSucceed appContext (checkViewPermission (record EditEntityPerm))
        shouldSucceed appContext (checkViewPermission (record ViewEntityPerm))
        shouldFailed appContext (checkViewPermission (record NoEntityPerm))
      it "checkEditPermission" $ do
        shouldSucceed appContext (checkEditPermission (record EditEntityPerm))
        shouldFailed appContext (checkEditPermission (record ViewEntityPerm))
        shouldFailed appContext (checkEditPermission (record NoEntityPerm))
