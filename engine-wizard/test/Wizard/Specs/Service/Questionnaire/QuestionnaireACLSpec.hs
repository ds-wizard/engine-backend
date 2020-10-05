module Wizard.Specs.Service.Questionnaire.QuestionnaireACLSpec where

import Control.Lens ((&), (.~), (?~), (^.))
import Test.Hspec

import LensesConfig
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Service.Questionnaire.QuestionnaireACL
import Wizard.Service.User.UserMapper

import Wizard.Specs.Common

questionnaireACLSpec appContext =
  describe "Questionnaire ACL" $ do
    let (admin, adminCtx) = (Just $ userAlbert ^. uuid, appContext & currentUser ?~ toDTO userAlbert)
    let (owner, ownerCtx) = (Just $ userNikola ^. uuid, appContext & currentUser ?~ toDTO userNikola)
    let (nonOwner, nonOwnerCtx) = (Just $ userIsaac ^. uuid, appContext & currentUser ?~ toDTO userIsaac)
    let (anonymous, anonymousCtx) = (Nothing, appContext & currentUser .~ Nothing)
    it "checkViewPermissionToQtn" $ do
      let fn1 = checkViewPermissionToQtn PrivateQuestionnaire RestrictedQuestionnaire owner
      shouldSucceed adminCtx fn1
      shouldSucceed ownerCtx fn1
      shouldFailed nonOwnerCtx fn1
      shouldFailed anonymousCtx fn1
      let fn2 = checkViewPermissionToQtn VisibleViewQuestionnaire RestrictedQuestionnaire owner
      shouldSucceed adminCtx fn2
      shouldSucceed ownerCtx fn2
      shouldSucceed nonOwnerCtx fn2
      shouldFailed anonymousCtx fn2
      let fn3 = checkViewPermissionToQtn VisibleEditQuestionnaire RestrictedQuestionnaire owner
      shouldSucceed adminCtx fn3
      shouldSucceed ownerCtx fn3
      shouldSucceed nonOwnerCtx fn3
      shouldFailed anonymousCtx fn3
      -- --------------------
      let fn4 = checkViewPermissionToQtn PrivateQuestionnaire AnyoneWithLinkViewQuestionnaire owner
      shouldSucceed adminCtx fn4
      shouldSucceed ownerCtx fn4
      shouldSucceed nonOwnerCtx fn4
      shouldSucceed anonymousCtx fn4
      let fn5 = checkViewPermissionToQtn VisibleViewQuestionnaire AnyoneWithLinkViewQuestionnaire owner
      shouldSucceed adminCtx fn5
      shouldSucceed ownerCtx fn5
      shouldSucceed nonOwnerCtx fn5
      shouldSucceed anonymousCtx fn5
      let fn6 = checkViewPermissionToQtn VisibleEditQuestionnaire AnyoneWithLinkViewQuestionnaire owner
      shouldSucceed adminCtx fn6
      shouldSucceed ownerCtx fn6
      shouldSucceed nonOwnerCtx fn6
      shouldSucceed anonymousCtx fn6
      -- --------------------
      let fn7 = checkViewPermissionToQtn PrivateQuestionnaire AnyoneWithLinkEditQuestionnaire owner
      shouldSucceed adminCtx fn7
      shouldSucceed ownerCtx fn7
      shouldSucceed nonOwnerCtx fn7
      shouldSucceed anonymousCtx fn7
      let fn8 = checkViewPermissionToQtn VisibleViewQuestionnaire AnyoneWithLinkEditQuestionnaire owner
      shouldSucceed adminCtx fn8
      shouldSucceed ownerCtx fn8
      shouldSucceed nonOwnerCtx fn8
      shouldSucceed anonymousCtx fn8
      let fn9 = checkViewPermissionToQtn VisibleEditQuestionnaire AnyoneWithLinkEditQuestionnaire owner
      shouldSucceed adminCtx fn9
      shouldSucceed ownerCtx fn9
      shouldSucceed nonOwnerCtx fn9
      shouldSucceed anonymousCtx fn9
    it "checkEditPermissionToQtn" $ do
      let fn1 = checkEditPermissionToQtn PrivateQuestionnaire owner
      shouldSucceed adminCtx fn1
      shouldSucceed ownerCtx fn1
      shouldFailed nonOwnerCtx fn1
      shouldFailed anonymousCtx fn1
      let fn2 = checkEditPermissionToQtn VisibleViewQuestionnaire owner
      shouldSucceed adminCtx fn2
      shouldSucceed ownerCtx fn2
      shouldFailed nonOwnerCtx fn2
      shouldFailed anonymousCtx fn2
      let fn3 = checkEditPermissionToQtn VisibleEditQuestionnaire owner
      shouldSucceed adminCtx fn3
      shouldSucceed ownerCtx fn3
      shouldSucceed nonOwnerCtx fn3
      shouldFailed anonymousCtx fn3
    it "checkEditContentPermissionToQtn" $ do
      let fn1 = checkEditContentPermissionToQtn PrivateQuestionnaire RestrictedQuestionnaire owner
      shouldSucceed adminCtx fn1
      shouldSucceed ownerCtx fn1
      shouldFailed nonOwnerCtx fn1
      shouldFailed anonymousCtx fn1
      let fn2 = checkEditContentPermissionToQtn VisibleViewQuestionnaire RestrictedQuestionnaire owner
      shouldSucceed adminCtx fn2
      shouldSucceed ownerCtx fn2
      shouldFailed nonOwnerCtx fn2
      shouldFailed anonymousCtx fn2
      let fn3 = checkEditContentPermissionToQtn VisibleEditQuestionnaire RestrictedQuestionnaire owner
      shouldSucceed adminCtx fn3
      shouldSucceed ownerCtx fn3
      shouldSucceed nonOwnerCtx fn3
      shouldFailed anonymousCtx fn3
      -- --------------------
      let fn4 = checkEditContentPermissionToQtn PrivateQuestionnaire AnyoneWithLinkViewQuestionnaire owner
      shouldSucceed adminCtx fn4
      shouldSucceed ownerCtx fn4
      shouldFailed nonOwnerCtx fn4
      shouldFailed anonymousCtx fn4
      let fn5 = checkEditContentPermissionToQtn VisibleViewQuestionnaire AnyoneWithLinkViewQuestionnaire owner
      shouldSucceed adminCtx fn5
      shouldSucceed ownerCtx fn5
      shouldFailed nonOwnerCtx fn5
      shouldFailed anonymousCtx fn5
      let fn6 = checkEditContentPermissionToQtn VisibleEditQuestionnaire AnyoneWithLinkViewQuestionnaire owner
      shouldSucceed adminCtx fn6
      shouldSucceed ownerCtx fn6
      shouldSucceed nonOwnerCtx fn6
      shouldFailed anonymousCtx fn6
      -- --------------------
      let fn7 = checkEditContentPermissionToQtn PrivateQuestionnaire AnyoneWithLinkEditQuestionnaire owner
      shouldSucceed adminCtx fn7
      shouldSucceed ownerCtx fn7
      shouldSucceed nonOwnerCtx fn7
      shouldSucceed anonymousCtx fn7
      let fn8 = checkEditContentPermissionToQtn VisibleViewQuestionnaire AnyoneWithLinkEditQuestionnaire owner
      shouldSucceed adminCtx fn8
      shouldSucceed ownerCtx fn8
      shouldSucceed nonOwnerCtx fn8
      shouldSucceed anonymousCtx fn8
      let fn9 = checkEditContentPermissionToQtn VisibleEditQuestionnaire AnyoneWithLinkEditQuestionnaire owner
      shouldSucceed adminCtx fn9
      shouldSucceed ownerCtx fn9
      shouldSucceed nonOwnerCtx fn9
      shouldSucceed anonymousCtx fn9
    it "checkMigrationPermissionToQtn" $ do
      let fn1 = checkMigrationPermissionToQtn PrivateQuestionnaire owner
      shouldSucceed adminCtx fn1
      shouldSucceed ownerCtx fn1
      shouldFailed nonOwnerCtx fn1
      shouldFailed anonymousCtx fn1
      let fn2 = checkMigrationPermissionToQtn VisibleViewQuestionnaire owner
      shouldSucceed adminCtx fn2
      shouldSucceed ownerCtx fn2
      shouldFailed nonOwnerCtx fn2
      shouldFailed anonymousCtx fn2
      let fn3 = checkMigrationPermissionToQtn VisibleEditQuestionnaire owner
      shouldSucceed adminCtx fn3
      shouldSucceed ownerCtx fn3
      shouldSucceed nonOwnerCtx fn3
      shouldFailed anonymousCtx fn3
