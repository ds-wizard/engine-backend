module Wizard.Specs.Service.Project.Compiler.ProjectCompilerServiceSpec where

import Test.Hspec

import Wizard.Database.Migration.Development.Project.Data.ProjectEvents
import Wizard.Database.Migration.Development.Project.Data.ProjectLabels
import Wizard.Database.Migration.Development.Project.Data.ProjectReplies
import Wizard.Database.Migration.Development.Project.Data.Projects
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Project.Event.ProjectEvent
import Wizard.Model.Project.ProjectContent
import Wizard.Service.Project.Compiler.ProjectCompilerService
import Wizard.Service.Project.Event.ProjectEventMapper

projectCompilerServiceSpec =
  describe "Project Compiler Service" $
    describe "applyEvent" $ do
      it "SetReplyEvent" $
        -- GIVEN:
        do
          let event = toEventList (sre_rQ1Updated' project1Uuid) (Just userAlbert)
          -- WHEN:
          let updatedProjectCtn = applyEvent project1Ctn event
          -- THEN:
          updatedProjectCtn.replies `shouldBe` fRepliesWithUpdated
      it "ClearReplyEvent" $
        -- GIVEN:
        do
          let event = toEventList (cre_rQ1' project1Uuid) (Just userAlbert)
          -- WHEN:
          let updatedProjectCtn = applyEvent project1Ctn event
          -- THEN:
          updatedProjectCtn.replies `shouldBe` fRepliesWithDeleted
      it "SetPhaseEvent" $
        -- GIVEN:
        do
          let event = toEventList (sphse_2' project1Uuid) (Just userAlbert)
          -- WHEN:
          let updatedProjectCtn = applyEvent project1Ctn event
          -- THEN:
          updatedProjectCtn.phaseUuid `shouldBe` (sphse_2 project1Uuid).phaseUuid
      it "SetLabelsEvent" $
        -- GIVEN:
        do
          let event = toEventList (slble_rQ2' project1Uuid) (Just userAlbert)
          -- WHEN:
          let updatedProjectCtn = applyEvent project1Ctn event
          -- THEN:
          updatedProjectCtn.labels `shouldBe` fLabelsEdited
