module Wizard.Specs.API.KnowledgeModelEditor.Common where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditor

import Wizard.Specs.API.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfEditorInDB appContext kmEditor previousPackageId forkOfPackageId createdBy = do
  editorFromDb <- getFirstFromDB findKnowledgeModelEditors appContext
  compareKnowledgeModelEditor editorFromDb kmEditor previousPackageId forkOfPackageId createdBy

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareKnowledgeModelEditor resDto expDto previousPackageId forkOfPackageId createdBy = do
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.kmId `shouldBe` expDto.kmId
  liftIO $ resDto.previousPackageId `shouldBe` previousPackageId
  liftIO $ resDto.createdBy `shouldBe` createdBy

compareEditorDtos resDto expDto previousPackageId forkOfPackageId createdBy = do
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.kmId `shouldBe` expDto.kmId
  liftIO $ resDto.version `shouldBe` expDto.version
  liftIO $ resDto.description `shouldBe` expDto.description
  liftIO $ resDto.readme `shouldBe` expDto.readme
  liftIO $ resDto.license `shouldBe` expDto.license
  liftIO $ resDto.previousPackageId `shouldBe` previousPackageId
  liftIO $ resDto.createdBy `shouldBe` createdBy
