module Wizard.Specs.API.Project.Migration.Common where

import Data.Either (isRight)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Wizard.Database.DAO.Project.ProjectMigrationDAO
import Wizard.Model.Project.Migration.ProjectMigration

import Wizard.Specs.API.Project.Common
import Wizard.Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfMigrationStateInDB appContext entity = do
  eEntitiesFromDb <-
    runInContextIO (findProjectMigrationsByOldProjectUuid entity.oldProjectUuid) appContext
  liftIO $ isRight eEntitiesFromDb `shouldBe` True
  let (Right entitiesFromDb) = eEntitiesFromDb
  liftIO $ length entitiesFromDb `shouldBe` 1
  let entityFromDb = head entitiesFromDb
  compareProjectMigrators entityFromDb entity

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareProjectMigrators resDto expDto = do
  liftIO $ resDto.oldProjectUuid `shouldBe` expDto.oldProjectUuid
  liftIO $ resDto.newProjectUuid `shouldBe` expDto.newProjectUuid
  liftIO $ resDto.resolvedQuestionUuids `shouldBe` expDto.resolvedQuestionUuids

compareProjectMigratorDtos resDto expDto = do
  compareProjectCreateDtos'' resDto.oldProject expDto.oldProject
  compareProjectCreateDtos'' resDto.newProject expDto.newProject
  liftIO $ resDto.resolvedQuestionUuids `shouldBe` expDto.resolvedQuestionUuids
