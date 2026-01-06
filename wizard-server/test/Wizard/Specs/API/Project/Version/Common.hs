module Wizard.Specs.API.Project.Version.Common where

import Data.Either (isLeft, isRight)
import qualified Data.UUID as U
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Wizard.Database.DAO.Project.ProjectVersionDAO
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Model.Project.Version.ProjectVersion
import Wizard.Model.Tenant.Tenant

import Wizard.Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfProjectVersionInDB appContext version = do
  eVersion <- runInContextIO (findProjectVersionByUuid version.uuid) appContext
  liftIO $ isRight eVersion `shouldBe` True
  let (Right versionFromDb) = eVersion
  compareProjectVersionCreateDtos versionFromDb version

assertAbsenceOfProjectVersionInDB appContext version = do
  eVersion <- runInContextIO (findProjectVersionByUuid version.uuid) appContext
  liftIO $ isLeft eVersion `shouldBe` True
  let (Left error) = eVersion
  liftIO $
    error
      `shouldBe` NotExistsError
        (_ERROR_DATABASE__ENTITY_NOT_FOUND "project_version" [("tenant_uuid", U.toString defaultTenant.uuid), ("uuid", U.toString version.uuid)])

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareProjectVersionCreateDtos resDto expDto = do
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.eventUuid `shouldBe` expDto.eventUuid
