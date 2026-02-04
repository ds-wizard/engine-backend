module Registry.Specs.API.KnowledgeModelPackage.Common where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage

import Registry.Specs.API.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfPackageInDB appContext kmPackage = do
  packageFromDb <- getOneFromDB (findPackageByUuid kmPackage.uuid) appContext
  comparePackageDtos packageFromDb kmPackage

-- --------------------------------
-- COMPARATORS
-- --------------------------------
comparePackageDtos resDto expDto = do
  liftIO $ resDto.uuid `shouldBe` expDto.uuid
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.organizationId `shouldBe` expDto.organizationId
  liftIO $ resDto.kmId `shouldBe` expDto.kmId
  liftIO $ resDto.version `shouldBe` expDto.version
  liftIO $ resDto.description `shouldBe` expDto.description
  liftIO $ resDto.previousPackageUuid `shouldBe` expDto.previousPackageUuid
