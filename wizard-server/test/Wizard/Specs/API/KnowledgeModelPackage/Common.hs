module Wizard.Specs.API.KnowledgeModelPackage.Common where

import Control.Monad (forM_)

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.Coordinate.Model.Coordinate.Coordinate
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage

import Wizard.Specs.API.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfPackageInDB appContext package = do
  packageFromDb <- getOneFromDB (findPackageByUuid package.uuid) appContext
  comparePackageDtos packageFromDb package

assertExistenceOfBundlePackageInDB appContext package = do
  packageFromDb <- getOneFromDB (findPackageByCoordinate (Coordinate package.organizationId package.kmId package.version)) appContext
  comparePackageDtos packageFromDb package

-- --------------------------------
-- COMPARATORS
-- --------------------------------
comparePackageDtosList res exp =
  forM_ (zip res exp) $ uncurry comparePackageDtos

comparePackageDtos resDto expDto = do
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.organizationId `shouldBe` expDto.organizationId
  liftIO $ resDto.kmId `shouldBe` expDto.kmId
  liftIO $ resDto.version `shouldBe` expDto.version
  liftIO $ resDto.description `shouldBe` expDto.description
