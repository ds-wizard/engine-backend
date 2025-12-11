module Wizard.Specs.API.ProjectImporter.Common where

import Data.Either (isRight)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Wizard.Database.DAO.Project.ProjectImporterDAO
import Wizard.Model.Project.Importer.ProjectImporter

import Wizard.Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfProjectImporterInDB appContext importer = do
  eProjectImporter <- runInContextIO (findProjectImporterById importer.piId) appContext
  liftIO $ isRight eProjectImporter `shouldBe` True
  let (Right importerFromDB) = eProjectImporter
  compareProjectImporterDtos importerFromDB importer

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareProjectImporterDtos resDto expDto = do
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.enabled `shouldBe` expDto.enabled
