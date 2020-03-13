module Wizard.Specs.API.Config.Common where

import Data.Either (isRight)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Error.ErrorJM ()
import Wizard.Database.DAO.Config.AppConfigDAO

import Wizard.Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfAppConfigInDB appContext appConfig = do
  eitherAppConfig <- runInContextIO findAppConfig appContext
  liftIO $ isRight eitherAppConfig `shouldBe` True
  let (Right appConfigFromDb) = eitherAppConfig
  compareAppConfigDtos appConfigFromDb appConfig

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareAppConfigDtos resDto expDto = liftIO $ (resDto == expDto) `shouldBe` True
