module Wizard.Specs.API.Config.Common where

import Data.Either (isRight)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Error.ErrorJM ()
import Wizard.Service.Config.AppConfigService

import Wizard.Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfAppConfigInDB appContext appConfig = do
  eitherAppConfig <- runInContextIO getAppConfig appContext
  liftIO $ isRight eitherAppConfig `shouldBe` True
  let (Right appConfigFromDb) = eitherAppConfig
  compareDtos appConfigFromDb appConfig

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareDtos resDto expDto = liftIO $ (resDto == expDto) `shouldBe` True
