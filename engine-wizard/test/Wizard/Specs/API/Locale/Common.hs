module Wizard.Specs.API.Locale.Common where

import Data.Either (isRight)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Model.Locale.Locale
import Wizard.Database.DAO.Locale.LocaleDAO

import Wizard.Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfLocaleInDB appContext locale = do
  eLocale <- runInContextIO (findLocaleById locale.lId) appContext
  liftIO $ isRight eLocale `shouldBe` True
  let (Right localeFromDB) = eLocale
  compareLocaleDtos localeFromDB locale

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareLocaleDtos resDto expDto = do
  liftIO $ resDto.lId `shouldBe` expDto.lId
  liftIO $ resDto.enabled `shouldBe` expDto.enabled
  liftIO $ resDto.defaultLocale `shouldBe` expDto.defaultLocale
