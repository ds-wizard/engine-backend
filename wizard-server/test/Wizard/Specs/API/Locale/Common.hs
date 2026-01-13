module Wizard.Specs.API.Locale.Common where

import Data.Either (isRight)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.Locale.Database.DAO.Locale.LocaleDAO
import Shared.Locale.Model.Locale.Locale
import Wizard.Model.Context.ContextLenses ()

import Wizard.Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfLocaleInDB appContext locale = do
  eLocale <- runInContextIO (findLocaleByUuid locale.uuid) appContext
  liftIO $ isRight eLocale `shouldBe` True
  let (Right localeFromDB) = eLocale
  compareLocaleDtos localeFromDB locale

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareLocaleDtos resDto expDto = do
  liftIO $ resDto.uuid `shouldBe` expDto.uuid
  liftIO $ resDto.enabled `shouldBe` expDto.enabled
  liftIO $ resDto.defaultLocale `shouldBe` expDto.defaultLocale
