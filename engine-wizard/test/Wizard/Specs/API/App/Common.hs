module Wizard.Specs.API.App.Common where

import qualified Data.UUID as U
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Error.ErrorJM ()
import Wizard.Database.DAO.App.AppDAO
import Wizard.Model.App.App

import Wizard.Specs.API.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfAppInDB appContext app = do
  appFromDb <- getOneFromDB (findAppById . U.toString $ app.uuid) appContext
  compareAppDtos appFromDb app

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareAppDtos resDto expDto = do
  liftIO $ resDto.appId `shouldBe` expDto.appId
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.serverDomain `shouldBe` expDto.serverDomain
  liftIO $ resDto.serverUrl `shouldBe` expDto.serverUrl
  liftIO $ resDto.clientUrl `shouldBe` expDto.clientUrl
  liftIO $ resDto.enabled `shouldBe` expDto.enabled
