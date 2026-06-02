module Wizard.Specs.API.OpenIdClient.Common where

import Data.Either (isRight)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Model.Context.AppContext ()
import Wizard.Model.Context.ContextLenses ()
import WizardLib.Public.Api.Resource.OpenId.Client.Definition.OpenIdClientDetailDTO
import WizardLib.Public.Database.DAO.OpenId.OpenIdClientDefinitionDAO
import WizardLib.Public.Model.OpenId.OpenIdClient

import Wizard.Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfOpenIdClientInDB appContext openIdClient = do
  eOpenIdClient <- runInContextIO (findOpenIdClientDefinitionByUuid openIdClient.uuid) appContext
  liftIO $ isRight eOpenIdClient `shouldBe` True
  let (Right openIdClientFromDB) = eOpenIdClient
  compareOpenIdClients openIdClientFromDB openIdClient

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareOpenIdClients :: OpenIdClient -> OpenIdClient -> WaiSession st ()
compareOpenIdClients resModel expModel = do
  liftIO $ resModel.uuid `shouldBe` expModel.uuid
  liftIO $ resModel.name `shouldBe` expModel.name
  liftIO $ resModel.url `shouldBe` expModel.url
  liftIO $ resModel.clientId `shouldBe` expModel.clientId
  liftIO $ resModel.clientSecret `shouldBe` expModel.clientSecret
  liftIO $ resModel.parameters `shouldBe` expModel.parameters
  liftIO $ resModel.style `shouldBe` expModel.style
  liftIO $ resModel.registrationEnabled `shouldBe` expModel.registrationEnabled
  liftIO $ resModel.scopeProfile `shouldBe` expModel.scopeProfile
  liftIO $ resModel.scopeEmail `shouldBe` expModel.scopeEmail
  liftIO $ resModel.tenantUuid `shouldBe` expModel.tenantUuid

compareOpenIdClientDetailDtos :: OpenIdClientDetailDTO -> OpenIdClientDetailDTO -> WaiSession st ()
compareOpenIdClientDetailDtos resDto expDto = do
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.url `shouldBe` expDto.url
  liftIO $ resDto.clientId `shouldBe` expDto.clientId
  liftIO $ resDto.clientSecret `shouldBe` expDto.clientSecret
  liftIO $ resDto.parameters `shouldBe` expDto.parameters
  liftIO $ resDto.style `shouldBe` expDto.style
  liftIO $ resDto.registrationEnabled `shouldBe` expDto.registrationEnabled
  liftIO $ resDto.scopeProfile `shouldBe` expDto.scopeProfile
  liftIO $ resDto.scopeEmail `shouldBe` expDto.scopeEmail
  liftIO $ resDto.tenantUuid `shouldBe` expDto.tenantUuid
