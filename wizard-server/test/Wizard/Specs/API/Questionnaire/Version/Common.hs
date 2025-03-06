module Wizard.Specs.API.Questionnaire.Version.Common where

import Data.Either (isLeft, isRight)
import qualified Data.UUID as U
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Wizard.Database.DAO.Questionnaire.QuestionnaireVersionDAO
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Model.Questionnaire.QuestionnaireVersion
import Wizard.Model.Tenant.Tenant

import Wizard.Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfQuestionnaireVersionInDB appContext version = do
  eVersion <- runInContextIO (findQuestionnaireVersionByUuid version.uuid) appContext
  liftIO $ isRight eVersion `shouldBe` True
  let (Right versionFromDb) = eVersion
  compareQuestionnaireVersionCreateDtos versionFromDb version

assertAbsenceOfQuestionnaireVersionInDB appContext version = do
  eVersion <- runInContextIO (findQuestionnaireVersionByUuid version.uuid) appContext
  liftIO $ isLeft eVersion `shouldBe` True
  let (Left error) = eVersion
  liftIO $
    error
      `shouldBe` NotExistsError
        (_ERROR_DATABASE__ENTITY_NOT_FOUND "questionnaire_version" [("tenant_uuid", U.toString defaultTenant.uuid), ("uuid", U.toString version.uuid)])

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareQuestionnaireVersionCreateDtos resDto expDto = do
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.eventUuid `shouldBe` expDto.eventUuid
