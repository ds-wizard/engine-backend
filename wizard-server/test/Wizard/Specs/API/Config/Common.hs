module Wizard.Specs.API.Config.Common where

import Data.Either (isRight)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Wizard.Service.Tenant.Config.ConfigService

import Wizard.Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfTenantConfigQuestionnaireInDB appContext tcQuestionnaire = do
  eitherTcQuestionnaire <- runInContextIO getCurrentTenantConfigQuestionnaire appContext
  liftIO $ isRight eitherTcQuestionnaire `shouldBe` True
  let (Right tcQuestionnaireFromDb) = eitherTcQuestionnaire
  compareDtos tcQuestionnaireFromDb tcQuestionnaire

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareDtos resDto expDto = liftIO $ resDto `shouldBe` expDto
