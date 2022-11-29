module Wizard.Specs.API.Questionnaire.Version.Common where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Error.ErrorJM ()

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareQuestionnaireVersionCreateDtos resDto expDto = do
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.eventUuid `shouldBe` expDto.eventUuid
