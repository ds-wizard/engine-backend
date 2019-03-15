module Specs.API.Questionnaire.Common where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Api.Resource.Error.ErrorDTO ()

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareQuestionnaireDtos resDto expDto = liftIO $ (resDto == expDto) `shouldBe` True
