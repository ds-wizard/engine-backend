module Wizard.Specs.API.KnowledgeModelSecret.Common where

import Control.Monad.Reader (liftIO)
import Test.Hspec

import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelSecretDAO
import Wizard.Model.KnowledgeModel.KnowledgeModelSecret

import Wizard.Specs.API.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfKnowledgeModelSecretInDB appContext kmSecret = do
  kmSecretFromDb <- getFirstFromDB findKnowledgeModelSecrets appContext
  compareKnowledgeModelSecretDtos kmSecretFromDb kmSecret

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareKnowledgeModelSecretDtos resDto expDto = do
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.value `shouldBe` expDto.value
