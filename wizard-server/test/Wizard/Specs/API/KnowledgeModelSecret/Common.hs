module Wizard.Specs.API.KnowledgeModelSecret.Common where

import Control.Monad.Reader (liftIO)
import Test.Hspec

import Wizard.Database.DAO.KnowledgeModelSecret.KnowledgeModelSecretDAO
import Wizard.Model.KnowledgeModelSecret.KnowledgeModelSecret

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
