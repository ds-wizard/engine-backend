module Specs.API.ActionKey.Common where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Api.Resource.Error.ErrorDTO ()
import Database.DAO.ActionKey.ActionKeyDAO

import Specs.API.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfActionKeyInDB appContext actionKey = do
  actionKeyFromDb <- getFirstFromDB findActionKeys appContext
  compareActionKeyDtos actionKeyFromDb actionKey

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareActionKeyDtos resDto expDto = liftIO $ (resDto == expDto) `shouldBe` True
