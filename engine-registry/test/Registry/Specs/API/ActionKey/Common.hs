module Registry.Specs.API.ActionKey.Common where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Registry.Database.DAO.ActionKey.ActionKeySqlDAO

import Registry.Specs.API.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfActionKeyInDB appContext actionKey = do
  actionKeyFromDb <- getFirstFromDB findActionKeys appContext
  compareActionKeyDtos actionKeyFromDb actionKey

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareActionKeyDtos resDto expDto = liftIO $ resDto `shouldBe` expDto
