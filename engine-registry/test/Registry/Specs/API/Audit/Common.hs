module Registry.Specs.API.Audit.Common where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Registry.Database.DAO.Audit.AuditEntryDAO

import Registry.Specs.API.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfAuditEntryInDB appContext ae = do
  aeD <- getFirstFromDB findAuditEntries appContext
  liftIO $ ae `shouldBe` aeD
