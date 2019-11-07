module Specs.API.Audit.Common where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Api.Resource.Error.ErrorDTO ()
import Database.DAO.Audit.AuditEntryDAO

import Specs.API.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfAuditEntryInDB appContext ae = do
  aeD <- getFirstFromDB findAuditEntries appContext
  liftIO $ ae `shouldBe` aeD
