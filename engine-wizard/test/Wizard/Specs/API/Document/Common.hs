module Wizard.Specs.API.Document.Common where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import LensesConfig
import Shared.Api.Resource.Error.ErrorJM ()
import Wizard.Database.DAO.Document.DocumentDAO

import Wizard.Specs.API.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfDocumentInDB appContext reqDto = do
  docFromDb <- getFirstFromDB findDocuments appContext
  liftIO $ (docFromDb ^. name) `shouldBe` (reqDto ^. name)
  liftIO $ (docFromDb ^. questionnaireUuid) `shouldBe` (reqDto ^. questionnaireUuid)
  liftIO $ (docFromDb ^. templateUuid) `shouldBe` (reqDto ^. templateUuid)
  liftIO $ (docFromDb ^. format) `shouldBe` (reqDto ^. format)

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareDocumentDtos resDto expDto = do
  liftIO $ (resDto ^. name) `shouldBe` (expDto ^. name)
  liftIO $ (fromJust (resDto ^. questionnaire) ^. uuid) `shouldBe` (expDto ^. questionnaireUuid)
  liftIO $ (resDto ^. template . uuid) `shouldBe` (expDto ^. templateUuid)
  liftIO $ (resDto ^. format) `shouldBe` (expDto ^. format)
