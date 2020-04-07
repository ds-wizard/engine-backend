module Wizard.Specs.API.Document.Common where

import Control.Lens ((^.))
import Data.Either (isLeft)
import Data.Maybe (fromJust)
import qualified Data.UUID as U
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import LensesConfig hiding (request)
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Wizard.Database.DAO.Document.DocumentDAO

import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfDocumentInDB appContext reqDto = do
  docFromDb <- getFirstFromDB findDocuments appContext
  liftIO $ (docFromDb ^. name) `shouldBe` (reqDto ^. name)
  liftIO $ (docFromDb ^. questionnaireUuid) `shouldBe` (reqDto ^. questionnaireUuid)
  liftIO $ (docFromDb ^. templateUuid) `shouldBe` (reqDto ^. templateUuid)
  liftIO $ (docFromDb ^. formatUuid) `shouldBe` (reqDto ^. formatUuid)

assertAbsenceOfDocumentInDB appContext doc = do
  let docUuid = U.toString $ doc ^. uuid
  eDoc <- runInContextIO (findDocumentById docUuid) appContext
  liftIO $ isLeft eDoc `shouldBe` True
  let (Left error) = eDoc
  liftIO $ error `shouldBe` (NotExistsError $ _ERROR_DATABASE__ENTITY_NOT_FOUND "document" docUuid)

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareDocumentDtos resDto expDto = do
  liftIO $ (resDto ^. name) `shouldBe` (expDto ^. name)
  liftIO $ (fromJust (resDto ^. questionnaire) ^. uuid) `shouldBe` (expDto ^. questionnaireUuid)
  liftIO $ (resDto ^. template . uuid) `shouldBe` (expDto ^. templateUuid)
  liftIO $ (resDto ^. formatUuid) `shouldBe` (expDto ^. formatUuid)
