module Wizard.Specs.API.Document.Common where

import Data.Either (isLeft)
import Data.Maybe (fromJust)
import qualified Data.UUID as U
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Model.Document.Document
import Wizard.Model.Tenant.Tenant

import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfDocumentInDB appContext reqDto = do
  docFromDb <- getFirstFromDB findDocuments appContext
  liftIO $ docFromDb.name `shouldBe` reqDto.name
  liftIO $ docFromDb.questionnaireUuid `shouldBe` reqDto.questionnaireUuid
  liftIO $ docFromDb.documentTemplateId `shouldBe` reqDto.documentTemplateId
  liftIO $ docFromDb.formatUuid `shouldBe` reqDto.formatUuid

assertAbsenceOfDocumentInDB appContext doc = do
  eDoc <- runInContextIO (findDocumentByUuid doc.uuid) appContext
  liftIO $ isLeft eDoc `shouldBe` True
  let (Left error) = eDoc
  liftIO $
    error
      `shouldBe` NotExistsError
        (_ERROR_DATABASE__ENTITY_NOT_FOUND "document" [("tenant_uuid", U.toString defaultTenant.uuid), ("uuid", U.toString doc.uuid)])

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareDocumentDtos resDto expDto = do
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ (fromJust resDto.questionnaire).uuid `shouldBe` expDto.questionnaireUuid
  liftIO $ resDto.documentTemplate.tId `shouldBe` expDto.documentTemplateId
  liftIO $ resDto.formatUuid `shouldBe` expDto.formatUuid
