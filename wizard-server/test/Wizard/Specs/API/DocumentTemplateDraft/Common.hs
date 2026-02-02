module Wizard.Specs.API.DocumentTemplateDraft.Common where

import Data.Either (isRight)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDAO
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDataDAO
import Wizard.Model.DocumentTemplate.DocumentTemplateDraftData

import Wizard.Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfDocumentTemplateInDB appContext dt = do
  eDt <- runInContextIO (findDraftByUuid dt.uuid) appContext
  liftIO $ isRight eDt `shouldBe` True
  let (Right dtFromDB) = eDt
  compareDtos dtFromDB dt

assertExistenceOfDraftDataInDB appContext draftData = do
  eDraftData <- runInContextIO (findDraftDataByUuid draftData.documentTemplateUuid) appContext
  liftIO $ isRight eDraftData `shouldBe` True
  let (Right draftDataFromDB) = eDraftData
  liftIO $ draftDataFromDB `shouldBe` draftData

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareDtos resDto expDto = do
  liftIO $ resDto.uuid `shouldBe` expDto.uuid
  liftIO $ resDto.organizationId `shouldBe` expDto.organizationId
  liftIO $ resDto.templateId `shouldBe` expDto.templateId
  liftIO $ resDto.version `shouldBe` expDto.version
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.description `shouldBe` expDto.description
