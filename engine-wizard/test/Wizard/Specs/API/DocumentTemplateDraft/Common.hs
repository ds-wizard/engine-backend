module Wizard.Specs.API.DocumentTemplateDraft.Common where

import Data.Either (isRight)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Model.DocumentTemplate.DocumentTemplate
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDAO
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDataDAO
import Wizard.Model.DocumentTemplate.DocumentTemplateDraftData

import Wizard.Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfTemplateInDB appContext tml = do
  eTemplate <- runInContextIO (findDraftById tml.tId) appContext
  liftIO $ isRight eTemplate `shouldBe` True
  let (Right templateFromDB) = eTemplate
  compareDtos templateFromDB tml

assertExistenceOfDraftDataInDB appContext draftData = do
  eDraftData <- runInContextIO (findDraftDataById draftData.documentTemplateId) appContext
  liftIO $ isRight eDraftData `shouldBe` True
  let (Right draftDataFromDB) = eDraftData
  liftIO $ draftDataFromDB `shouldBe` draftData

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareDtos resDto expDto = do
  liftIO $ resDto.tId `shouldBe` expDto.tId
  liftIO $ resDto.organizationId `shouldBe` expDto.organizationId
  liftIO $ resDto.templateId `shouldBe` expDto.templateId
  liftIO $ resDto.version `shouldBe` expDto.version
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.description `shouldBe` expDto.description
