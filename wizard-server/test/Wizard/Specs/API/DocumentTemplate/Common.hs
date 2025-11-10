module Wizard.Specs.API.DocumentTemplate.Common where

import Data.Either (isRight)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Wizard.Model.Context.ContextLenses ()

import Wizard.Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfTemplateInDB appContext tml = do
  eTemplate <- runInContextIO (findDocumentTemplateById tml.tId) appContext
  liftIO $ isRight eTemplate `shouldBe` True
  let (Right templateFromDB) = eTemplate
  compareTemplateDtos templateFromDB tml

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareTemplateDtos resDto expDto = do
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.description `shouldBe` expDto.description
