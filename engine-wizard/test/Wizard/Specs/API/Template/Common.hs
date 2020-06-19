module Wizard.Specs.API.Template.Common where

import Control.Lens ((^.))
import Data.Either (isRight)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import LensesConfig hiding (request)
import Shared.Api.Resource.Error.ErrorJM ()
import Wizard.Database.DAO.Template.TemplateDAO

import Wizard.Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfTemplateInDB appContext template = do
  eTemplate <- runInContextIO (findTemplateById $ template ^. tId) appContext
  liftIO $ isRight eTemplate `shouldBe` True
  let (Right templateFromDB) = eTemplate
  compareTemplateDtos templateFromDB template

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareTemplateDtos resDto expDto = do
  liftIO $ (resDto ^. name) `shouldBe` (expDto ^. name)
  liftIO $ (resDto ^. description) `shouldBe` (expDto ^. description)
